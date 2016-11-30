package japgolly.microlibs.config

import japgolly.microlibs.stdlib_ext._, StdlibExt._
import java.util.Properties
import java.util.regex.Pattern
import scala.collection.JavaConverters._
/*
import scalaz.{-\/, \/, \/-}
import scalaz.{Applicative, Apply, Bind, Monad, RWS}
import scalaz.Scalaz.Id
import scalaz.syntax.applicative._
import scalaz.syntax.traverse
import scalaz.std.vector.vectorInstance
*/
import scalaz._, Scalaz._

case class Key(value: String) extends AnyVal

sealed trait ConfigValue extends Product with Serializable
object ConfigValue {
  final case class Found(value: String) extends ConfigValue
  case object NotFound extends ConfigValue
  final case class Error(desc: String, value: Option[String]) extends ConfigValue

  def option(o: Option[String]): ConfigValue =
    o match {
      case Some(v) => Found(v)
      case None => NotFound
    }
}

trait ConfigStore[F[_]] {
  def apply(key: Key): F[ConfigValue]
  def bulk(filter: Key => Boolean): F[Map[Key, String]]
}
object ConfigStore {
  private def obj = "ConfigStore"

  def empty[F[_]](implicit F: Applicative[F]): ConfigStore[F] =
    new ConfigStore[F] {
      override def toString = s"$obj.empty"
      override def hashCode = 0
      override def apply(key: Key) = F.pure(ConfigValue.NotFound)
      override def bulk(f: Key => Boolean) = F.pure(Map.empty)
    }

  def javaProps[F[_]](p: Properties)(implicit F: Applicative[F]): ConfigStore[F] =
    new ConfigStore[F] {
      override def toString = s"$obj.javaProps($p)"
      override def hashCode = p.##
      override def apply(key: Key) = {
        val o = Option(p.getProperty(key.value))
        val r = ConfigValue.option(o)
        F.pure(r)
      }
      override def bulk(f: Key => Boolean) = F.pure(
        p.keys()
          .asScala
          .map(k => Key(k.toString))
          .filter(f)
          .map(k => k -> p.getProperty(k.value))
          .toMap)
    }

  def stringMap[F[_]](m: Map[String, String])(implicit F: Applicative[F]): ConfigStore[F] =
    new ConfigStore[F] {
      override def toString = s"$obj.stringMap($m)"
      override def hashCode = m.##
      override def apply(key: Key) = {
        val o = m.get(key.value)
        val r = ConfigValue.option(o)
        F.pure(r)
      }
      override def bulk(f: Key => Boolean) = F.pure(m.toIterator
        .map { case (k, v) => (Key(k), v) }
        .filter(x => f(x._1))
        .toMap)
    }
}

final case class Sources[F[_]](highToLowPri: Vector[Source[F]]) extends AnyVal {
  def >(lowerPri: Sources[F]): Sources[F] =
    Sources(highToLowPri ++ lowerPri.highToLowPri)

  def <(higherPri: Sources[F]): Sources[F] =
    higherPri > this
}

object Sources {
  def empty[F[_]]: Sources[F] =
    apply(Vector.empty)

}

final case class SourceName(value: String) extends AnyVal
final case class Source[F[_]](name: SourceName, prepare: F[String \/ ConfigStore[F]])

object Source {
  implicit def toSources[F[_]](s: Source[F]): Sources[F] =
    Sources(Vector.empty :+ s)

  def point[F[_]](name: String, config: => ConfigStore[F])(implicit F: Applicative[F]): Source[F] =
    Source[F](SourceName(name), F.point(\/-(config)))

  def manual[F[_]](name: String)(kvs: (String, String)*)(implicit F: Applicative[F]): Source[F] =
    point(name, ConfigStore.stringMap(kvs.toMap))

  def environment[F[_]](implicit F: Applicative[F]): Source[F] =
    point("Environment", ConfigStore.stringMap(sys.env))

  def system[F[_]](implicit F: Applicative[F]): Source[F] =
    Source[F](SourceName("System"), F.point {
      def cfg() = ConfigStore.javaProps[F](System.getProperties())
      \/.fromTryCatchNonFatal(cfg()).leftMap(_.getMessage)
    })

  def propFileOnClasspath[F[_]](filename: String, optional: Boolean)(implicit F: Applicative[F]): Source[F] = {
    val f = filename.replaceFirst("^/*", "/")
    Source[F](SourceName(s"classpath:$f"), F.point {
      def load() = {
        val i = getClass.getResourceAsStream(f)
        if (i eq null) {
          if (optional)
            \/-(ConfigStore.empty[F])
          else
            -\/("File not found.")
        } else {
          val p = new Properties()
          p.load(i)
          \/-(ConfigStore.javaProps[F](p))
        }
      }
      \/.fromTryCatchNonFatal(load()).leftMap(_.getMessage).flatMap(identity)
    })
  }
}

// ===================================================================================================================

final case class ValueReader[A](read: ConfigValue.Found => String \/ A) extends AnyVal {
  def map[B](f: A => B): ValueReader[B] =
    ValueReader(read(_) map f)

  def mapAttempt[B](f: A => String \/ B): ValueReader[B] =
    ValueReader(read(_) flatMap f)

  def flatMap[B](f: A => ValueReader[B]): ValueReader[B] =
    ValueReader(v => read(v).flatMap(f(_) read v))
}

object ValueReader {
  object X {
    implicit val readString: ValueReader[String] =
      ValueReader(v => \/-(v.value))
  }
  object Y {
    implicit val readString: ValueReader[String] =
      X.readString.map(_.trim.replaceFirst("\\s*#.*$", ""))
  }
  object N {
    implicit def readInt(implicit s: ValueReader[String]): ValueReader[Int] =
      s.mapAttempt {
        case ParseInt(i) => \/-(i)
        case _ => -\/("Int expected.")
      }

    implicit def readLong(s: ValueReader[String]): ValueReader[Long] =
      s.mapAttempt {
        case ParseLong(l) => \/-(l)
        case _ => -\/("Long expected.")
      }

    private val RegexTrue = Pattern.compile("^(?:t(?:rue)?|y(?:es)?|1|on|enabled?)$", Pattern.CASE_INSENSITIVE)
    private val RegexFalse = Pattern.compile("^(?:f(?:alse)?|n(?:o)?|0|off|disabled?)$", Pattern.CASE_INSENSITIVE)

    implicit def readBoolean(s: ValueReader[String]): ValueReader[Boolean] =
      s.mapAttempt(s =>
        if (RegexTrue.matcher(s).matches)
          \/-(true)
        else if (RegexFalse.matcher(s).matches)
          \/-(false)
        else
          -\/("Boolean expected.")
      )
  }
}

trait Config[A] {
  import Config.{R, S}

  final def run[F[_]](sources: Sources[F])(implicit F: Monad[F]): F[ResultX[A]] = {

    type OK = (SourceName, ConfigStore[F])
    type KO = (SourceName, String)

    val r1: Vector[F[KO \/ OK]] = sources.highToLowPri.map(s => F.map(s.prepare)(_.bimap(s.name -> _, s.name -> _)))
    val r2: F[Vector[KO \/ OK]] = r1.sequenceU
    val r3: F[KO \/ Vector[OK]] = F.map(r2)(_.sequenceU)

    F.bind(r3) {
      case \/-(stores) => step(F).run(R(stores), S.init)._2.map {
        case Result.Success(a) => ResultX.Success(a)
        case Result.Failure(m) => ResultX.QueryFailure(m)
      }
      case -\/((s, err)) => F.pure(ResultX.PreparationFailure(s, err))
    }
  }

  final def withKeyReport: Config[(A, KeyReport)] =
    Config.inst.tuple2(this, Config.keyReport)

  final def map[B](f: A => B): Config[B] =
    mapAttempt(a => Result.Success(f(a)))

  final def mapAttempt[B](f: A => Result[B]): Config[B] = {
    val self = this
    new Config[B] {
      override def step[F[_]](implicit F: Applicative[F]) =
        self.step(F).map(F.map(_)(_ flatMap f))
    }
  }

  def step[F[_]](implicit F: Applicative[F]): Config.Step[F, Result[A]]

  def withKeyMod(f: String => String): Config[A] =
    Config.keyModCompose(f) *> this <* Config.keyModPop

  def withCaseInsensitiveKeys: Config[A] =
    withKeyMod(_.toLowerCase)

  def withPrefix(prefix: String): Config[A] =
    withKeyMod(prefix + _)
}

case class SV(source: SourceName, value: ConfigValue)
case class XXX[F[_]](highToLowPri: F[Vector[SV]], selected: F[Option[SV]])

object Config {
  private val IdMonad = implicitly[Monad[Id]]

  final case class R[F[_]](highToLowPri: Vector[(SourceName, ConfigStore[F])])

  final case class S[F[_]](keyModStack: List[Key => Key], queryCache: Map[Key, XXX[F]]) {
    def keyMod: Key => Key =
      keyModStack.headOption.getOrElse(identity[Key])
    def keyModPush(f: Key => Key): S[F] =
      copy(f :: keyModStack)
    def keyModPop: S[F] =
      keyModStack match {
        case Nil => this
        case _ :: t => copy(t)
      }
  }
  object S {
    def init[F[_]]: S[F] = S(Nil, Map.empty)
  }

  type Step[F[_], A] = RWS[R[F], Unit, S[F], F[A]]

  implicit val inst = new Applicative[Config] {
    override def point[A](a: => A) = new Config[A] {
      override def step[F[_]](implicit F: Applicative[F]) =
        RWS((r, s) => ((), F.point(Result.Success(a)), s))
    }
    override def map[A, B](fa: Config[A])(f: A => B) = fa map f
    override def ap[A, B](fa: => Config[A])(ff: => Config[A => B]) = new Config[B] {
      override def step[F[_]](implicit F: Applicative[F]) = {
        val ga = fa.step[F].getF[S[F], R[F]](IdMonad)
        val gf = ff.step[F].getF[S[F], R[F]](IdMonad)
        RWS { (r, s0) =>
          val (_, ff, s1) = gf(r, s0)
          val (_, fa, s2) = ga(r, s1)
          ((), F.compose(Result.inst).ap(fa)(ff), s2)
        }
      }
    }
  }

  def get[A](key: String)(implicit v: ValueReader[A]): Config[Option[A]] =
    new Config[Option[A]] {
      override def step[F[_]](implicit F: Applicative[F]): Step[F, Result[Option[A]]] =
        RWS { (r, s1) =>
          val k = s1.keyMod(Key(key))

          val (xxx: XXX[F], s2) =
          s1.queryCache.get(k) match {
            case Some(q) => (q, s1)

            case None =>
              val results: F[Vector[SV]] =
                r.highToLowPri.toIterator
                  .map { case (name, store) => store(k).map(SV(name, _)) }
                  .toVector
                  .sequenceU

              val selected: F[Option[SV]] =
                results.map(_.foldLeft[Option[SV]](None) {
                  case (None, nameAndValue) => Some(nameAndValue)
                  case (found@Some(SV(_, _: ConfigValue.Found)), _) => found
                  case (error@Some(SV(_, _: ConfigValue.Error)), _) => error
                  case (Some(SV(_, ConfigValue.NotFound)), next) => Some(next)
                })

              val q = XXX(results, selected)
              (q, s1.copy(queryCache = s1.queryCache.updated(k, q)))
          }


          val result: F[Result[Option[A]]] =
            xxx.selected.map {
              case None => Result.Success(None)
              case Some(SV(name, found: ConfigValue.Found)) =>
                v.read(found) match {
                  case \/-(a) => Result.Success(Some(a))
                  case -\/(e) => Result.Failure(Map(k -> Some((name, ConfigValue.Error(e, Some(found.value))))))
                }
              case Some(SV(_, ConfigValue.NotFound)) => Result.Success(None)
              case Some(SV(n, e: ConfigValue.Error)) => Result.Failure(Map(k -> Some((n, e))))
            }

          ((), result, s2)
        }
    }

  def get[A: ValueReader](key: String, default: => A): Config[A] =
    get[A](key).map(_ getOrElse default)

  def need[A: ValueReader](key: String): Config[A] =
    get[A](key).mapAttempt {
      case Some(a) => Result.Success(a)
      case None => Result.Failure(Map(Key(key) -> None))
    }

  def keyReport: Config[KeyReport] =
    new Config[KeyReport] {
      override def step[F[_]](implicit F: Applicative[F]) =
        RWS { (r, s) =>

          implicit def semigroupConfigValue: Semigroup[ConfigValue] = Semigroup.firstSemigroup
          type M = Map[Key, Map[SourceName, ConfigValue]]
          def emptyM: M = Map.empty

          val fUsed: F[M] =
            s.queryCache
              .toVector
              .traverse { case (k, x) => x.highToLowPri.map(x => k -> x.toIterator.map(sv => sv.source -> sv.value).toMap) }
              .map(_.foldLeft(emptyM) { case (m, (k, vs)) => m.modifyValue(k, _.fold(vs)(_ ++ vs)) })

          val usedKeys = s.queryCache.keySet

          val fUnused: F[M] =
            r.highToLowPri.traverse { case (src, store) =>
              store.bulk(!usedKeys.contains(_))
                .map(_.mapValuesNow(value => Map(src -> ConfigValue.Found(value))))
            }.map(_.foldLeft(emptyM)(_ |+| _))


          val result: F[Result[KeyReport]] =
            F.apply2(fUsed, fUnused)((used, unused) =>
              Result.Success(
                KeyReport(r.highToLowPri.map(_._1), used, unused)))

          ((), result, s)
        }
    }

  private def keyModTS(f: Key => Key): String => String = s => f(Key(s)).value
  private def keyModFS(f: String => String): Key => Key = k => Key(f(k.value))

  private[config] def keyModUpdate(f: (String => String) => String => String): Config[Unit] =
    new Config[Unit] {
      override def step[F[_]](implicit F: Applicative[F]) =
        RWS((_, s) => ((), F pure Result.Success(()), s.keyModPush(keyModFS(f(keyModTS(s.keyMod))))))
    }

  private[config] def keyModCompose(f: String => String): Config[Unit] =
    keyModUpdate(_ compose f)

  private[config] def keyModPop: Config[Unit] =
    new Config[Unit] {
      override def step[F[_]](implicit F: Applicative[F]) =
        RWS((_, s) => ((), F point Result.Success(()), s.keyModPop))
    }
}

final case class KeyReport(sourcesHighToLowPri: Vector[SourceName],
                           used: Map[Key, Map[SourceName, ConfigValue]],
                           unused: Map[Key, Map[SourceName, ConfigValue]]) {

  def ignoreUnusedKeys(f: String => Boolean): KeyReport =
    copy(unused = unused.filterKeys(k => !f(k.value)))

  private def table(map: Map[Key, Map[SourceName, ConfigValue]]): String = {
    val header: Vector[String] =
      "Key" +: sourcesHighToLowPri.map(_.value)

    val valueRows: List[Vector[String]] =
      map.keys.toList.sortBy(_.value).map(k =>
        k.value +: sourcesHighToLowPri.map(s => map.get(k).flatMap(_ get s) getOrElse ConfigValue.NotFound).map {
          case ConfigValue.Found(v) => v.replace("\n", "\\n")
          case ConfigValue.NotFound => ""
          case ConfigValue.Error(err, None) => s"¡$err!"
          case ConfigValue.Error(err, Some(v)) => s"$v ¡$err!"
        }
      )
    AsciiTable(header :: valueRows)
  }

  def reportUsed: String = table(used)
  def reportUnused: String = table(unused)

  def report: String =
    s"""
       !Used keys (${used.size}):
       !$reportUsed
       !
       !Unused keys (${unused.size}):
       !$reportUnused
     """.stripMargin('!')

  // TODO filter in/out {un,}used
  // TODO password
}


sealed abstract class Result[+A] {
  def map[B](f: A => B): Result[B]
  def flatMap[B](f: A => Result[B]): Result[B]
}

object Result {
  final case class Failure(failures: Map[Key, Option[(SourceName, ConfigValue.Error)]]) extends Result[Nothing] {
    override def map[B](f: Nothing => B): Result[B] = this
     override def flatMap[B](f: Nothing => Result[B]): Result[B] = this
  }
  final case class Success[+A](value: A) extends Result[A] {
    override def map[B](f: A => B): Result[B] = Success(f(value))
     override def flatMap[B](f: A => Result[B]): Result[B] = f(value)
  }

  implicit val inst: Applicative[Result] = new Applicative[Result] {
    override def point[A](a: => A) = Success(a)
    override def map[A, B](fa: Result[A])(f: A => B) = fa map f
    override def ap[A, B](fa: => Result[A])(ff: => Result[A => B]) =
      (fa, ff) match {
        case (Success(a), Success(f)) => Success(f(a))
        case (f: Failure, Success(_)) => f
        case (Success(_), f: Failure) => f
        case (Failure(x), Failure(y)) => Failure(x ++ y)
      }
  }
}

sealed abstract class ResultX[+A] {
  def toDisjunction: String \/ A
}

object ResultX {
  final case class PreparationFailure(sourceName: SourceName, error: String) extends ResultX[Nothing] {
    override def toDisjunction = -\/(s"Error preparing source [$sourceName]: $error")
  }

  final case class QueryFailure(failures: Map[Key, Option[(SourceName, ConfigValue.Error)]]) extends ResultX[Nothing] {
    override def toDisjunction = -\/ {
      val each = failures.toVector.sortBy(_._1.value.toLowerCase).map {
        case (Key(k), None) =>
          s"No value for key [$k]"
        case (Key(k), Some((SourceName(src), ConfigValue.Error(desc, None)))) =>
          s"Error reading key [$k] from source [$src]: $desc"
        case (Key(k), Some((SourceName(src), ConfigValue.Error(desc, Some(v))))) =>
          s"Error reading key [$k] from source [$src] with value [$v]: $desc"
      }
      var errors = "error"
      if (each.length != 1) errors += "s"
      s"${each.length} $errors:${each.map("\n  - " + _).mkString}"
    }
  }

  final case class Success[+A](value: A) extends ResultX[A] {
    override def toDisjunction = \/-(value)
  }
}
