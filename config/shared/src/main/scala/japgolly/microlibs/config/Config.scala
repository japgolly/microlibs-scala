package japgolly.microlibs.config

import japgolly.microlibs.stdlib_ext.{ParseInt, ParseLong}
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.util.Properties
import java.util.regex.Pattern
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
}
object ConfigStore {
  private def obj = "ConfigStore"

  def empty[F[_]](implicit F: Applicative[F]): ConfigStore[F] =
    new ConfigStore[F] {
      override def toString = s"$obj.empty"
      override def hashCode = 0
      override def apply(key: Key) = F.pure(ConfigValue.NotFound)
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
        case _ => -\/("Not an Int.")
      }

    implicit def readLong(s: ValueReader[String]): ValueReader[Long] =
      s.mapAttempt {
        case ParseLong(l) => \/-(l)
        case _ => -\/("Not a Long.")
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
          -\/("Not a Boolean.")
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

}

case class SV(source: SourceName, value: ConfigValue)
case class XXX[F[_]](highToLowPri: F[Vector[SV]], selected: F[Option[SV]])
object Config {
  private val IdMonad = implicitly[Monad[Id]]

  final case class R[F[_]](highToLowPri: Vector[(SourceName, ConfigStore[F])])

  final case class W(usedKeys: Set[Key]) {
    def ++(w: W): W = W(usedKeys ++ w.usedKeys)
  }
  object W {
    val empty = W(Set.empty)
  }

  final case class S[F[_]](keyMod: Key => Key, queryCache: Map[Key, XXX[F]])
  object S {
    def init[F[_]]: S[F] = S(identity[Key], Map.empty)
  }

  type Step[F[_], A] = RWS[R[F], W, S[F], F[A]]

  implicit val inst = new Applicative[Config] {
    override def point[A](a: => A) = new Config[A] {
      override def step[F[_]](implicit F: Applicative[F]) =
        RWS((r, s) => (W.empty, F.point(Result.Success(a)), s))
    }
    override def map[A, B](fa: Config[A])(f: A => B) = fa map f
    override def ap[A, B](fa: => Config[A])(ff: => Config[A => B]) = new Config[B] {
      override def step[F[_]](implicit F: Applicative[F]) = {
        val ga = fa.step[F].getF[S[F], R[F]](IdMonad)
        val gf = ff.step[F].getF[S[F], R[F]](IdMonad)
        RWS { (r, s0) =>
          val (w1, ff, s1) = gf(r, s0)
          val (w2, fa, s2) = ga(r, s1)
          (w1 ++ w2, F.compose(Result.inst).ap(fa)(ff), s2)
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

          (W(Set.empty + k), result, s2)
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
      override def step[F[_]](implicit F: Applicative[F]): Step[F, Result[KeyReport]] =
        RWS { (r, s) =>

          val omg: F[Vector[(Key, Map[SourceName, ConfigValue])]] =
            s.queryCache.toVector.traverse { case (k, x) =>
              val qwe: F[(Key, Map[SourceName, ConfigValue])] = x.highToLowPri.map(x =>
                k -> x.toIterator.map(sv => sv.source -> sv.value).toMap)
              qwe
            }
          val omg2: F[Map[Key, Map[SourceName, ConfigValue]]] =
            omg.map(_.foldLeft[Map[Key, Map[SourceName, ConfigValue]]](Map.empty) {
              case (q, (k, vs)) => q.modifyValue(k, _.fold(vs)(_ ++ vs))
            })

          val result: F[Result[KeyReport]] =
            omg2.map(used =>
              Result.Success(
                KeyReport(r.highToLowPri.map(_._1), used)))

          (W.empty, result, s)
        }
    }
}

final case class KeyReport(sourcesHighToLowPri: Vector[SourceName],
                           used: Map[Key, Map[SourceName, ConfigValue]]) {
  def report: String = {
    val sb = new StringBuilder
    sb append sourcesHighToLowPri.toIterator.map(_.value).mkString("Key | ", " | ", "")
    for (k <- used.keys.toList.sortBy(_.value)) {
      val vs = sourcesHighToLowPri.toIterator.map(used(k).apply).map {
        case ConfigValue.Found(v) => v
        case ConfigValue.NotFound => ""
        case ConfigValue.Error(err, None) => s"¡$err!"
        case ConfigValue.Error(err, Some(v)) => s"$v ¡$err!"
      }
      sb append s"\n${k.value} | ${vs.mkString(" | ")}"
    }
    sb.toString
  }

}
// filter in/out {un,}used
// password


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

sealed abstract class ResultX[+A]

object ResultX {
  final case class PreparationFailure(sourceName: SourceName, error: String) extends ResultX[Nothing]
  final case class QueryFailure(failures: Map[Key, Option[(SourceName, ConfigValue.Error)]]) extends ResultX[Nothing]
  final case class Success[+A](value: A) extends ResultX[A]
}


object PAD {

//  sealed abstract class Result[+A]
//  object Result {
////    final case class Failure(attempts: NonEmpty[Map[Key, NonEmptyVector[Source]]]) extends Result[Nothing]
//    final case class FailureRead(key: Key, sources: Vector[SourceName]) extends Result[Nothing]
//    final case class Value[+A](key: Key, value: A, source: SourceName, overridden: Map[SourceName, A]) extends Result[A]
//  }

  // ===================================================================================================================

//  import scalaz.syntax.apply._
//  import ValueReader.Y._
//  import ValueReader.N._
//
//  val fa = Config.get[Boolean]("blah")
//  val fb = Config.get[Int]("blah", 3)
//  val fc = Config.need[String]("blah")
//  val fx: Config[Blah] = (fa |@| fb |@| fc) (Blah.apply)
//  val blah: Blah =
//    fx(SOURCES).unsafePerformIO() match {
//      case Result.Success(x) => x
//      case Result.Failure(errors) => sys error errors.toString
//    }
//
//  def SOURCES: Sources[IO] =
//    Source.environment[IO] > Source.propFileOnClasspath[IO]("blah.props") > Source.system[IO]
//
//  final case class Blah(ob: Option[Boolean], i: Int, s: String)

  // TODO Prefixes/KeyMod
  // .prefix = AddPrefix *> run <* RemovePrefix

}
