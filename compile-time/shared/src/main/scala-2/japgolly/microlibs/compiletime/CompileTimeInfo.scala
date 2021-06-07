package japgolly.microlibs.compiletime

import scala.reflect.macros.blackbox.Context

object CompileTimeInfo {

  // Null versions

  def envVarOrNull(key: String): String =
    macro CompileTimeInfoMacros.envVarOrNull

  def sysPropOrNull(key: String): String =
    macro CompileTimeInfoMacros.sysPropOrNull

  def envVarOrSysPropOrNull(key: String): String =
    macro CompileTimeInfoMacros.envVarOrSysPropOrNull

  def sysPropOrEnvVarOrNull(key: String): String =
    macro CompileTimeInfoMacros.sysPropOrEnvVarOrNull

  // Option versions

  def envVar(key: String): Option[String] =
    macro CompileTimeInfoMacros.envVar

  def sysProp(key: String): Option[String] =
    macro CompileTimeInfoMacros.sysProp

  def envVarOrSysProp(key: String): Option[String] =
    macro CompileTimeInfoMacros.envVarOrSysProp

  def sysPropOrEnvVar(key: String): Option[String] =
    macro CompileTimeInfoMacros.sysPropOrEnvVar
}

// =====================================================================================================================

final class CompileTimeInfoMacros(val c: Context) extends MacroUtils {
  import c.universe._

  private def _envVar(key: String): Option[String] =
    Option(System.getenv(key))

  private def _sysProp(key: String): Option[String] =
    Option(System.getProperty(key, null))

  // Null versions

  private def lit(o: Option[String]): c.Expr[String] =
    c.Expr[String](Literal(Constant(o.orNull)))

  def envVarOrNull(key: c.Expr[String]): c.Expr[String] = {
    val k = readMacroArg_string(key)
    val v = _envVar(k)
    lit(v)
  }

  def sysPropOrNull(key: c.Expr[String]): c.Expr[String] = {
    val k = readMacroArg_string(key)
    val v = _sysProp(k)
    lit(v)
  }

  def envVarOrSysPropOrNull(key: c.Expr[String]): c.Expr[String] = {
    val k = readMacroArg_string(key)
    val v = _envVar(k) orElse _sysProp(k)
    lit(v)
  }

  def sysPropOrEnvVarOrNull(key: c.Expr[String]): c.Expr[String] = {
    val k = readMacroArg_string(key)
    val v = _sysProp(k) orElse _envVar(k)
    lit(v)
  }

  // Option versions

  private def opt(o: Option[String]): c.Expr[Option[String]] =
    c.Expr[Option[String]](
      o match {
        case Some(s) => q"_root_.scala.Some(${Literal(Constant(s))})"
        case None    => q"_root_.scala.Option.empty[String]"
      }
    )

  def envVar(key: c.Expr[String]): c.Expr[Option[String]] = {
    val k = readMacroArg_string(key)
    val v = _envVar(k)
    opt(v)
  }

  def sysProp(key: c.Expr[String]): c.Expr[Option[String]] = {
    val k = readMacroArg_string(key)
    val v = _sysProp(k)
    opt(v)
  }

  def envVarOrSysProp(key: c.Expr[String]): c.Expr[Option[String]] = {
    val k = readMacroArg_string(key)
    val v = _envVar(k) orElse _sysProp(k)
    opt(v)
  }

  def sysPropOrEnvVar(key: c.Expr[String]): c.Expr[Option[String]] = {
    val k = readMacroArg_string(key)
    val v = _sysProp(k) orElse _envVar(k)
    opt(v)
  }

}
