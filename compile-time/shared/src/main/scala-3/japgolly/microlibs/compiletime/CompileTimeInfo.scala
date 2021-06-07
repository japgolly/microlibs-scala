package japgolly.microlibs.compiletime

import scala.quoted.*
import MacroEnv.*

object CompileTimeInfo {

  // Null versions

  transparent inline def envVarOrNull(inline key: String): String =
    ${ quoted.envVarOrNull('key) }

  transparent inline def sysPropOrNull(inline key: String): String =
    ${ quoted.sysPropOrNull('key) }

  transparent inline def envVarOrSysPropOrNull(inline key: String): String =
    ${ quoted.envVarOrSysPropOrNull('key) }

  transparent inline def sysPropOrEnvVarOrNull(inline key: String): String =
    ${ quoted.sysPropOrEnvVarOrNull('key) }

  // Option versions

  transparent inline def envVar(inline key: String): Option[String] =
    ${ quoted.envVar('key) }

  transparent inline def sysProp(inline key: String): Option[String] =
    ${ quoted.sysProp('key) }

  transparent inline def envVarOrSysProp(inline key: String): Option[String] =
    ${ quoted.envVarOrSysProp('key) }

  transparent inline def sysPropOrEnvVar(inline key: String): Option[String] =
    ${ quoted.sysPropOrEnvVar('key) }

  // ===================================================================================================================

  object quoted {

    private def getEnvVar(key: String): Option[String] =
      Option(System.getenv(key))

    private def getSysProp(key: String): Option[String] =
      Option(System.getProperty(key, null))

    // Null versions

    def envVarOrNull(key: Expr[String])(using Quotes): Expr[String] = {
      import quotes.reflect.*
      val k = key.valueOrError
      val v = getEnvVar(k)
      Expr(v.orNull)
    }

    def sysPropOrNull(key: Expr[String])(using Quotes): Expr[String] = {
      import quotes.reflect.*
      val k = key.valueOrError
      val v = getSysProp(k)
      Expr(v.orNull)
    }

    def envVarOrSysPropOrNull(key: Expr[String])(using Quotes): Expr[String] = {
      import quotes.reflect.*
      val k = key.valueOrError
      val v = getEnvVar(k) orElse getSysProp(k)
      Expr(v.orNull)
    }

    def sysPropOrEnvVarOrNull(key: Expr[String])(using Quotes): Expr[String] = {
      import quotes.reflect.*
      val k = key.valueOrError
      val v = getSysProp(k) orElse getEnvVar(k)
      Expr(v.orNull)
    }

    // Option versions

    def envVar(key: Expr[String])(using Quotes): Expr[Option[String]] = {
      import quotes.reflect.*
      val k = key.valueOrError
      val v = getEnvVar(k)
      Expr(v)
    }

    def sysProp(key: Expr[String])(using Quotes): Expr[Option[String]] = {
      import quotes.reflect.*
      val k = key.valueOrError
      val v = getSysProp(k)
      Expr(v)
    }

    def envVarOrSysProp(key: Expr[String])(using Quotes): Expr[Option[String]] = {
      import quotes.reflect.*
      val k = key.valueOrError
      val v = getEnvVar(k) orElse getSysProp(k)
      Expr(v)
    }

    def sysPropOrEnvVar(key: Expr[String])(using Quotes): Expr[Option[String]] = {
      import quotes.reflect.*
      val k = key.valueOrError
      val v = getSysProp(k) orElse getEnvVar(k)
      Expr(v)
    }
  }
}
