package japgolly.microlibs.compiletime

import scala.quoted.*
import MacroEnv.*

object CompileTimeInfo {

  transparent inline def envVar(inline name: String): Option[String] =
    ${ quoted.envVar('name) }

  transparent inline def sysProp(inline name: String): Option[String] =
    ${ quoted.sysProp('name) }

  transparent inline def envVarOrSysProp(inline name: String): Option[String] =
    ${ quoted.envVarOrSysProp('name) }

  transparent inline def sysPropOrEnvVar(inline name: String): Option[String] =
    ${ quoted.sysPropOrEnvVar('name) }

  // ===================================================================================================================

  object quoted {

    private def getEnvVar(key: String): Option[String] =
      Option(System.getenv(key))

    private def getSysProp(key: String): Option[String] =
      Option(System.getProperty(key, null))

    def envVar(name: Expr[String])(using Quotes): Expr[Option[String]] = {
      import quotes.reflect.*
      val key = name.valueOrError
      val value = getEnvVar(key)
      Expr(value)
    }

    def sysProp(name: Expr[String])(using Quotes): Expr[Option[String]] = {
      import quotes.reflect.*
      val key = name.valueOrError
      val value = getSysProp(key)
      Expr(value)
    }

    def envVarOrSysProp(name: Expr[String])(using Quotes): Expr[Option[String]] = {
      import quotes.reflect.*
      val key = name.valueOrError
      val value = getEnvVar(key) orElse getSysProp(key)
      Expr(value)
    }

    def sysPropOrEnvVar(name: Expr[String])(using Quotes): Expr[Option[String]] = {
      import quotes.reflect.*
      val key = name.valueOrError
      val value = getSysProp(key) orElse getEnvVar(key)
      Expr(value)
    }
  }
}
