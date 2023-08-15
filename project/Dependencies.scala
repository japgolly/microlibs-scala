import sbt._
import sbt.Keys._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {

  object Ver {

    // Exported
    def cats            = "2.10.0"
    def scala2          = "2.13.9"
    def scala3          = "3.2.0"
    def sourceCode      = "0.3.0"
    def univEq          = "2.0.1"

    // Internal
    def jamm            = "0.3.3"
    def kindProjector   = "0.13.2"
    def nyaya           = "1.1.0"
    def scalaCheck      = "1.17.0"
    def scalaJsJavaTime = "2.4.0"
    def utest           = "0.8.1"
  }

  object Dep {
    val catsCore        = Def.setting("org.typelevel"                 %%% "cats-core"               % Ver.cats)
    val catsFree        = Def.setting("org.typelevel"                 %%% "cats-free"               % Ver.cats)
    val jamm            = Def.setting("com.github.jbellis"              % "jamm"                    % Ver.jamm)
    val nyayaGen        = Def.setting("com.github.japgolly.nyaya"     %%% "nyaya-gen"               % Ver.nyaya)
    val nyayaProp       = Def.setting("com.github.japgolly.nyaya"     %%% "nyaya-prop"              % Ver.nyaya)
    val nyayaTest       = Def.setting("com.github.japgolly.nyaya"     %%% "nyaya-test"              % Ver.nyaya)
    val scalaCheck      = Def.setting("org.scalacheck"                %%% "scalacheck"              % Ver.scalaCheck)
    val scalaCompiler   = Def.setting("org.scala-lang"                  % "scala-compiler"          % scalaVersion.value)
    val scalaJsJavaTime = Def.setting("io.github.cquiroz"             %%% "scala-java-time"         % Ver.scalaJsJavaTime)
    val sourceCode      = Def.setting("com.lihaoyi"                   %%% "sourcecode"              % Ver.sourceCode)
    val univEq          = Def.setting("com.github.japgolly.univeq"    %%% "univeq"                  % Ver.univEq)
    val univEqCats      = Def.setting("com.github.japgolly.univeq"    %%% "univeq-cats"             % Ver.univEq)
    val utest           = Def.setting("com.lihaoyi"                   %%% "utest"                   % Ver.utest)

    // Compiler plugins
    val kindProjector = compilerPlugin("org.typelevel" %% "kind-projector" % Ver.kindProjector cross CrossVersion.full)
  }

}
