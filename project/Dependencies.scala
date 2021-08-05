import sbt._
import sbt.Keys._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {

  object Ver {

    // Exported
    val cats            = "2.6.1"
    val scala2          = "2.13.6"
    val scala3          = "3.0.1"
    val sourceCode      = "0.2.7"
    val univEq          = "1.6.0"

    // Internal
    val jamm            = "0.3.3"
    val kindProjector   = "0.13.0"
    val scalaCheck      = "1.15.4"
    val scalaJsJavaTime = "1.0.0"
    val utest           = "0.7.10"
  }

  object Dep {
    val catsCore        = Def.setting("org.typelevel"                 %%% "cats-core"               % Ver.cats)
    val catsFree        = Def.setting("org.typelevel"                 %%% "cats-free"               % Ver.cats)
    val jamm            = Def.setting("com.github.jbellis"              % "jamm"                    % Ver.jamm)
    val scalaCheck      = Def.setting("org.scalacheck"                %%% "scalacheck"              % Ver.scalaCheck)
    val scalaCompiler   = Def.setting("org.scala-lang"                  % "scala-compiler"          % scalaVersion.value)
    val scalaJsJavaTime = Def.setting("org.scala-js"                  %%% "scalajs-java-time"       % Ver.scalaJsJavaTime cross CrossVersion.for3Use2_13)
    val sourceCode      = Def.setting("com.lihaoyi"                   %%% "sourcecode"              % Ver.sourceCode)
    val univEq          = Def.setting("com.github.japgolly.univeq"    %%% "univeq"                  % Ver.univEq)
    val univEqCats      = Def.setting("com.github.japgolly.univeq"    %%% "univeq-cats"             % Ver.univEq)
    val utest           = Def.setting("com.lihaoyi"                   %%% "utest"                   % Ver.utest)

    // Compiler plugins
    val kindProjector = compilerPlugin("org.typelevel" %% "kind-projector" % Ver.kindProjector cross CrossVersion.full)
  }

}
