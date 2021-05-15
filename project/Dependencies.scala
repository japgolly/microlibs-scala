import sbt._
import sbt.Keys._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {

  object Ver {

    // Exported
    val Scala212        = "2.12.13"
    val Scala213        = "2.13.5"
    val Scala3          = "3.0.0-RC3"
    val ScalaCollCompat = "2.4.3"
    val Scalaz          = "7.2.31"
    val SourceCode      = "0.2.6"
    val UnivEq          = "1.4.0-RC4"

    // Internal
    val JAMM            = "0.3.3"
    val KindProjector   = "0.13.0"
    val MTest           = "0.7.9"
    val Nyaya           = "0.10.0-RC2"
    val ScalaJsJavaTime = "1.0.0"
  }

  object Dep {
    val JAMM            = Def.setting("com.github.jbellis"              % "jamm"                    % Ver.JAMM)
    val MTest           = Def.setting("com.lihaoyi"                   %%% "utest"                   % Ver.MTest)
    val NyayaGen        = Def.setting("com.github.japgolly.nyaya"     %%% "nyaya-gen"               % Ver.Nyaya)
    val NyayaProp       = Def.setting("com.github.japgolly.nyaya"     %%% "nyaya-prop"              % Ver.Nyaya)
    val NyayaTest       = Def.setting("com.github.japgolly.nyaya"     %%% "nyaya-test"              % Ver.Nyaya)
    val ScalaCollCompat = Def.setting("org.scala-lang.modules"        %%% "scala-collection-compat" % Ver.ScalaCollCompat)
    val ScalaCompiler   = Def.setting("org.scala-lang"                  % "scala-compiler"          % scalaVersion.value)
    val ScalaJsJavaTime = Def.setting("org.scala-js"                  %%% "scalajs-java-time"       % Ver.ScalaJsJavaTime cross CrossVersion.for3Use2_13)
    val Scalaz          = Def.setting("org.scalaz"                    %%% "scalaz-core"             % Ver.Scalaz cross CrossVersion.for3Use2_13)
    val SourceCode      = Def.setting("com.lihaoyi"                   %%% "sourcecode"              % Ver.SourceCode)
    val UnivEq          = Def.setting("com.github.japgolly.univeq"    %%% "univeq"                  % Ver.UnivEq)
    val UnivEqScalaz    = Def.setting("com.github.japgolly.univeq"    %%% "univeq-scalaz"           % Ver.UnivEq)

    // Compiler plugins
    val KindProjector = compilerPlugin("org.typelevel" %% "kind-projector" % Ver.KindProjector cross CrossVersion.full)
  }

}
