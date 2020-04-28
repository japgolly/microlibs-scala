import sbt._
import Keys._
import com.typesafe.sbt.pgp.PgpKeys
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import pl.project13.scala.sbt.JmhPlugin
import sbtcrossproject.CrossPlugin.autoImport._
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import sbtrelease.ReleasePlugin.autoImport._
import Lib._

object Microlibs {

  private val ghProject = "microlibs-scala"

  private val publicationSettings =
    Lib.publicationSettings(ghProject)

  object Ver {
    val JAMM            = "0.3.3"
    val JavaTimeScalaJs = "0.2.6"
    val KindProjector   = "0.11.0"
    val MacroParadise   = "2.1.1"
    val MTest           = "0.7.4"
    val Nyaya           = "0.9.2"
    val Scala212        = "2.12.11"
    val Scala213        = "2.13.1"
    val ScalaCollCompat = "2.1.6"
    val Scalaz          = "7.2.30"
    val SourceCode      = "0.2.1"
    val UnivEq          = "1.2.1"
  }

  def scalacFlags = Def.setting(
    Seq(
      "-deprecation",
      "-unchecked",
      "-feature",
      "-language:postfixOps",
      "-language:implicitConversions",
      "-language:higherKinds",
      "-language:existentials",
      "-opt:l:inline",
      "-opt-inline-from:japgolly.microlibs.**",
      "-opt-inline-from:japgolly.univeq.**",
      "-Ywarn-dead-code",
      "-Ywarn-unused",
      "-Ywarn-value-discard"))

  val commonSettings = ConfigureBoth(
    _.settings(
      organization                  := "com.github.japgolly.microlibs",
      homepage                      := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                      += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      scalaVersion                  := Ver.Scala213,
      crossScalaVersions            := Seq(Ver.Scala212, Ver.Scala213),
      scalacOptions                ++= scalacFlags.value,
      scalacOptions in Test        --= Seq("-Ywarn-dead-code", "-Ywarn-unused"),
      testFrameworks                := Nil,
      shellPrompt in ThisBuild      := ((s: State) => Project.extract(s).currentRef.project + "> "),
      updateOptions                 := updateOptions.value.withCachedResolution(true),
      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
      releaseTagComment             := s"v${(version in ThisBuild).value}",
      releaseVcsSign                := true,
      addCompilerPlugin("org.typelevel" %% "kind-projector" % Ver.KindProjector cross CrossVersion.full)))

  def definesMacros = ConfigureBoth(
    _.settings(
      scalacOptions += "-language:experimental.macros",
      libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"))

  def macroParadisePlugin =
    compilerPlugin("org.scalamacros" % "paradise" % Ver.MacroParadise cross CrossVersion.full)

  def utestSettings = ConfigureBoth(
    _.settings(
      libraryDependencies += "com.lihaoyi" %%% "utest" % Ver.MTest % Test,
      testFrameworks      += new TestFramework("utest.runner.Framework")))

  def useTestUtil: CPE =
    _.dependsOn(testUtil % "test->compile")

  // ===================================================================================================================

  lazy val root =
    Project("root", file("."))
      .settings(name := "Microlibs")
      .configure(commonSettings.jvm, preventPublication)
      .aggregate(rootJVM, rootJS, bench)

  lazy val rootJVM =
    Project("JVM", file(".rootJVM"))
      .configure(commonSettings.jvm, preventPublication)
      .aggregate(
        adtMacrosJVM, macroUtilsJVM, nameFnJVM, nonemptyJVM, recursionJVM, scalazExtJVM, stdlibExtJVM, testUtilJVM, utilsJVM)

  lazy val rootJS =
    Project("JS", file(".rootJS"))
      .configure(commonSettings.jvm, preventPublication)
      .aggregate(
        adtMacrosJS, macroUtilsJS, nameFnJS, nonemptyJS, recursionJS, scalazExtJS, stdlibExtJS, testUtilJS, utilsJS)

  // ===================================================================================================================

  lazy val adtMacrosJVM = adtMacros.jvm
  lazy val adtMacrosJS  = adtMacros.js
  lazy val adtMacros = crossProject(JVMPlatform, JSPlatform)
    .in(file("adt-macros"))
    .configureCross(commonSettings, publicationSettings, definesMacros, utestSettings)
    .dependsOn(macroUtils, nonempty)
    .settings(moduleName := "adt-macros")

  lazy val macroUtilsJVM = macroUtils.jvm
  lazy val macroUtilsJS  = macroUtils.js
  lazy val macroUtils = crossProject(JVMPlatform, JSPlatform)
    .in(file("macro-utils"))
    .configureCross(commonSettings, publicationSettings, definesMacros, utestSettings)
    .settings(
      moduleName := "macro-utils",
      libraryDependencies += "org.scala-lang.modules" %%% "scala-collection-compat" % Ver.ScalaCollCompat)

  lazy val nameFnJVM = nameFn.jvm
  lazy val nameFnJS  = nameFn.js
  lazy val nameFn = crossProject(JVMPlatform, JSPlatform)
    .in(file("name-fn"))
    .configureCross(commonSettings, publicationSettings, definesMacros, utestSettings)
    .settings(moduleName := "name-fn")

  lazy val nonemptyJVM = nonempty.jvm
  lazy val nonemptyJS  = nonempty.js
  lazy val nonempty = crossProject(JVMPlatform, JSPlatform)
    .configureCross(commonSettings, publicationSettings, utestSettings)
    .settings(
      libraryDependencies ++= Seq(
        "org.scalaz"                 %%% "scalaz-core"             % Ver.Scalaz,
        "com.github.japgolly.univeq" %%% "univeq-scalaz"           % Ver.UnivEq,
        "org.scala-lang.modules"     %%% "scala-collection-compat" % Ver.ScalaCollCompat))

  lazy val recursionJVM = recursion.jvm
  lazy val recursionJS  = recursion.js
  lazy val recursion = crossProject(JVMPlatform, JSPlatform)
    .configureCross(commonSettings, publicationSettings, utestSettings)
    .settings(libraryDependencies += "org.scalaz" %%% "scalaz-core" % Ver.Scalaz)

  lazy val scalazExtJVM = scalazExt.jvm
  lazy val scalazExtJS  = scalazExt.js
  lazy val scalazExt = crossProject(JVMPlatform, JSPlatform)
    .in(file("scalaz-ext"))
    .configureCross(commonSettings, publicationSettings, definesMacros, utestSettings)
    .dependsOn(macroUtils)
    .settings(
      moduleName := "scalaz-ext",
      libraryDependencies += "org.scalaz" %%% "scalaz-core" % Ver.Scalaz)

  lazy val stdlibExtJVM = stdlibExt.jvm
  lazy val stdlibExtJS  = stdlibExt.js
  lazy val stdlibExt = crossProject(JVMPlatform, JSPlatform)
    .in(file("stdlib-ext"))
    .configureCross(commonSettings, publicationSettings, utestSettings, useTestUtil)
    .settings(
      moduleName := "stdlib-ext",
      libraryDependencies += "org.scala-lang.modules" %%% "scala-collection-compat" % Ver.ScalaCollCompat)
    .jsSettings(libraryDependencies += "org.scala-js" %%% "scalajs-java-time" % Ver.JavaTimeScalaJs % Test)

  lazy val testUtilJVM = testUtil.jvm
  lazy val testUtilJS  = testUtil.js
  lazy val testUtil = crossProject(JVMPlatform, JSPlatform)
    .in(file("test-util"))
    .configureCross(commonSettings, publicationSettings)
    .settings(
      moduleName := "test-util",
      libraryDependencies ++= Seq(
        "com.github.japgolly.univeq" %%% "univeq" % Ver.UnivEq,
        "com.github.japgolly.univeq" %%% "univeq-scalaz" % Ver.UnivEq,
        "org.scalaz" %%% "scalaz-core" % Ver.Scalaz,
        "com.lihaoyi" %%% "sourcecode" % Ver.SourceCode))

  lazy val utilsJVM = utils.jvm
  lazy val utilsJS  = utils.js
  lazy val utils = crossProject(JVMPlatform, JSPlatform)
    .configureCross(commonSettings, publicationSettings, utestSettings)
    .dependsOn(stdlibExt)
    .settings(
      libraryDependencies ++= Seq(
        "com.github.japgolly.univeq" %%% "univeq" % Ver.UnivEq,
        "com.github.japgolly.nyaya" %%% "nyaya-gen" % Ver.Nyaya % Test,
        "com.github.japgolly.nyaya" %%% "nyaya-prop" % Ver.Nyaya % Test,
        "com.github.japgolly.nyaya" %%% "nyaya-test" % Ver.Nyaya % Test))

  // ===================================================================================================================

  lazy val bench = project.in(file("bench"))
    .dependsOn(recursionJVM % "compile->test")
    .enablePlugins(JmhPlugin)
    .configure(commonSettings.jvm, preventPublication)
    .settings(
      name := "bench",
      libraryDependencies += "com.github.jbellis" % "jamm" % Ver.JAMM,
      fork := true,
      javaOptions ++= Seq("-server", "-Xss8M"),

      // Add the JAMM jar as an agent
      javaOptions in run := {
        val classPath = (dependencyClasspath in Compile).value
        val jammJar = classPath.collectFirst {
          case sbt.Attributed(f) if f.getName.matches("jamm-[0-9.]+\\.jar") => f.getAbsolutePath
        }.get
        val oldOptions = (javaOptions in run).value
        val newOptions = oldOptions :+ s"-javaagent:$jammJar"
        newOptions
      }
    )
}
