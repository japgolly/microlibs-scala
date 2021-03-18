import sbt._
import Keys._
import com.jsuereth.sbtpgp.PgpKeys
import dotty.tools.sbtplugin.DottyPlugin.autoImport._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import pl.project13.scala.sbt.JmhPlugin
import sbtcrossproject.CrossPlugin.autoImport._
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import sbtrelease.ReleasePlugin.autoImport._
import Lib._

object Microlibs {
  import Dependencies._

  private val ghProject = "microlibs-scala"

  private val publicationSettings =
    Lib.publicationSettings(ghProject)

  def scalacCommonFlags = Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
  )

  def scalac2Flags = Seq(
    "-opt:l:inline",
    "-opt-inline-from:japgolly.microlibs.**",
    "-opt-inline-from:japgolly.univeq.**",
    "-Ywarn-dead-code",
    "-Ywarn-unused",
    "-Ywarn-value-discard",
  )

  def scalac3Flags = Seq(
    "-source:3.0-migration",
    "-Ykind-projector",
  )

  val commonSettings = ConfigureBoth(
    _.settings(
      organization                  := "com.github.japgolly.microlibs",
      homepage                      := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                      += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      // scalaVersion                  := Ver.Scala213,
      scalaVersion                  := Ver.Scala3,
      crossScalaVersions            := Seq(Ver.Scala212, Ver.Scala213, Ver.Scala3),
      scalacOptions                ++= scalacCommonFlags,
      scalacOptions                ++= byScalaVersion {
                                         case (2, _) => scalac2Flags
                                         case (3, _) => scalac3Flags
                                       }.value,
      scalacOptions in Test        --= Seq("-Ywarn-dead-code", "-Ywarn-unused"),
      testFrameworks                := Nil,
      shellPrompt in ThisBuild      := ((s: State) => Project.extract(s).currentRef.project + "> "),
      updateOptions                 := updateOptions.value.withCachedResolution(true),
      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
      releaseTagComment             := s"v${(version in ThisBuild).value}",
      releaseVcsSign                := true,
      libraryDependencies          ++= Seq(Dep.KindProjector).filterNot(_ => isDotty.value),
  ))

  def definesMacros = ConfigureBoth(
    _.settings(
      scalacOptions       ++= (if (isDotty.value) Nil else Seq("-language:experimental.macros")),
      libraryDependencies ++= (if (isDotty.value) Nil else Seq(Dep.ScalaCompiler.value % Provided)),
  ))

  def utestSettings = ConfigureBoth(
    _.settings(
      libraryDependencies += Dep.MTest.value % Test,
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
    .configureCross(commonSettings, crossProjectScalaDirs, publicationSettings, definesMacros, utestSettings)
    .dependsOn(macroUtils, nonempty)
    .settings(moduleName := "adt-macros")

  lazy val macroUtilsJVM = macroUtils.jvm
  lazy val macroUtilsJS  = macroUtils.js
  lazy val macroUtils = crossProject(JVMPlatform, JSPlatform)
    .in(file("macro-utils"))
    .configureCross(commonSettings, crossProjectScalaDirs, publicationSettings, definesMacros, utestSettings)
    .settings(
      moduleName := "macro-utils",
      scalacOptions --= Seq("-source:3.0-migration"),
      libraryDependencies += Dep.ScalaCollCompat.value)

  lazy val nameFnJVM = nameFn.jvm
  lazy val nameFnJS  = nameFn.js
  lazy val nameFn = crossProject(JVMPlatform, JSPlatform)
    .in(file("name-fn"))
    .configureCross(commonSettings, crossProjectScalaDirs, publicationSettings, definesMacros, utestSettings)
    .settings(moduleName := "name-fn")

  lazy val nonemptyJVM = nonempty.jvm
  lazy val nonemptyJS  = nonempty.js
  lazy val nonempty = crossProject(JVMPlatform, JSPlatform)
    .configureCross(commonSettings, publicationSettings, utestSettings)
    .settings(
      libraryDependencies ++= Seq(
        Dep.Scalaz         .value,
        Dep.UnivEqScalaz   .value,
        Dep.ScalaCollCompat.value))

  lazy val recursionJVM = recursion.jvm
  lazy val recursionJS  = recursion.js
  lazy val recursion = crossProject(JVMPlatform, JSPlatform)
    .configureCross(commonSettings, crossProjectScalaDirs, publicationSettings, utestSettings)
    .settings(libraryDependencies += Dep.Scalaz.value)

  lazy val scalazExtJVM = scalazExt.jvm
  lazy val scalazExtJS  = scalazExt.js
  lazy val scalazExt = crossProject(JVMPlatform, JSPlatform)
    .in(file("scalaz-ext"))
    .configureCross(commonSettings, crossProjectScalaDirs, publicationSettings, definesMacros, utestSettings)
    .dependsOn(macroUtils)
    .settings(
      moduleName := "scalaz-ext",
      scalacOptions --= Seq("-source:3.0-migration"),
      libraryDependencies += Dep.Scalaz.value)

  lazy val stdlibExtJVM = stdlibExt.jvm
  lazy val stdlibExtJS  = stdlibExt.js
  lazy val stdlibExt = crossProject(JVMPlatform, JSPlatform)
    .in(file("stdlib-ext"))
    .configureCross(commonSettings, crossProjectScalaDirs, publicationSettings, utestSettings, useTestUtil)
    .settings(
      moduleName := "stdlib-ext",
      libraryDependencies += Dep.ScalaCollCompat.value)
    .jsSettings(libraryDependencies += Dep.ScalaJsJavaTime.value % Test)

  lazy val testUtilJVM = testUtil.jvm
  lazy val testUtilJS  = testUtil.js
  lazy val testUtil = crossProject(JVMPlatform, JSPlatform)
    .in(file("test-util"))
    .configureCross(commonSettings, publicationSettings, utestSettings)
    .settings(
      moduleName := "test-util",
      libraryDependencies ++= Seq(
        Dep.UnivEq      .value,
        Dep.UnivEqScalaz.value,
        Dep.Scalaz      .value,
        Dep.SourceCode  .value))

  lazy val utilsJVM = utils.jvm
  lazy val utilsJS  = utils.js
  lazy val utils = crossProject(JVMPlatform, JSPlatform)
    .configureCross(commonSettings, publicationSettings, utestSettings)
    .dependsOn(stdlibExt)
    .settings(
      libraryDependencies ++= Seq(
        Dep.UnivEq   .value,
        Dep.NyayaGen .value % Test,
        Dep.NyayaProp.value % Test,
        Dep.NyayaTest.value % Test))

  // ===================================================================================================================

  lazy val bench = project.in(file("bench"))
    .dependsOn(recursionJVM % "compile->test")
    .enablePlugins(JmhPlugin)
    .configure(commonSettings.jvm, preventPublication)
    .settings(
      name := "bench",
      libraryDependencies += Dep.JAMM.value,
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
