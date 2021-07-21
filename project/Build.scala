import sbt._
import Keys._
import com.jsuereth.sbtpgp.PgpKeys
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
      scalaVersion                  := Ver.scala2,
      crossScalaVersions            := Seq(Ver.scala2, Ver.scala3),
      scalacOptions                ++= scalacCommonFlags,
      scalacOptions                ++= byScalaVersion {
                                         case (2, _) => scalac2Flags
                                         case (3, _) => scalac3Flags
                                       }.value,
      Test / scalacOptions         --= Seq("-Ywarn-dead-code"),
      testFrameworks                := Nil,
      ThisBuild / shellPrompt       := ((s: State) => Project.extract(s).currentRef.project + "> "),
      updateOptions                 := updateOptions.value.withCachedResolution(true),
      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
      releaseTagComment             := s"v${(ThisBuild / version).value}",
      releaseVcsSign                := true,
      libraryDependencies          ++= Seq(Dep.kindProjector).filterNot(_ => scalaVersion.value.startsWith("3")),
  ))

  def definesMacros = ConfigureBoth(
    _.settings(
      scalacOptions       ++= (if (scalaVersion.value startsWith "3") Nil else Seq("-language:experimental.macros")),
      libraryDependencies ++= (if (scalaVersion.value startsWith "3") Nil else Seq(Dep.scalaCompiler.value % Provided)),
  ))

  def utestSettings = ConfigureBoth(
    _.settings(
      libraryDependencies += Dep.utest.value % Test,
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
        adtMacrosJVM,
        catsExtJVM,
        compileTimeJVM,
        disjunctionJVM,
        nameFnJVM,
        nonemptyJVM,
        recursionJVM,
        stdlibExtJVM,
        testUtilJVM,
        typesJVM,
        utilsJVM,
      )

  lazy val rootJS =
    Project("JS", file(".rootJS"))
      .configure(commonSettings.jvm, preventPublication)
      .aggregate(
        adtMacrosJS,
        catsExtJS,
        compileTimeJS,
        disjunctionJS,
        nameFnJS,
        nonemptyJS,
        recursionJS,
        stdlibExtJS,
        testUtilJS,
        typesJS,
        utilsJS,
      )

  // ===================================================================================================================

  lazy val adtMacrosJVM = adtMacros.jvm
  lazy val adtMacrosJS  = adtMacros.js
  lazy val adtMacros = crossProject(JVMPlatform, JSPlatform)
    .in(file("adt-macros"))
    .configureCross(commonSettings, publicationSettings, definesMacros, utestSettings)
    .dependsOn(compileTime, nonempty)
    .settings(
      moduleName := "adt-macros",
      scalacOptions --= Seq("-source:3.0-migration"))

  lazy val catsExtJVM = catsExt.jvm
  lazy val catsExtJS  = catsExt.js
  lazy val catsExt = crossProject(JVMPlatform, JSPlatform)
    .in(file("cats-ext"))
    .configureCross(commonSettings, publicationSettings, definesMacros, utestSettings)
    .dependsOn(compileTime)
    .settings(
      moduleName := "cats-ext",
      scalacOptions --= Seq("-source:3.0-migration"),
      libraryDependencies += Dep.catsCore.value)

  lazy val compileTimeJVM = compileTime.jvm
  lazy val compileTimeJS  = compileTime.js
  lazy val compileTime = crossProject(JVMPlatform, JSPlatform)
    .in(file("compile-time"))
    .configureCross(commonSettings, publicationSettings, definesMacros, utestSettings)
    .settings(
      moduleName := "compile-time",
      scalacOptions --= Seq("-source:3.0-migration"),
      libraryDependencies ++= Seq(Dep.sourceCode.value).filterNot(_ => scalaVersion.value.startsWith("3")))

  lazy val disjunctionJVM = disjunction.jvm
  lazy val disjunctionJS  = disjunction.js
  lazy val disjunction = crossProject(JVMPlatform, JSPlatform)
    .configureCross(commonSettings, publicationSettings)

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
        Dep.catsCore  .value,
        Dep.univEqCats.value,
      ))

  lazy val recursionJVM = recursion.jvm
  lazy val recursionJS  = recursion.js
  lazy val recursion = crossProject(JVMPlatform, JSPlatform)
    .configureCross(commonSettings, publicationSettings, utestSettings)
    .settings(
      libraryDependencies ++= Seq(
        Dep.catsCore.value,
        Dep.catsFree.value,
      ),
    )

  lazy val stdlibExtJVM = stdlibExt.jvm
  lazy val stdlibExtJS  = stdlibExt.js
  lazy val stdlibExt = crossProject(JVMPlatform, JSPlatform)
    .in(file("stdlib-ext"))
    .configureCross(commonSettings, publicationSettings, utestSettings, useTestUtil)
    .configure(disableScalaDoc3)
    .settings(
      moduleName := "stdlib-ext",
      libraryDependencies ++= Seq(
        Dep.scalaCheck.value % Test,
      ))
    .jsSettings(libraryDependencies += Dep.scalaJsJavaTime.value % Test)

  lazy val testUtilJVM = testUtil.jvm
  lazy val testUtilJS  = testUtil.js
  lazy val testUtil = crossProject(JVMPlatform, JSPlatform)
    .in(file("test-util"))
    .configureCross(commonSettings, publicationSettings, utestSettings)
    .dependsOn(compileTime)
    .settings(
      moduleName := "test-util",
      libraryDependencies ++= Seq(
        Dep.catsCore  .value,
        Dep.sourceCode.value,
        Dep.univEq    .value,
        Dep.univEqCats.value,
      ))

  lazy val typesJVM = types.jvm
  lazy val typesJS  = types.js
  lazy val types = crossProject(JVMPlatform, JSPlatform)
    .configureCross(commonSettings, publicationSettings, utestSettings)
    .dependsOn(testUtil % "test->compile")

  lazy val utilsJVM = utils.jvm
  lazy val utilsJS  = utils.js
  lazy val utils = crossProject(JVMPlatform, JSPlatform)
    .configureCross(commonSettings, publicationSettings, utestSettings)
    .dependsOn(stdlibExt)
    .settings(
      libraryDependencies ++= Seq(
        Dep.univEq    .value,
        Dep.scalaCheck.value % Test,
      ))

  // ===================================================================================================================

  lazy val bench = project.in(file("bench"))
    .dependsOn(recursionJVM % "compile->test")
    .enablePlugins(JmhPlugin)
    .configure(commonSettings.jvm, preventPublication)
    .settings(
      name := "bench",
      libraryDependencies += Dep.jamm.value,
      fork := true,
      javaOptions ++= Seq("-server", "-Xss8M"),

      // Add the JAMM jar as an agent
      run / javaOptions := {
        val classPath = (Compile / dependencyClasspath).value
        val jammJar = classPath.collectFirst {
          case sbt.Attributed(f) if f.getName.matches("jamm-[0-9.]+\\.jar") => f.getAbsolutePath
        }.get
        val oldOptions = (run / javaOptions).value
        val newOptions = oldOptions :+ s"-javaagent:$jammJar"
        newOptions
      }
    )
}
