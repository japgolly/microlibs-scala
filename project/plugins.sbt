val scalaJSVersion =
  Option(System.getenv("SCALAJS_VERSION")).filter(_.nonEmpty).getOrElse("0.6.32")

addSbtPlugin("com.github.gseitz"  % "sbt-release"              % "1.0.12")
addSbtPlugin("com.jsuereth"       % "sbt-pgp"                  % "1.1.2-1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % scalaJSVersion)
addSbtPlugin("org.xerial.sbt"     % "sbt-sonatype"             % "3.9.4")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"                  % "0.3.7")
