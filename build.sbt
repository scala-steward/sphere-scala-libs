import pl.project13.scala.sbt.JmhPlugin

// sbt-github-actions needs configuration in `ThisBuild`
ThisBuild / crossScalaVersions := Seq("2.12.12", "2.13.3")
ThisBuild / scalaVersion := crossScalaVersions.value.last
ThisBuild / githubWorkflowPublishTargetBranches := List()
ThisBuild / githubWorkflowJavaVersions := List("adopt@1.15")
ThisBuild / githubWorkflowBuildPreamble ++= List(
  WorkflowStep.Sbt(List("scalafmtCheckAll"), name = Some("Check formatting"))
)

lazy val publishSettings = Seq(
  releaseCrossBuild := true,
  UpdateVersionInFiles("sphere-util", file("README.md")),
  UpdateVersionInFiles("sphere-json", file("README.md"), file("json/README.md")),
  UpdateVersionInFiles("sphere-mongo", file("README.md"), file("mongo/README.md"))
) ++ BintrayPlugin.bintraySettings

lazy val standardSettings = Defaults.coreDefaultSettings ++ publishSettings ++ Seq(
  organization := "io.sphere",
  licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html")),
  logBuffered := false,
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-feature"
  ),
  javacOptions ++= Seq("-deprecation", "-Xlint:unchecked"),
  // targets Java 8 bytecode (scalac & javac)
  scalacOptions in ThisBuild += "-target:jvm-1.8",
  javacOptions in ThisBuild ++= Seq("-source", "8", "-target", "8"),
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.3" % Test,
    "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % Test,
    "org.scalacheck" %% "scalacheck" % "1.15.2" % Test,
    "ch.qos.logback" % "logback-classic" % "1.2.3" % Test
  ),
  shellPrompt in ThisBuild := { state ⇒
    scala.Console.CYAN + Project.extract(state).currentRef.project + "> " + scala.Console.RESET
  }
)

lazy val `sphere-libs` = project
  .in(file("."))
  .settings(standardSettings: _*)
  .settings(publishArtifact := false, publish := {})
  .disablePlugins(BintrayPlugin)
  .aggregate(
    `sphere-util`,
    `sphere-json`,
    `sphere-json-core`,
    `sphere-json-derivation`,
    `sphere-mongo`,
    `sphere-mongo-core`,
    `sphere-mongo-derivation`,
    `sphere-mongo-derivation-magnolia`
  )

lazy val `sphere-util` = project
  .in(file("./util"))
  .settings(standardSettings: _*)
  .settings(homepage := Some(url("https://github.com/commercetools/sphere-scala-libs/README.md")))

lazy val `sphere-json-core` = project
  .in(file("./json/json-core"))
  .settings(standardSettings: _*)
  .dependsOn(`sphere-util`)

lazy val `sphere-json-derivation` = project
  .in(file("./json/json-derivation"))
  .settings(standardSettings: _*)
  .settings(Fmpp.settings: _*)
  .dependsOn(`sphere-json-core`)

lazy val `sphere-json` = project
  .in(file("./json"))
  .settings(standardSettings: _*)
  .settings(homepage := Some(
    url("https://github.com/commercetools/sphere-scala-libs/blob/master/json/README.md")))
  .dependsOn(`sphere-json-core`, `sphere-json-derivation`)

lazy val `sphere-mongo-core` = project
  .in(file("./mongo/mongo-core"))
  .settings(standardSettings: _*)
  .dependsOn(`sphere-util`)

lazy val `sphere-mongo-derivation` = project
  .in(file("./mongo/mongo-derivation"))
  .settings(standardSettings: _*)
  .settings(Fmpp.settings: _*)
  .dependsOn(`sphere-mongo-core`)

lazy val `sphere-mongo-derivation-magnolia` = project
  .in(file("./mongo/mongo-derivation-magnolia"))
  .settings(standardSettings: _*)
  .dependsOn(`sphere-mongo-core`)

lazy val `sphere-mongo` = project
  .in(file("./mongo"))
  .settings(standardSettings: _*)
  .settings(homepage := Some(
    url("https://github.com/commercetools/sphere-scala-libs/blob/master/mongo/README.md")))
  .dependsOn(`sphere-mongo-core`, `sphere-mongo-derivation`)

// benchmarks

lazy val benchmarks = project
  .settings(standardSettings: _*)
  .settings(publishArtifact := false, publish := {})
  .enablePlugins(JmhPlugin)
  .disablePlugins(BintrayPlugin)
  .dependsOn(`sphere-util`, `sphere-json`, `sphere-mongo`)
