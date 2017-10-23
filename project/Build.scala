import sbt._
import sbt.Keys._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys

/**
 * Please use plain text editor to edit this file instead of NetBeans (To be supported)
 */
object Build extends sbt.Build {

  lazy val root = Project("IK", file("."))
    .settings(basicSettings: _*)
    .settings(libraryDependencies ++= Dependencies.basic)

  lazy val basicSettings = Seq(
    organization := "com.millmanorama",
    version := "0.1.0",
    scalaVersion := "2.11.4",
    scalacOptions ++= Seq("-unchecked", "-deprecation"),
    javacOptions ++= Seq("-source", "1.6", "-target", "1.6"),
    resolvers ++= Seq(
      "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases",
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
      "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"))

  // scalariform code format settings
  SbtScalariform.scalariformSettings // enable scalariformSettings
  import scalariform.formatter.preferences._
  ScalariformKeys.preferences := ScalariformKeys.preferences.value
    .setPreference(RewriteArrowSymbols, false)
    .setPreference(AlignParameters, true)
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(DoubleIndentClassDeclaration, true)
    .setPreference(IndentSpaces, 2)
}

object Dependencies {
  // ---- define dependencies libs
  var basic: Seq[ModuleID] =
    Seq("org.scalanlp" %% "breeze" % "0.13.2",
        "org.scalanlp" %% "breeze-natives" % "0.13.2",
        // the visualization library is distributed separately as well. 
        // It depends on LGPL code.
        "org.scalanlp" %% "breeze-viz" % "0.13.2")
}
