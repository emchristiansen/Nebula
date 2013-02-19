import sbt._
import Keys._

import sbtassembly.Plugin._
import AssemblyKeys._

object SFSPipelineBuild extends Build {
  def extraResolvers = Seq(
    resolvers ++= Seq(
//      "NativeLibs4Java Respository" at "http://nativelibs4java.sourceforge.net/maven/",
//      "Sonatype OSS Snapshots Repository" at "http://oss.sonatype.org/content/groups/public",
      "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
      "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
//      "repo.codahale.com" at "http://repo.codahale.com",
      "maven.twttr.com" at "http://maven.twttr.com",
      "spray-io" at "http://repo.spray.io/",
      "typesafe-releases" at "http://repo.typesafe.com/typesafe/repo",
      "Expecty Repository" at "https://raw.github.com/pniederw/expecty/master/m2repo/"
    )
  )

  def extraTestFrameworks = Seq(
//    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
  )

  val slf4jVersion = "1.7.2"

  def extraLibraryDependencies = Seq(
    libraryDependencies ++= Seq(
//      "com.nativelibs4java" % "scalacl" % "0.3-SNAPSHOT",
//      "com.nativelibs4java" % "javacl" % "1.0-SNAPSHOT",
//      "com.github.axel22" %% "scalameter" % "0.2",
      "log4j" % "log4j" % "1.2.17",
      "org.slf4j" % "slf4j-api" % slf4jVersion,
      "org.slf4j" % "slf4j-log4j12" % slf4jVersion,
      "org.expecty" % "expecty" % "0.9",
      "commons-lang" % "commons-lang" % "2.6",
      "org.scala-lang" % "scala-reflect" % "2.10.0",
      "org.scala-lang" % "scala-compiler" % "2.10.0",
      "org.apache.commons" % "commons-math3" % "3.1.1",
      "commons-io" % "commons-io" % "2.4",
      "org.scalatest" %% "scalatest" % "2.0.M5b",
      "org.scalacheck" %% "scalacheck" % "1.10.0",
      "org.scala-stm" %% "scala-stm" % "0.7",
      "com.chuusai" %% "shapeless" % "1.2.3",
      "org.clapper" %% "grizzled-scala" % "1.1.3",
      "org.scalanlp" %% "breeze-math" % "0.2-SNAPSHOT",
      "org.spire-math" %% "spire" % "0.3.0",
      "org.scalaz" %% "scalaz-core" % "7.0-SNAPSHOT",
      "io.spray" %%  "spray-json" % "1.2.3",
      "org.rogach" %% "scallop" % "0.8.0",
      "junit" % "junit" % "4.11",
      "org.imgscalr" % "imgscalr-lib" % "4.2"
    )
  )

  def scalaSettings = Seq(
    scalaVersion := "2.10.0",
    scalacOptions ++= Seq(
      "-optimize",
      "-unchecked",
      "-deprecation",
      "-feature",
      "-language:implicitConversions",
      "-language:postfixOps",
      "-language:existentials",
      "-Yinline-warnings"
    )
  )

  def libSettings = Project.defaultSettings ++ extraResolvers ++ extraLibraryDependencies ++ scalaSettings ++ extraTestFrameworks ++ assemblySettings// ++ extraAssemblySettings

  val projectName = "nebula"
  lazy val root = {
    val settings = libSettings ++ Seq(name := projectName)
//    val settings = libSettings ++ Seq(name := "nebula", fork := true)
    Project(id = projectName, base = file("."), settings = settings)
  }
}
