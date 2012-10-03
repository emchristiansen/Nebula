import sbt._
import Keys._

import sbtassembly.Plugin._
import AssemblyKeys._

object SFSPipelineBuild extends Build {
  def extraResolvers = Seq(
    resolvers ++= Seq(
      "NativeLibs4Java Respository" at "http://nativelibs4java.sourceforge.net/maven/",
      "Sonatype OSS Snapshots Repository" at "http://oss.sonatype.org/content/groups/public",
      "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
      "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
      "repo.codahale.com" at "http://repo.codahale.com",
      "maven.twttr.com" at "http://maven.twttr.com",
      "typesafe-releases" at "http://repo.typesafe.com/typesafe/repo"
    )
  )

  def extraLibraryDependencies = Seq(
    libraryDependencies ++= Seq(
//      "com.nativelibs4java" % "scalacl" % "0.3-SNAPSHOT",
      "com.nativelibs4java" % "javacl" % "1.0-SNAPSHOT",
      "commons-lang" % "commons-lang" % "2.6",
      "org.apache.commons" % "commons-math3" % "3.0",
      "org.apache.commons" % "commons-math" % "2.2",
      "commons-io" % "commons-io" % "2.3",
//      "com.frugalmechanic" % "scala-optparse" % "1.0",
      "org.scalatest" %% "scalatest" % "1.8" % "test",
      "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
      "com.github.mhendred.face4j" % "face4j-core" % "1.6.2",
//      "org.scala-tools" %% "scala-stm" % "0.6",
      "net.liftweb" % "lift-json_2.9.1" % "2.4-RC1",
      "com.twitter" % "util-eval" % "5.3.6",
//      "com.chuusai" %% "shapeless" % "1.2.2",
//      "play" % "play_2.9.1" % "2.0.3",
      "org.scalanlp" %% "breeze-math" % "0.1",
      "org.scalanlp" %% "breeze-learn" % "0.1",
      "org.scalanlp" %% "breeze-process" % "0.1",
      "org.scalanlp" %% "breeze-viz" % "0.1"
    )
  )

  def scalaSettings = Seq(
    scalaVersion := "2.9.2",
    scalacOptions ++= Seq(
      "-optimize",
      "-unchecked",
      "-deprecation",
      "-Ydependent-method-types"
    )
  )

  def libSettings = Project.defaultSettings ++ extraResolvers ++ extraLibraryDependencies ++ scalaSettings ++ assemblySettings

  lazy val root = {
    val settings = libSettings ++ Seq(name := "nebula")
    Project(id = "nebula", base = file("."), settings = settings)
  }
}