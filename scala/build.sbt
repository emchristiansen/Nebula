import AssemblyKeys._

assemblySettings

name := "nebula"

scalaVersion := "2.9.2"

scalacOptions ++= Seq(
"-optimize",
"-unchecked",
"-deprecation"
)

libraryDependencies ++= Seq(
"commons-lang" % "commons-lang" % "2.6",
"org.apache.commons" % "commons-math3" % "3.0",
"org.apache.commons" % "commons-math" % "2.2",
"commons-io" % "commons-io" % "2.3",
"com.frugalmechanic" % "scala-optparse" % "1.0"
)