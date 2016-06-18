organization := "co.s4n"

name := "scala_school_scalaz"

version := "1.0"

scalaVersion := "2.11.0"

libraryDependencies ++= Seq(  
 "org.scalaz" %% "scalaz-core" % "7.2.0" withSources() withJavadoc(),
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "org.mockito" % "mockito-core" % "1.9.5",
  "org.specs2" %% "specs2-core" % "3.8.3" % "test"
)

