organization := "co.s4n"

name := "scala_school_scalaz"

version := "1.0"

scalaVersion := "2.11.0"

libraryDependencies ++= Seq(  
 "org.scalaz" %% "scalaz-core" % "7.1.2",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "org.mockito" % "mockito-core" % "1.9.5",
  "org.specs2" %% "specs2-mock" % "2.4.2"
)

