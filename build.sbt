name := "Scheduling of Multicritical Real-Time Systems"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "1.7.5",
  //"org.slf4j" % "slf4j-simple" % "1.7.5",
  "org.slf4j" % "slf4j-log4j12" % "1.7.12")

libraryDependencies += "log4j" % "log4j" % "1.2.14"
