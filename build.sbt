val scalatest   = "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
val scalacheck  = "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"
val junit       = "com.novocode" % "junit-interface" % "0.11" % "test"

val slf4jApi    = "org.slf4j" % "slf4j-api" % "1.7.5"
val slf4jBind   = "org.slf4j" % "slf4j-log4j12" % "1.7.12"
val slf4jSimple = "org.slf4j" % "slf4j-simple" % "1.6.4"
val log4j       = "log4j" % "log4j" % "1.2.14"

val testLibraries = Seq(scalatest, scalacheck, junit)
val logging = Seq(slf4jApi, slf4jBind, slf4jSimple)

lazy val commonSettings = Seq(
  organization := "org.kokho",
  version := "0.1.0",
  scalaVersion := "2.10.4"
)

lazy val master = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "Scheduling of Multicritical Real-Time Systems",
//    libraryDependencies ++= logging,
    libraryDependencies ++= testLibraries
  )