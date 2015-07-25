val scalatest   = "org.scalatest" % "scalatest_2.10" % "2.2.5" % "test"
//val scalacheck  = "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"
//val junit       = "com.novocode" % "junit-interface" % "0.11" % "test"

val testLibraries = Seq(scalatest)

lazy val commonSettings = Seq(
  organization := "org.kokho",
  version := "0.1.0",
  scalaVersion := "2.10.5"
)

lazy val master = (project in file(".")).
  settings(commonSettings: _*).
 // enablePlugins(CoverallsPlugin).
  settings(
    name := "Scheduling of Multicritical Real-Time Systems",
    libraryDependencies ++= testLibraries
  )
