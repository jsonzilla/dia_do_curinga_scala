name := "DiaDoCuringaScala2"

lazy val main = project.in(file(".")).settings(
  name := "DiaDoCuringaScala2",
  version := "1.1",
  scalaVersion := "3.0.0",
  scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
  libraryDependencies ++= Seq(
    "org.scalactic" %% "scalactic" % "3.2.10",
    "org.scalatest" %% "scalatest-funsuite" % "3.2.10" % "test"
  )
)

