name := "securestring"
version := "0.0.2"
organization := "de.choffmeister"

crossScalaVersions := Seq("2.11.12", "2.12.10", "2.13.1")

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "4.8.3" % Test,
)
