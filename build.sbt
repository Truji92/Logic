name := "Logic"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.0" % "test"

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "50", "-workers", "1", "-verbosity", "1")

libraryDependencies += "org.scalactic" %% "scalactic" % "2.2.6"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
    