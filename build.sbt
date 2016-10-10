name := "Logic"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.0" % "test"

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "50", "-workers", "1", "-verbosity", "1")

libraryDependencies += "org.scalactic" %% "scalactic" % "2.2.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"

scalacOptions += "-Xplugin-require:scalaxy-streams"

scalacOptions in Test ~= (_ filterNot (_ == "-Xplugin-require:scalaxy-streams"))

scalacOptions in Test += "-Xplugin-disable:scalaxy-streams"

autoCompilerPlugins := true

addCompilerPlugin("com.nativelibs4java" %% "scalaxy-streams" % "0.3.4")

scalacOptions ++= Seq("-optimise", "-Yclosure-elim", "-Yinline")