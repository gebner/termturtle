lazy val root = (project in file(".")).
  settings(
    name := "termturtle",
    scalaVersion := "2.11.7",
    libraryDependencies += "at.logic.gapt" %% "gapt" % "1.11-SNAPSHOT",

    resolvers += "Underscore Training" at "https://dl.bintray.com/underscoreio/training",
    libraryDependencies += "underscoreio" %% "doodle" % "0.1.0",

    libraryDependencies += "org.jfree" % "jfreesvg" % "3.0"
  )
