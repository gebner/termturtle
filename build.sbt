lazy val root = (project in file(".")).
  settings(
    name := "termturtle",
    scalaVersion := "2.11.7",
    resolvers += Resolver.jcenterRepo,
    libraryDependencies += "at.logic.gapt" %% "gapt" % "2.1",

    resolvers += Resolver.bintrayRepo("underscoreio", "training"),
    libraryDependencies += "underscoreio" %% "doodle" % "0.5.1",

    libraryDependencies += "org.jfree" % "jfreesvg" % "3.0"
  )
