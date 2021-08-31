
lazy val root = (project in file("."))
  .settings(
    name := "Lenses Play",
    scalaVersion := "3.0.1",
    libraryDependencies ++= Seq(
       //https://repo1.maven.org/maven2/dev/optics/
      "dev.optics" %% "monocle-core"  % "3.0.0",
//      "dev.optics" %% "monocle-macro" % "3.0.0" // only for Scala 2.13
    )
  )


