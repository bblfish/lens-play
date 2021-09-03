
lazy val root = (project in file("."))
  .settings(
    name := "Lenses Play",
    scalaVersion := "3.0.1",
    libraryDependencies ++= Seq(
       //https://repo1.maven.org/maven2/dev/optics/
      "dev.optics" %% "monocle-core"  % "3.1.0",
		 // @see [[https://scalameta.org/munit/docs/getting-started.html Getting Started]]
	    // @see https://mvnrepository.com/artifact/org.scalameta/munit
		 "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )


