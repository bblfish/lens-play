
lazy val root = (project in file("."))
  .settings(
    name := "Lenses Play",
    scalaVersion := "3.0.2",
    libraryDependencies ++= Seq(
       //https://repo1.maven.org/maven2/dev/optics/
		"dev.optics" %% "monocle-core"  % "3.1.0",
		"dev.optics" %% "monocle-macro"  % "3.1.0",
		 //"org.typelevel" %% "shapeless3-deriving" % "3.0.1",
		//we just want the URI class from akka
//		 "com.typesafe.akka" %% "akka-http" %  "10.2.6" cross CrossVersion.for3Use2_13,
		 // @see [[https://scalameta.org/munit/docs/getting-started.html Getting Started]]
	    // @see https://mvnrepository.com/artifact/org.scalameta/munit
		"org.scalameta" %% "munit" % "0.7.29" % Test
		)
	)


