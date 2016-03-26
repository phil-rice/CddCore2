name := "cddcore2"


val versionNos = new {
  val scala = "2.11.7"
  val scalaTest = "2.2.6"
  val mustache = "0.9.1"
  val json4s = "3.3.0"
  val junit = "4.11"
  val scalaXml = "1.0.5"
}

lazy val baseSettings = Seq(
  version := "0.1.0",
  scalaVersion := versionNos.scala,
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  javaOptions ++= Seq("-Xmx4G", "-XX:+UseConcMarkSweepGC"),
  testOptions in Test += Tests.Argument("-oCOLHPQ")
)

lazy val commonSettings = baseSettings ++ Seq(
  libraryDependencies += "org.scalatest" %% "scalatest" % versionNos.scalaTest % "test"
)

lazy val utilitiesSettings = commonSettings ++ Seq(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % versionNos.scala
)

lazy val renderingSettings = commonSettings ++ Seq(
  libraryDependencies += "com.github.spullara.mustache.java" % "scala-extensions-2.11" % versionNos.mustache,
  libraryDependencies += "org.json4s" %% "json4s-jackson" % versionNos.json4s
)

lazy val junitSettings = commonSettings ++ Seq(
  libraryDependencies += "junit" % "junit" % versionNos.junit
)

lazy val structureSettings = commonSettings ++ Seq(
  libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % versionNos.scalaXml,
  libraryDependencies += "junit" % "junit" % versionNos.junit % "test"
)
lazy val utilities = (project in file("module/utilities")).
  settings(utilitiesSettings: _*)


lazy val enginecomponents = (project in file("module/enginecomponents")).
  settings(commonSettings: _*).
  dependsOn(utilities % "test->test;compile->compile").
  aggregate(utilities)

lazy val enginecomponentstest = (project in file("module/enginecomponentstest")).
  settings(commonSettings: _*).
  dependsOn(utilities % "test->test;compile->compile").
  aggregate(utilities).
  dependsOn(enginecomponents % "test->test;compile->compile").
  aggregate(enginecomponents)

lazy val engine = (project in file("module/engine")).
  settings(commonSettings: _*).
  dependsOn(utilities % "test->test").
  dependsOn(enginecomponents).
  dependsOn(enginecomponentstest % "test->test").
  aggregate(enginecomponentstest)


lazy val rendering = (project in file("module/rendering")).
  settings(renderingSettings: _*).
  dependsOn(engine % "test->test;compile->compile").
  aggregate(engine)

lazy val cddunit = (project in file("module/cddunit")).
  settings(junitSettings: _*).
  dependsOn(engine % "test->test;compile->compile").dependsOn(rendering).
  aggregate(rendering)

lazy val examples = (project in file("module/examples")).
  settings(commonSettings: _*).
  dependsOn(engine % "test->test;compile->compile", rendering, cddunit, structure).
  aggregate(engine, rendering, cddunit, structure)

lazy val structure = (project in file("module/structure")).
  settings(structureSettings: _*).
  dependsOn(utilities % "test->test;compile->compile").
  aggregate(utilities)

lazy val test = (project in file("module/test")).
  settings(commonSettings: _*).
  dependsOn(enginecomponentstest % "test->test").
  aggregate(enginecomponentstest).
  dependsOn(examples % "test->test").
  aggregate(examples).
  dependsOn(engine).
  aggregate(engine).
  dependsOn(rendering).
  dependsOn(cddunit).
  aggregate(rendering).
  aggregate(cddunit)