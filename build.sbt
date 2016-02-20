import Dependencies._

name := "cddcore2"

lazy val commonSettings = Seq(
  version := "0.1.0",
  scalaVersion := scalaVersionNo,
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  javaOptions ++= Seq("-Xmx4G", "-XX:+UseConcMarkSweepGC"),
  libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)

lazy val utilitiesSettings = commonSettings ++ Seq(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersionNo
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
  aggregate(utilities).
  dependsOn(enginecomponents).
  aggregate(enginecomponents)

lazy val rendering = (project in file("module/rendering")).
  settings(commonSettings: _*).
  dependsOn(engine % "test->test;compile->compile").
  aggregate(engine)

lazy val examples = (project in file("module/examples")).
  settings(commonSettings: _*).
  dependsOn(engine % "test->test;compile->compile").
  aggregate(engine)

lazy val test = (project in file("module/test")).
  settings(commonSettings: _*).
  dependsOn(enginecomponentstest % "test->test").
  aggregate(enginecomponentstest).
  dependsOn(engine).
  aggregate(engine)