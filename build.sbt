import Dependencies._

name := "cddcore2"

lazy val baseSettings = Seq(
  version := "0.1.0",
  scalaVersion := scalaVersionNo,
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  javaOptions ++= Seq("-Xmx4G", "-XX:+UseConcMarkSweepGC")
)

lazy val commonSettings = baseSettings ++ Seq(
  libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)

lazy val utilitiesSettings = commonSettings ++ Seq(
//  libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.6",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersionNo
)

//lazy val mustacheSettings = baseSettings ++ Seq(
//  libraryDependencies ++= Seq(
//    "junit" % "junit" % "4.8.1" % "test->default",
//    "org.specs2" %% "specs2" % "2.3.12" % "test->default",
//    "com.typesafe.akka" %% "akka-actor" % "2.3.3" % "test->default"
//  )
//)

lazy val renderingSettings = commonSettings ++ Seq(
  libraryDependencies += "com.github.spullara.mustache.java" % "scala-extensions-2.11" % "0.9.1",
  libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.3.0"
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

//lazy val mustache = (project in file("module/mustache")).
//  settings(mustacheSettings: _*)


lazy val rendering = (project in file("module/rendering")).
  settings(renderingSettings: _*).
  dependsOn(engine % "test->test;compile->compile").
  aggregate(engine)
//  dependsOn(mustache).
//  aggregate(mustache)


lazy val examples = (project in file("module/examples")).
  settings(commonSettings: _*).
  dependsOn(engine % "test->test;compile->compile").
  aggregate(engine)

lazy val test = (project in file("module/test")).
  settings(commonSettings: _*).
  dependsOn(enginecomponentstest % "test->test").
  aggregate(enginecomponentstest).
  dependsOn(examples % "test->test").
  aggregate(examples).
  dependsOn(engine).
  aggregate(engine).
  dependsOn(rendering).
  aggregate(rendering)