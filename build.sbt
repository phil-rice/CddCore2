name := "cddcore2"

lazy val commonSettings = Seq(
  version := "0.1.0",
  scalaVersion := "2.11.7",
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  javaOptions ++= Seq("-Xmx4G", "-XX:+UseConcMarkSweepGC"),
  libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)

lazy val core = (project in file("module/core")).
  settings(commonSettings: _*)

lazy val rendering = (project in file("module/rendering")).
  settings(commonSettings: _*).
  dependsOn(core % "test->test;compile->compile").
  aggregate(core)

lazy val examples = (project in file("module/examples")).
  settings(commonSettings: _*).
  dependsOn(core % "test->test;compile->compile").
  aggregate(core)
