name := "cddcore2"

version := "1.0"

scalaVersion := "2.11.7"

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

javaOptions ++= Seq("-Xmx4G", "-XX:+UseConcMarkSweepGC")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
    