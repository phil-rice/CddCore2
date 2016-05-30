import sbt.Keys._

name := "cddcore"

val versionNos = new {
  val scala = "2.11.7"
  val scalaTest = "2.2.6"
  val mustache = "0.9.1"
  val json4s = "3.3.0"
  val junit = "4.11"
  val novacode = "0.11"
  val scalaXml = "1.0.5"
  val jetty = "9.3.8.v20160314"
  val testInterface = "1.0"
}

lazy val baseSettings = Seq(
  organization := "org.cddcore",
  version := "3.0.2",
  scalaVersion := versionNos.scala,
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  javaOptions ++= Seq("-Xmx4G", "-XX:+UseConcMarkSweepGC"),
  //  testOptions in Test += Tests.Argument("-oCOLHPQ"),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    "Twitter Maven" at "https://maven.twttr.com"
  ),

  //  publishTo <<= version { (v: String) =>
  //    val nexus = "https://oss.sonatype.org/"
  //    if (v.trim.endsWith("SNAPSHOT"))
  //      Some("snapshots" at nexus + "content/repositories/snapshots")
  //    else
  //      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  //  },
  pomIncludeRepository := { _ => false },
  publishMavenStyle := true,
  publishArtifact in Test := false,
  //  credentials += Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", "phil.rice", "jirapsr123"),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  pomIncludeRepository := { _ => false },
  pomExtra in ThisBuild := (
      <url>http://www.constraintdrivendevelopment.com</url>
      <licenses>
        <license>
          <name>BSD-style</name>
          <url>http://www.opensource.org/licenses/bsd-license.php</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>https://github.com/phil-rice/autoTdd</url>
        <connection>git@github.com:phil-rice/cddcore2.git</connection>
      </scm>
      <developers>
        <developer>
          <id>phil.rice</id>
          <name>Phil Rice</name>
          <url>http://www.constraintdrivendevelopment.org</url>
        </developer>
      </developers>),
  publishArtifact in Test := false
)

testFrameworks += new TestFramework("org.cddcore.testinterface.CddFramework")

lazy val commonSettings = baseSettings ++ Seq(
  testFrameworks += new TestFramework("org.cddcore.testinterface.CddFramework"),
  libraryDependencies += "org.scalatest" %% "scalatest" % versionNos.scalaTest % "test",
  libraryDependencies += "com.novocode" % "junit-interface" % versionNos.novacode % "test"
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

lazy val testInterfaceSettings = commonSettings ++ Seq(
  libraryDependencies += "org.scala-sbt" % "test-interface" % versionNos.testInterface

)
lazy val websiteSettings = commonSettings ++ Seq(
  libraryDependencies += "org.eclipse.jetty" % "jetty-servlet" % versionNos.jetty
)

lazy val structureSettings = junitSettings ++ Seq(
  libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % versionNos.scalaXml
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  aggregate(test)

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
  aggregate(rendering).aggregate(engine)

lazy val testInterface = (project in file("module/testinterface")).
  settings(testInterfaceSettings: _*).
  dependsOn(engine % "test->test;compile->compile").
  dependsOn(cddunit)

lazy val examples = (project in file("module/examples")).
  settings(commonSettings: _*).
  dependsOn(engine % "test->test;compile->compile", website, cddunit, structure).
  aggregate(engine, website, cddunit, structure)

lazy val structure = (project in file("module/structure")).
  settings(structureSettings: _*).
  dependsOn(utilities % "test->test;compile->compile").
  aggregate(utilities)

lazy val website = (project in file("module/website")).
  settings(websiteSettings: _*).
  dependsOn(utilities % "test->test;compile->compile", rendering).
  aggregate(utilities, rendering)

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
  dependsOn(testInterface).
  aggregate(rendering).
  aggregate(cddunit).
  aggregate(testInterface)