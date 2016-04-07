logLevel := Level.Warn

resolvers += "spray repo" at "http://repo.spray.io"

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.2.0")

addSbtPlugin("io.spray" % "sbt-revolver" % "0.8.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8")