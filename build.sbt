name := "SFJD"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "javax.servlet" % "javax.servlet-api" % "3.0.1" % "provided",
  "net.databinder.dispatch" %% "dispatch-core" % "0.11.2",
  "org.scalactic" %% "scalactic" % "2.2.6",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.0" % "test",
  "org.typelevel" %% "cats" % "0.4.0",
  "org.projectlombok" % "lombok" % "1.16.8"
)

lazy val SFJD = (project in file(".")).enablePlugins(JettyPlugin)