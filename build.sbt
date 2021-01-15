import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.angelsacademy"
ThisBuild / organizationName := "AngelsAcademy"

lazy val root = (project in file("."))
  .settings(
    name := "immunization",
    libraryDependencies += scalaTest % Test
  )

libraryDependencies ++= Seq(
  "com.google.apis"         %  "google-api-services-bigquery" % "v2-rev405-1.25.0",
  "com.google.oauth-client" %  "google-oauth-client"          % "1.25.0",
  "com.google.oauth-client" %  "google-oauth-client-jetty"    % "1.25.0",
  "com.google.http-client"  %  "google-http-client-jackson2"  % "1.25.0",
  "ch.qos.logback"          %  "logback-classic"              % "1.2.3"   % Test,
  "org.scalatest"           %% "scalatest"                    % "3.0.5"   % Test
)

libraryDependencies += "joda-time" % "joda-time" % "2.10.8"
libraryDependencies += "com.google.apis" % "google-api-services-sheets" % "v4-rev1-1.21.0"
libraryDependencies += "io.spray" %%  "spray-json" % "1.3.6"
libraryDependencies += "com.madgag" %% "scala-io-file" % "0.4.9"
libraryDependencies += "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.11.1"
libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.11.1"
libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.11.1"
// libraryDependencies += "org.apache.logging.log4j" %% "log4j-api-scala" % "11.0"

// Uncomment the following for publishing to Sonatype.
// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for more detail.

ThisBuild / description := "Report on Immunization based on a Google Sheet"
ThisBuild / licenses    := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage    := Some(url("https://github.com/MidnightSkulker/Immunization"))
ThisBuild / scmInfo := Some(
  ScmInfo(
     url("https://github.com/MidnightSkulker/Immunization"),
     "scm:git@github.com/MidnightSkulker/Immunization"
   )
 )
 ThisBuild / developers := List(
   Developer(
     id    = "MidnightSkulker",
     name  = "Peter White",
     email = "desourdesourde@gmail.com",
     url   = url("https://github.com/MidnightSkulker/Immunization")
  )
 )

// ThisBuild / pomIncludeRepository := { _ => false }
// ThisBuild / publishTo := {
//   val nexus = "https://oss.sonatype.org/"
//   if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
//   else Some("releases" at nexus + "service/local/staging/deploy/maven2")
// }
// ThisBuild / publishMavenStyle := true
