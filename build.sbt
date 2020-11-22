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

libraryDependencies += "joda-time" % "joda-time" % "2.10.8"

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
