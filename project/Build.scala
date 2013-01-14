import java.io.File
import sbt._
import Keys._
import Process._

object MPDEBuild extends Build {

  val scala = "2.10.0-SNAPSHOT"

  val defaults = Defaults.defaultSettings ++ Seq(
    // scala version + resolver
    scalaVersion := scala,
    //scalaBinaryVersion := scala,
    resolvers in ThisBuild += ScalaToolsSnapshots, // to get 2.10.0-SNAPSHOT,
    resolvers +=  "OSSH" at "https://oss.sonatype.org/content/groups/public",

    // paths - so we don't need to have src/main/scala ... just src/ test/ and resources/
    scalaSource in Compile <<= baseDirectory(_ / "src"),
    scalaSource in Test <<= baseDirectory(_ / "test"),
    resourceDirectory in Compile <<= baseDirectory(_ / "resources"),

    // sbteclipse needs some info on source directories:
    unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_)),
    unmanagedSourceDirectories in Test <<= (scalaSource in Test)(Seq(_)),

    // add the library, reflect and the compiler as libraries
    libraryDependencies <<= scalaVersion(ver => Seq(
      "org.scala-lang" % "scala-library" % ver,
      "org.scala-lang" % "scala-reflect" % ver,
      "org.scala-lang" % "scala-compiler" % ver, 
      "org.scalatest" % "scalatest_2.10.0-RC5" % "2.0.M5-B1" % "test"
    )),

    // testing
    parallelExecution in Test := false
    // testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD")
  )

  lazy val _mpde         = Project(id = "mpde",             base = file(".")) aggregate (framework, vector_dsl)
  lazy val framework     = Project(id = "mpde-framework",   base = file("components/framework"), settings = defaults)
  lazy val vector_dsl    = Project(id = "mpde-vector-dsl",  base = file("components/dsls/vector"), settings = defaults) dependsOn(framework)
}
