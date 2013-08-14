import java.io.File
import sbt._
import Keys._
import Process._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys

object YinYangBuild extends Build {

  lazy val formatSettings = SbtScalariform.scalariformSettings ++ Seq(
    ScalariformKeys.preferences in Compile := formattingPreferences,
    ScalariformKeys.preferences in Test    := formattingPreferences
  )

  def formattingPreferences = {
    import scalariform.formatter.preferences._
    FormattingPreferences()
    .setPreference(RewriteArrowSymbols, false)
    .setPreference(AlignParameters, true)
    .setPreference(AlignSingleLineCaseStatements, true)
  }
  lazy val scalaOrg = "org.scala-lang.virtualized"
  lazy val scalaSettings = Defaults.defaultSettings ++ Seq(
    scalaOrganization := scalaOrg,
    scalaVersion := "2.10.2-RC1"
  )

  lazy val defaults = scalaSettings ++ formatSettings ++ Seq(
    resolvers in ThisBuild += ScalaToolsSnapshots,
    resolvers +=  "OSSH" at "https://oss.sonatype.org/content/groups/public",
    resolvers += Resolver.sonatypeRepo("snapshots"),

    // paths - so we don't need to have src/main/scala ... just src/ test/ and resources/
    scalaSource in Compile <<= baseDirectory(_ / "src"),
    scalaSource in Test <<= baseDirectory(_ / "test"),
    resourceDirectory in Compile <<= baseDirectory(_ / "resources"),

    // sbteclipse needs some info on source directories:
    unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_)),
    unmanagedSourceDirectories in Test <<= (scalaSource in Test)(Seq(_)),

    // add the library, reflect, and the compiler as libraries
    libraryDependencies <<= scalaVersion(ver => Seq(
      scalaOrg % "scala-library" % ver,
      scalaOrg % "scala-reflect" % ver,
      scalaOrg % "scala-compiler" % ver,
      "org.scalatest" % "scalatest_2.10" % "2.0.M6-SNAP7" % "test",
      "junit" % "junit" % "4.8.1" % "test" // we need JUnit explicitly
    )),

    // add the macro paradise compiler plugin
//    addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise_2.10.2" % "2.0.0-SNAPSHOT"),

    // add scalac options (verbose deprecation warnings)
    scalacOptions += "-deprecation",

    // testing
    parallelExecution in Test := false,
    organization := "ch.epfl.lamp"
  )

   // delite settings
  lazy val deliteSettings = defaults ++ Seq(
    libraryDependencies += "stanford-ppl" % "optiml_2.10" % "0.1-SNAPSHOT",
    libraryDependencies += "stanford-ppl" % "optigraph_2.10" % "0.1-SNAPSHOT",
    libraryDependencies += "EPFL" % "lms_2.10" % "0.3-SNAPSHOT"
  )

  lazy val _yinyang        = Project(id = "root",                     base = file("."), settings = Project.defaultSettings ++ Seq(publishArtifact := false)) aggregate (framework, vector_dsl, vector_dsl_test)
  lazy val framework       = Project(id = "yinyang",                  base = file("components/framework"), settings = defaults ++ Seq(name := "yinyang"))
  lazy val vector_dsl      = Project(id = "yinyang-vector-dsl",       base = file("components/dsls/vector"), settings = defaults) dependsOn(framework)
  lazy val vector_dsl_test = Project(id = "yinyang-vector-dsl-test",  base = file("components/dsls/vector-test"), settings = defaults) dependsOn(framework, vector_dsl)
  lazy val delite_test = Project(id = "delite-test",  base = file("components/delite-test"), settings = deliteSettings) dependsOn(framework)
}
