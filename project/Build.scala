import java.io.File
import sbt._
import Keys._
import Process._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys

object MPDEBuild extends Build {

  lazy val formatSettings = SbtScalariform.scalariformSettings ++ Seq(
    ScalariformKeys.preferences in Compile := formattingPreferences,
    ScalariformKeys.preferences in Test    := formattingPreferences
  )

  def formattingPreferences = {
    import scalariform.formatter.preferences._
    FormattingPreferences()
    .setPreference(RewriteArrowSymbols, true)
    .setPreference(AlignParameters, true)
    .setPreference(AlignSingleLineCaseStatements, true)
  }

  lazy val defaults = Defaults.defaultSettings ++ formatSettings ++ Seq(
    // scala version + resolver
    scalaVersion := "2.10.1-SNAPSHOT",
    resolvers in ThisBuild += ScalaToolsSnapshots,
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
      "org.scalatest" % "scalatest_2.10" % "2.0.M6-SNAP7" % "test",
      "junit" % "junit" % "4.8.1" % "test" // we need JUnit explicitly
    )),

    // testing
    parallelExecution in Test := false
    // testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD")
  )
  
  // delite settings
  lazy val deliteSettings = defaults ++ Seq(
   libraryDependencies += "EPFL" % "lms_2.10" % "0.3-SNAPSHOT" // just LMS for now
  )

  lazy val _mpde           = Project(id = "mpde",                  base = file(".")) aggregate (framework, vector_dsl, vector_dsl_test)
  lazy val framework       = Project(id = "mpde-framework",        base = file("components/framework"), settings = defaults)
  lazy val vector_dsl      = Project(id = "mpde-vector-dsl",       base = file("components/dsls/vector"), settings = defaults) dependsOn(framework)
  lazy val vector_dsl_test = Project(id = "mpde-vector-dsl-test",  base = file("components/dsls/vector-test"), settings = defaults) dependsOn(framework, vector_dsl)
  lazy val delite_test = Project(id = "delite-test",  base = file("components/delite-test"), settings = deliteSettings) dependsOn(framework)
}
