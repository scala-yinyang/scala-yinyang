import java.io.File
import sbt._
import Keys._
import com.typesafe.sbt.SbtSite.SiteKeys._
import Process._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import com.typesafe.sbt.SbtSite.site
import com.typesafe.sbt.site.JekyllSupport.Jekyll
import com.typesafe.sbt.SbtGhPages.ghpages
import com.typesafe.sbt.SbtGit.git

object YinYangBuild extends Build {
  lazy val projectSettings = Seq[Setting[_]](
    version              := "0.1-SNAPSHOT",
    organization         := "ch.epfl.lamp",
    licenses             := Seq("New BSD" -> url("https://raw2.github.com/vjovanov/yin-yang/master/LICENSE")),
    homepage             := Some(url("http://scala-yinyang.org/")),
    organizationHomepage := Some(url("http://lamp.epfl.ch")),
    scmInfo              := Some(ScmInfo(url("https://github.com/vjovanov/yin-yang.git"),"git://github.com/vjovanov/yin-yang.git"))
  )

  lazy val scalaSettings = Defaults.defaultSettings ++ Seq(
    scalaOrganization    := scalaOrg,
    scalaVersion         := "2.11.1",
    scalacOptions        := defaultScalacOptions 
  )

  // libraries
  lazy val libraryDeps = Seq(
    libraryDependencies <<= scalaVersion(ver => Seq(
      scalaOrg % "scala-library" % ver,
      scalaOrg % "scala-reflect" % ver,
      scalaOrg % "scala-compiler" % ver,
      "org.scalatest" % "scalatest_2.11" % "2.1.5" % "test",
      "junit" % "junit" % "4.11" % "test" // we need JUnit explicitly
  )))

  // modules
  lazy val _yinyang      = Project(id = "root",           base = file(".")                   , settings = Project.defaultSettings ++ Seq(publishArtifact := false)) aggregate (yinyang, yy_core, yy_paradise, example_dsls, backend, generator)
  lazy val yy_core       = Project(id = "yy-core",        base = file("components/core")     , settings = defaults ++ Seq(name := "yy-core"))
  lazy val yy_paradise   = Project(id = "yy-paradise",    base = file("components/paradise") , settings = defaults ++ paradise ++ Seq(name := "yy-paradise")) dependsOn(yy_core)
  lazy val yinyang       = Project(id = "yin-yang",       base = file("components/yin-yang") , settings = defaults ++ Seq(name := "yin-yang")) dependsOn(yy_core)
  lazy val backend       = Project(id = "backend",        base = file("components/backend")  , settings = defaults ++ Seq(name := "yy-backend")) dependsOn (yinyang)
  lazy val generator     = Project(id = "generator",      base = file("components/generator"), settings = defaults ++ Seq(name := "yy-generator")) dependsOn (yinyang)
  lazy val example_dsls  = Project(id = "example-dsls",   base = file("components/dsls")     , settings = defaults) dependsOn(yinyang)

  lazy val defaults = projectSettings ++ scalaSettings ++ formatSettings ++ libraryDeps ++ Seq(
    resolvers +=  "OSSH" at "https://oss.sonatype.org/content/groups/public",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    // paths - so we don't need to have src/main/scala ... just src/ test/ and resources/
    scalaSource in Compile <<= baseDirectory(_ / "src"),
    scalaSource in Test <<= baseDirectory(_ / "test"),
    resourceDirectory in Compile <<= baseDirectory(_ / "resources"),
    // sbteclipse needs some info on source directories:
    unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_)),
    unmanagedSourceDirectories in Test <<= (scalaSource in Test)(Seq(_)),
    parallelExecution in Test := false,
    incOptions := incOptions.value.withNameHashing(true)
  )

  // add the macro paradise compiler plugin
  lazy val paradise = Seq(
    addCompilerPlugin("org.scalamacros" % "paradise_2.11.0" % "2.0.0"),
    scalacOptions := defaultScalacOptions
  )


  lazy val website = Seq(site.settings,
    ghpages.settings,
    site.includeScaladoc(),
    site.jekyllSupport(),
    git.remoteRepo := "git@github.com:vjovanov/yin-yang.git",
    includeFilter in Jekyll := ("*.html" | "*.png" | "*.js" | "*.css" | "CNAME")
  )

  lazy val publishing = Seq(
    // for integration testing against scala snapshots
    resolvers += Resolver.sonatypeRepo("snapshots"),
   // so we don't have to wait for maven central synch
   resolvers += Resolver.sonatypeRepo("releases"),
   // publishing
   credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
   // If we want on maven central, we need to be in maven style.
   publishMavenStyle := true,
   publishArtifact in Test := false,
   // The Nexus repo we're publishing to.
   publishTo := Some(
     if (version.value.trim.endsWith("SNAPSHOT")) Resolver.sonatypeRepo("snapshots")
     else Opts.resolver.sonatypeStaging
   ),
   // Maven central cannot allow other repos.  We're ok here because the artifacts we
   // we use externally are *optional* dependencies.
   pomIncludeRepository := { _ => false },
   pomExtra := (
     <developers>
       <developer>
         <id>vjovanov</id>
         <name>Vojin Jovanovic</name>
         <url>http://www.vjovanov.com/</url>
       </developer>
     </developers>
   )
  )

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
  lazy val defaultScalacOptions = Seq("-deprecation", "-feature", "-language:higherKinds", "-language:implicitConversions")
  lazy val scalaOrg = "org.scala-lang"
}
