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
    version              := "0.2.0-SNAPSHOT",
    organization         := "ch.epfl.lamp",
    licenses             := Seq("New BSD" ->
      url("https://raw.githubusercontent.com/scala-yinyang/scala-yinyang/master/LICENCE")),
    homepage             := Some(url("https://github.com/scala-yinyang/scala-yinyang")),
    organizationHomepage := Some(url("http://lamp.epfl.ch")),
    scmInfo              := Some(ScmInfo(
      url("https://github.com/scala-yinyang/scala-yinyang.git"),
      "scm:git:git://github.com/scala-yinyang/scala-yinyang.git"))
  )

  lazy val scalaSettings = Defaults.defaultSettings ++ Seq(
    scalaOrganization    := scalaOrg,
    scalaVersion         := "2.11.7",
    crossScalaVersions := Seq("2.11.0", "2.11.1", "2.11.2", "2.11.3", "2.11.4", "2.11.5", "2.11.6", "2.11.7"),
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
  lazy val _yinyang      = Project(id = "root",             base = file(".")                   , settings = Project.defaultSettings ++ Seq(publishArtifact := false)) aggregate (yinyang, yy_core, yy_paradise, examples)
  lazy val yy_core       = Project(id = "core",             base = file("components/core")     , settings = defaults ++ Seq(name := "yinyang-core"))
  lazy val yy_paradise   = Project(id = "paradise",         base = file("components/paradise") , settings = defaults ++ paradise ++ Seq(name := "yinyang-paradise")) dependsOn(yy_core)
  lazy val yinyang       = Project(id = "yinyang",          base = file("components/yin-yang") , settings = defaults ++ Seq(name := "scala-yinyang")) dependsOn(yy_core)
  lazy val examples      = Project(id = "examples",         base = file("components/examples") , settings = defaults ++ Seq(publishArtifact := false)) dependsOn(yinyang)

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
    libraryDependencies +=
      compilerPlugin("org.scalamacros" % "paradise" %  "2.1.0-M5" cross CrossVersion.full),
    scalacOptions := defaultScalacOptions
  )

  lazy val website = Seq(site.settings,
    ghpages.settings,
    site.includeScaladoc(),
    site.jekyllSupport(),
    git.remoteRepo := "git@github.com:scala-yinyang/scala-yinyang.git",
    includeFilter in Jekyll := ("*.html" | "*.png" | "*.js" | "*.css" | "CNAME")
  )

  lazy val publishing = Seq(
    // for integration testing against scala snapshots
    resolvers += Resolver.sonatypeRepo("snapshots"),
    // so we don't have to wait for maven central synch
    resolvers += Resolver.sonatypeRepo("releases"),
    // If we want on maven central, we need to be in maven style.
    publishMavenStyle := true,
    publishArtifact in Test := false,
    // The Nexus repo we're publishing to.
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
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
        <developer>
          <id>amirsh</id>
          <name>Amir Shaikhha</name>
          <url>http://people.epfl.ch/amir.shaikhha</url>
        </developer>
        <developer>
          <id>sstucki</id>
          <name>Sandro Stucki</name>
          <url>http://people.epfl.ch/sandro.stucki</url>
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
  lazy val defaultScalacOptions = Seq(
    "-deprecation",
    "-feature",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-Xexperimental"
  )

  lazy val scalaOrg = "org.scala-lang"
}
