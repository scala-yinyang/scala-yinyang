package benchmarks

import java.io._

import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._
import scala.io._
import scala.tools.nsc.interpreter.AbstractFileClassLoader

object TypeCheckingBenchmark {
  var compiler: Global = _
  var reporter: ConsoleReporter = _

  def setupCompiler() = {
    /*
      output = new ByteArrayOutputStream()
      val writer = new PrintWriter(new OutputStreamWriter(output))
    */
    val settings = new Settings()

    settings.classpath.value = this.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader ⇒ ctx.getURLs.map(_.getPath).mkString(":")
      case _                            ⇒ System.getProperty("java.class.path")
    }
    settings.bootclasspath.value = Predef.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader ⇒ ctx.getURLs.map(_.getPath).mkString(":")
      case _                            ⇒ System.getProperty("sun.boot.class.path")
    }
    settings.encoding.value = "UTF-8"
    settings.outdir.value = "."
    settings.extdirs.value = ""

    reporter = new ConsoleReporter(settings, null, new PrintWriter(System.out)) //writer
    compiler = new Global(settings, reporter)
  }

  def resetCompiler(): Unit = {
    // TODO ask eugene
    reporter.reset
  }

  var compileCount: Int = 0
  def prependPackage(lines: Seq[String]): String = {
    compileCount += 1
    s"package bench$compileCount\n" + lines.mkString("\n")
  }

  def main(args: Array[String]) = {
    if (args.length != 6) throw new Exception("Read the source luc!! Wrong number of arguments!")
    val fileName = args(0)
    val startLine = args(1).toInt
    val endLine = args(2).toInt
    val errorLine = args(3)
    val warm = args(4).toBoolean
    val out = args(5)

    // initialize the scala compiler
    setupCompiler()

    val fw = new FileWriter(out, true)
    def report(time: Long): Unit =
      fw.write(time + " ")

    val comp = compiler
    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)

    val lines = Source.fromFile(fileName).getLines().toSeq
    if (warm) {
      for (i ← 0 until 10) {
        val run = new comp.Run
        val source = prependPackage(lines)
        val st = System.currentTimeMillis()
        // time this with scala test and without
        run.compileSources(scala.List(new util.BatchSourceFile("<stdin>", source)))
        reporter.printSummary()
        // end of benchmark
        println(s"Warmup round $i: ${System.currentTimeMillis() - st}")

        resetCompiler()
      }
    }

    for (i ← startLine until endLine) {

      val (before, after) = lines.splitAt(i)
      val source = prependPackage((before :+ errorLine) ++ after)

      if (!warm) {
        setupCompiler()
      }
      Runtime.getRuntime().gc()
      val run = new comp.Run
      val st = System.currentTimeMillis()
      // time this with scala test and without
      run.compileSources(scala.List(new util.BatchSourceFile("<stdin>", source.toString)))
      reporter.printSummary()
      // end of benchmark
      report((System.currentTimeMillis() - st))

      resetCompiler()

    }
    fw.close()
  }

}