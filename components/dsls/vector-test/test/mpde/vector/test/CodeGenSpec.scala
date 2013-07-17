package mpde.vector.test

import org.scalatest._
import dsl.print._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import reflect.runtime.universe._
import ch.epfl.yinyang.runtime.YYStorage

@RunWith(classOf[JUnitRunner])
class CodeGenSpec extends FlatSpec with ShouldMatchers {

  def checkCounts(compileTime: Int, runtime: Int, block: () => Unit): Unit = {
    val comp = YYStorage.getCompileTimeCompileCount()
    val run = YYStorage.getRuntimeCompileCount()
    block()
    val comp2 = YYStorage.getCompileTimeCompileCount() - comp
    val run2 = YYStorage.getRuntimeCompileCount() - run
    assert(comp2 == compileTime && run2 == runtime,
      s"DSL compilation counts don't agree, should be $compileTime at compile time " +
        s"and $runtime at runtime, but was $comp2 and $run2.")
  }

  "Eval test" should "work" in {
    checkCounts(1, 0, () => {
      val y = liftPrint {
        1
      }
    })
  }

  "Changing values from outside of DSL scope" should "change" in {
    checkCounts(1, 0, () => {
      for (i ← 0 to 2) {
        val j = liftPrint {
          i
        }
        assert(j == i, s"Value $j didn't change to $i")
      }
    })
  }

  "Static code staging" should "compile at compile time" in {
    checkCounts(1, 0, () => {
      val x = liftPrint {
        val x = 1
        val y = 2
        val z = 4
        println(x + y + z)
        x + y + z
      }
      assert(x == 7)
    })
  }

  "Dynamic code insertion" should "compile at compile time" in {
    checkCounts(1, 0, () => {
      val x = 1
      val y = 2

      assert(
        liftPrint {
          val z = 4
          println(z + x + y)
          z + x + y
        } == 7)
    })
  }

  "Compile time code generating" should "work" in {
    checkCounts(1, 0, () => {
      val y = 3
      assert(
        liftOptimizedPrint {
          val b = 0
          println(y)
          optimizingPrintln(b) // do not recompile
          1 + b
        } == 1)
    })
  }

  "Runtime code generating" should "not recompile" in {
    checkCounts(0, 1, () => {
      val y = 3
      for (i ← 0 to 2) {
        assert(
          liftOptimizedPrint {
            val b = 0
            println(b)
            optimizingPrintln(y)
            1 + b
          } == 1)
      }
    })
  }

  "Runtime code generating" should "recompile" in {
    checkCounts(0, 3, () => {
      for (i ← 0 to 2) {
        assert(
          liftOptimizedPrint {
            val b = 0
            println(b)
            optimizingPrintln(i)
            1 + b
          } == 1)
      }
    })
  }

  "Virtualization" should "work" in {
    checkCounts(1, 0, () => {
      val x = 1
      assert(
        liftPrint {
          val b = x
          val c = 1;
          b.hashCode();
          c != 1
          b.##
          c.hashCode
          b
        } == 1)
    })
  }
}
