package mpde.vector.test

import org.scalatest._
import dsl.print._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import reflect.runtime.universe._

@RunWith(classOf[JUnitRunner])
class CodeGenSpec extends FlatSpec with ShouldMatchers {

  def checkCounts(compileTime: Int, runtime: Int, block: () => Unit, dlsType: String): Unit = {
    import ch.epfl.yinyang.runtime.YYStorage

    val comp = YYStorage.getCompileTimeCompileCount()
    val run = YYStorage.getRuntimeCompileCount()
    block()
    val comp2 = YYStorage.getCompileTimeCompileCount() - comp
    val run2 = YYStorage.getRuntimeCompileCount() - run
    assert(comp2 == compileTime && run2 == runtime,
      s"$dlsType DSL compilation counts don't agree, should be $compileTime at compile time " +
        s"and $runtime at runtime, but was $comp2 and $run2.")
  }

  "Eval test" should "work" in {
    checkCounts(1, 0, () => {
      val y = liftUnstagedPrint {
        1
      }
    }, "unstaged")
    checkCounts(1, 0, () => {
      val y = liftOptimizedPrint {
        1
      }
    }, "optimized")
    checkCounts(1, 0, () => {
      val y = liftStagedPrint {
        1
      }
    }, "staged")
  }

  "Changing values from outside of DSL scope" should "change" in {
    checkCounts(1, 0, () => {
      for (i ← 0 to 1) {
        val j = liftUnstagedPrint {
          i
        }
        assert(j == i, s"Value $j didn't change to $i (unstaged)")
      }
    }, "unstaged")
    checkCounts(1, 0, () => {
      for (i ← 0 to 1) {
        val j = liftOptimizedPrint {
          i
        }
        assert(j == i, s"Value $j didn't change to $i (optimized = unstaged)")
      }
    }, "optimized")
    checkCounts(0, 2, () => {
      for (i ← 0 to 1) {
        val j = liftStagedPrint {
          i
        }
        assert(j == i, s"Value $j didn't change to $i (staged)")
      }
    }, "staged")
  }

  "Guarded values" should "be updated" in {
    checkCounts(0, 2, () => {
      for (i ← List(0, 1, 1)) {
        val j = liftStagedPrint {
          i
        }
      }
    }, "staged")
  }

  "Static code staging" should "compile at compile time" in {
    checkCounts(1, 0, () => {
      val v = liftUnstagedPrint {
        val x = 1
        val y = 2
        val z = 4
        println(x + y + z)
        x + y + z
      }
      assert(v == 7, "unstaged: computation should yield 7")
    }, "unstaged")
    checkCounts(1, 0, () => {
      val v = liftOptimizedPrint {
        val x = 1
        val y = 2
        val z = 4
        println(x + y + z)
        x + y + z
      }
      assert(v == 7, "optimized: computation should yield 7")
    }, "optimized")
    checkCounts(1, 0, () => {
      val v = liftStagedPrint {
        val x = 1
        val y = 2
        val z = 4
        println(x + y + z)
        x + y + z
      }
      assert(v == 7, "staged: computation should yield 7")
    }, "staged")
  }

  "Dynamic code insertion" should "work" in {
    checkCounts(1, 0, () => {
      val x = 1
      val y = 2
      assert(
        liftUnstagedPrint {
          val z = 4
          println(z + x + y)
          z + x + y
        } == 7, "unstaged: computation should yield 7")
    }, "unstaged")
    checkCounts(1, 0, () => {
      val x = 1
      val y = 2
      assert(
        liftOptimizedPrint {
          val z = 4
          println(z + x + y)
          z + x + y
        } == 7, "optimized: computation should yield 7")
    }, "optimized")
    checkCounts(0, 1, () => {
      val x = 1
      val y = 2
      assert(
        liftStagedPrint {
          val z = 4
          println(z + x + y)
          z + x + y
        } == 7, "staged: computation should yield 7")
    }, "staged")
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
    }, "optimized")
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
    }, "optimized")
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
    }, "optimized")
  }

  "Virtualization" should "work" in {
    checkCounts(1, 0, () => {
      val x = 1
      assert(
        liftUnstagedPrint {
          val b = x
          val c = 1;
          b.hashCode();
          c != 1
          b.##
          c.hashCode
          b
        } == 1, "unstaged should yield 1")
    }, "unstaged")
    checkCounts(1, 0, () => {
      val x = 1
      assert(
        liftOptimizedPrint {
          val b = x
          val c = 1;
          b.hashCode();
          c != 1
          b.##
          c.hashCode
          b
        } == 1, "optimized should yield 1")
    }, "optimized")
    checkCounts(0, 1, () => {
      val x = 1
      assert(
        liftStagedPrint {
          val b = x
          val c = 1;
          b.hashCode();
          c != 1
          b.##
          c.hashCode
          b
        } == 1, "staged should yield 1")
    }, "staged")
  }
}
