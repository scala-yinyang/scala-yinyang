package mpde.vector.test

import org.scalatest._
import dsl.print._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import reflect.runtime.universe._
import java.io.{ PrintStream, ByteArrayOutputStream }

@RunWith(classOf[JUnitRunner])
class CodeGenSpec extends FlatSpec with ShouldMatchers {

  // From FileDiffSuite.scala
  def captureOutput(func: => Unit): String = {
    val bstream = new ByteArrayOutputStream
    withOutput(new PrintStream(bstream))(func)
    bstream.toString
  }
  def withOutput(out: PrintStream)(func: => Unit): Unit = {
    val oldStdOut = System.out
    val oldStdErr = System.err
    try {
      System.setOut(out)
      System.setErr(out)
      Console.withOut(out)(Console.withErr(out)(func))
    } finally {
      out.flush()
      out.close()
      System.setOut(oldStdOut)
      System.setErr(oldStdErr)
    }
  }

  def checkCounts(compileTime: Int, runtime: Int, block: () => Unit, dlsType: String,
                  expectedOutput: String): Unit = {
    checkCounts(compileTime, runtime, block, dlsType, Some(expectedOutput), false)
  }
  def checkCounts(compileTime: Int, runtime: Int, block: () => Unit, dlsType: String,
                  expectedOutput: Option[String] = None, print: Boolean = true): Unit = {
    import ch.epfl.yinyang.runtime.YYStorage

    val comp = YYStorage.getCompileTimeCompileCount()
    val run = YYStorage.getRuntimeCompileCount()

    val output = captureOutput(block())
    expectedOutput map { exp =>
      assert(exp == output, {
        val prefix = (output, exp).zipped.takeWhile(t => t._1 == t._2).map(_._1).mkString
        val suffix = (output.reverse, exp.reverse).zipped.takeWhile(t => t._1 == t._2).map(_._1).mkString.reverse
        val diffExp = exp.drop(prefix.length).dropRight(suffix.length)
        val diffAct = output.drop(prefix.length).dropRight(suffix.length)
        s"DSL output doesn't match expected output.\nExpected: $exp\nActual: $output\n" +
          s"Common prefix: $prefix\nDiff:\n- Expected: $diffExp\n- Actual: $diffAct\n" +
          s"Common suffix: $suffix\n"
      })
    }
    if (print) {
      scala.Predef.println(output)
    }

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
    }, "unstaged", "")
    checkCounts(1, 0, () => {
      val y = liftOptimizedPrint {
        1
      }
    }, "optimized", "")
    checkCounts(1, 0, () => {
      val y = liftStagedPrint {
        1
      }
    }, "staged", "")
  }

  "Changing values from outside of DSL scope" should "change" in {
    checkCounts(1, 0, () => {
      for (i ← 0 to 1) {
        val j = liftUnstagedPrint {
          i
        }
        assert(j == i, s"Value $j didn't change to $i (unstaged)")
      }
    }, "unstaged", "")
    checkCounts(1, 0, () => {
      for (i ← 0 to 1) {
        val j = liftOptimizedPrint {
          i
        }
        assert(j == i, s"Value $j didn't change to $i (optimized = unstaged)")
      }
    }, "optimized", "")
    checkCounts(0, 2, () => {
      for (i ← 0 to 1) {
        val j = liftStagedPrint {
          i
        }
        assert(j == i, s"Value $j didn't change to $i (staged)")
      }
    }, "staged", "")
  }

  "Guarded values" should "be updated" in {
    checkCounts(0, 2, () => {
      for (i ← List(0, 1, 1)) {
        val j = liftStagedPrint {
          i
        }
      }
    }, "staged", "")
  }

  "Static code staging" should "compile at compile time" in {
    checkCounts(1, 0, () => {
      val v = liftUnstagedPrint {
        val x = 1
        val y = 2
        val z = 4
        print(x + y + z)
        x + y + z
      }
      assert(v == 7, "unstaged: computation should yield 7")
    }, "unstaged", "7 ")
    checkCounts(1, 0, () => {
      val v = liftOptimizedPrint {
        val x = 1
        val y = 2
        val z = 4
        print(x + y + z)
        x + y + z
      }
      assert(v == 7, "optimized: computation should yield 7")
    }, "optimized", "7 ")
    checkCounts(1, 0, () => {
      val v = liftStagedPrint {
        val x = 1
        val y = 2
        val z = 4
        print(x + y + z)
        x + y + z
      }
      assert(v == 7, "staged: computation should yield 7")
    }, "staged", "7 ")
  }

  "Dynamic code insertion" should "work" in {
    checkCounts(1, 0, () => {
      val x = 1
      val y = 2
      assert(
        liftUnstagedPrint {
          val z = 4
          print(z + x + y)
          z + x + y
        } == 7, "unstaged: computation should yield 7")
    }, "unstaged", "7 ")
    checkCounts(1, 0, () => {
      val x = 1
      val y = 2
      assert(
        liftOptimizedPrint {
          val z = 4
          print(z + x + y)
          z + x + y
        } == 7, "optimized: computation should yield 7")
    }, "optimized", "7 ")
    checkCounts(0, 1, () => {
      val x = 1
      val y = 2
      assert(
        liftStagedPrint {
          val z = 4
          print(z + x + y)
          z + x + y
        } == 7, "staged: computation should yield 7")
    }, "staged", "7 ")
  }

  "Compile time code generating" should "work" in {
    checkCounts(1, 0, () => {
      val y = 3
      assert(
        liftOptimizedPrint {
          val b = 0
          print(y)
          optimizingPrint(b) // do not recompile
          1 + b
        } == 1)
    }, "optimized", "3 optimizing on: 0 ")
  }

  "Runtime code generating" should "not recompile" in {
    checkCounts(0, 1, () => {
      val y = 3
      for (i ← 0 to 2) {
        assert(
          liftOptimizedPrint {
            val b = 0
            print(b)
            optimizingPrint(y)
            1 + b
          } == 1)
      }
    }, "optimized", "0 optimizing on: 3 0 optimizing on: 3 0 optimizing on: 3 ")
  }

  "Runtime code generating" should "recompile" in {
    checkCounts(0, 3, () => {
      for (i ← 0 to 2) {
        assert(
          liftOptimizedPrint {
            val b = 0
            print(b)
            optimizingPrint(i)
            1 + b
          } == 1)
      }
    }, "optimized", "0 optimizing on: 0 0 optimizing on: 1 0 optimizing on: 2 ")
  }

  "Runtime code generating" should "sometimes recompile" in {
    checkCounts(0, 2, () => {
      for (i: Int <- 0 to 1) {
        for (j: Int <- 0 to 1) {
          liftOptimizedPrint {
            print(j)
            optimizingPrint(i)
          }
        }
      }
    }, "optimized", "0 optimizing on: 0 1 optimizing on: 0 0 optimizing on: 1 1 optimizing on: 1 ")
    checkCounts(0, 2, () => {
      for (i <- List(0, 2, 1, 3)) {
        liftEvenOddOptimizedPrint {
          evenOddPrint(i)
        }
      }
    }, "even-odd-optimized", "Even: 0 Even: 2 Odd: 1 Odd: 3 ")
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
    }, "unstaged", "")
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
    }, "optimized", "")
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
    }, "staged", "")
  }

  "Return test" should "compile and work" in {
    checkCounts(1, 0, () =>
      assert(liftOptimizedPrint {
        optimizingPrint(3)
      }.getClass == ().getClass, "optimizingPrint didn't return Unit"),
      "optimized", "optimizing on: 3 ")

    checkCounts(2, 0, () => {
      assert(liftOptimizedPrint {
        true
      } == true)
      assert(liftReturningPrint {
        returningIncrementedPrint(1)
      } == 2, "returningIncrementedPrint didn't return 2")
    }, "returningIncremented", "inc: 1 ")

    // TODO nested statements with both effects and values that are computed
    // assert(liftReturningPrint {
    //   returningIncrementedPrint(returningIncrementedPrint(1))
    // } == 3, "NESTED returningIncrementedPrint didn't return 3")
  }

  "Runtime code generating" should "recompile only if NOT cached" in {
    checkCounts(0, 2, () =>
      for (i ← List(0, 1, 0, 1)) {
        val j = liftOptimizedPrint {
          optimizingPrint(i)
          i
        }
        assert(j == i, s"Value $j didn't change to $i (optimized)")
      }, "optimized", "optimizing on: 0 optimizing on: 1 optimizing on: 0 optimizing on: 1 ")
  }

  "Required VarTypes" should "work" in {
    checkCounts(0, 4, () =>
      for (i ← List(0, 1, 2, 3)) {
        liftVarTypePrint {
          reqStaticPrint(i)
        }
      }, "reqStatic", "Even: 0 Odd: 1 Even: 2 Odd: 3 ")
    checkCounts(0, 2, () =>
      for (i ← List(0, 1, 2, 3)) {
        liftVarTypePrint {
          reqDynamicPrint(i)
        }
      }, "reqDynamicPrint", "Even: 0 Odd: 1 Even: 2 Odd: 3 ")
  }

  "Optional VarTypes" should "have correct initial stability" in {
    checkCounts(0, 2, () =>
      for (i ← List(0, 1)) {
        liftVarTypePrint {
          optionalStaticPrint(i)
        }
      }, "optionalStatic", "Even: 0 Odd: 1 ")
    checkCounts(0, 1, () =>
      for (i ← List(0, 1)) {
        liftVarTypeInitiallyUnstablePrint {
          optionalStaticPrint(i)
        }
      }, "optionalStaticInitiallyUnstable", "Even: 0 Odd: 1 ")
    checkCounts(0, 1, () =>
      for (i ← List(0, 2)) {
        liftVarTypePrint {
          optionalDynamicPrint(i)
        }
      }, "optionalDynamic stable", "Even: 0 Even: 2 ")
    checkCounts(0, 2, () =>
      for (i ← List(0, 1)) {
        liftVarTypePrint {
          optionalDynamicPrint(i)
        }
      }, "optionalDynamic stable -> unstable", "Even: 0 Odd: 1 ")
    checkCounts(0, 1, () =>
      for (i ← List(0, 2)) {
        liftVarTypeInitiallyUnstablePrint {
          optionalDynamicPrint(i)
        }
      }, "optionalDynamic unstable", "Even: 0 Even: 2 ")
    checkCounts(0, 1, () =>
      for (i ← List(0, 1)) {
        liftVarTypeInitiallyUnstablePrint {
          optionalDynamicPrint(i)
        }
      }, "optionalDynamic unstable", "Even: 0 Odd: 1 ")
  }

  "Optional VarTypes" should "get promoted" in {
    val list1 = 0 :: List.fill(600)(1)
    val list300_250_50 = List.fill(300)(1) ::: List.fill(250)(2) ::: List.fill(50)(3)
    val list1_575_U = 0 :: List.fill(575)(1) ::: List.range(0, 25)

    def stringEvenOdd(l1: List[Int], l2: List[Int]): String = {
      def evenOdd(i: Int) = if (i % 2 == 0) s"Even: $i " else s"Odd: $i "
      l1.map(evenOdd).zip(l2.map(evenOdd)).foldLeft("")((s, t) => s + t._1 + t._2)
    }
    checkCounts(0, 4, () =>
      for ((i, j) ← (list1 zip list300_250_50)) {
        liftVarTypePrint {
          optionalStaticPrint(i)
          reqStaticPrint(j)
        }
      }, "stable -> unstable -> stable", stringEvenOdd(list1, list300_250_50))
    checkCounts(0, 5, () =>
      for ((i, j) ← (list1_575_U zip list300_250_50)) {
        liftVarTypePrint {
          optionalStaticPrint(i)
          reqStaticPrint(j)
        }
      }, "stable -> unstable -> stable -> unstable", stringEvenOdd(list1_575_U, list300_250_50))
  }
}
