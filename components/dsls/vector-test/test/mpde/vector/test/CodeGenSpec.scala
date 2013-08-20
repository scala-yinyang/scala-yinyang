package mpde.vector.test

import org.scalatest._
import dsl.print._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import reflect.runtime.universe._
import java.io.{ PrintStream, ByteArrayOutputStream }

@RunWith(classOf[JUnitRunner])
class CodeGenSpec extends FlatSpec with ShouldMatchers {

  def captureOutput(func: => Unit): String = {
    val bstream = new java.io.ByteArrayOutputStream()
    Console.withOut(bstream)(Console.withErr(bstream)(func))
    bstream.toString
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
    if (print) {
      scala.Predef.println(output)
    }
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
    val comp2 = YYStorage.getCompileTimeCompileCount() - comp
    val run2 = YYStorage.getRuntimeCompileCount() - run
    assert(comp2 == compileTime && run2 == runtime,
      s"$dlsType DSL compilation counts don't agree, should be $compileTime at compile time " +
        s"and $runtime at runtime, but was $comp2 and $run2.")
  }

  def evenOdd(i: Int) = if (i % 2 == 0) s"Even: $i " else s"Odd: $i "
  def stringEvenOdd(l: List[Int]): String = l.map(evenOdd).mkString
  def stringEvenOdd2(l1: List[Int], l2: List[Int]): String = l1.zip(l2).map(t => evenOdd(t._1) + evenOdd(t._2)).mkString

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
        liftVarTypeStab10Print {
          reqStaticPrint(i)
        }
      }, "reqStatic", "Even: 0 Odd: 1 Even: 2 Odd: 3 ")
    checkCounts(0, 2, () =>
      for (i ← List(0, 1, 2, 3)) {
        liftVarTypeStab10Print {
          reqDynamicPrint(i)
        }
      }, "reqDynamicPrint", "Even: 0 Odd: 1 Even: 2 Odd: 3 ")
  }

  "Optional VarTypes" should "have correct initial stability" in {
    checkCounts(0, 2, () =>
      for (i ← List(0, 1)) {
        liftVarTypeStab10Print {
          optionalStaticPrint(i)
        }
      }, "optionalStatic", "Even: 0 Odd: 1 ")
    checkCounts(0, 1, () =>
      for (i ← List(0, 1)) {
        liftVarTypeInitiallyUnstableStab10Print {
          optionalStaticPrint(i)
        }
      }, "optionalStaticInitiallyUnstable", "Even: 0 Odd: 1 ")
    checkCounts(0, 1, () =>
      for (i ← List(0, 2)) {
        liftVarTypeStab10Print {
          optionalDynamicPrint(i)
        }
      }, "optionalDynamic stable", "Even: 0 Even: 2 ")
    checkCounts(0, 2, () =>
      for (i ← List(0, 1)) {
        liftVarTypeStab10Print {
          optionalDynamicPrint(i)
        }
      }, "optionalDynamic stable -> unstable", "Even: 0 Odd: 1 ")
    checkCounts(0, 1, () =>
      for (i ← List(0, 2)) {
        liftVarTypeInitiallyUnstableStab10Print {
          optionalDynamicPrint(i)
        }
      }, "optionalDynamic unstable", "Even: 0 Even: 2 ")
    checkCounts(0, 1, () =>
      for (i ← List(0, 1)) {
        liftVarTypeInitiallyUnstableStab10Print {
          optionalDynamicPrint(i)
        }
      }, "optionalDynamic unstable", "Even: 0 Odd: 1 ")
  }

  "Optional VarTypes" should "get promoted" in {
    val list0012 = List(0, 0, 1, 2)

    val list0012_3x13_456 = list0012 ::: List.fill(13)(3) ::: List(4, 5, 6)
    val list0x15_1x5 = List.fill(15)(0) ::: List.fill(5)(1)

    checkCounts(0, 2, () =>
      for (i ← list0012) {
        liftVarTypeStab10Print {
          optionalStaticPrint(i)
        }
      }, "stable -> unstable", stringEvenOdd(list0012))
    checkCounts(0, 2, () =>
      for (i ← list0012_3x13_456) {
        liftVarTypeStab10Print {
          optionalStaticPrint(i)
        }
      }, "stable -> unstable (no promote w/o recomp)", stringEvenOdd(list0012_3x13_456))
    checkCounts(0, 4, () =>
      for ((i, j) ← (list0012_3x13_456 zip list0x15_1x5)) {
        liftVarTypeStab10Print {
          optionalStaticPrint(i)
          reqStaticPrint(j)
        }
      }, "stable -> unstable -> stable -> unstable", stringEvenOdd2(list0012_3x13_456, list0x15_1x5))
  }

  they should "inherit unstable counts" in {
    val list0_12x1 = 0 :: List.fill(12)(1)
    val list0_12x1_2 = 0 :: List.fill(12)(1) ::: List(2)
    val list0to10_4x11 = (0 to 11).toList ::: List.fill(3)(11)

    val list0_4x1_2_7x1_3 = 0 :: List.fill(4)(1) ::: List(2) ::: List.fill(8)(1) ::: List(3)
    val list5x0_2_6x0_2x3 = List.fill(5)(0) ::: List(2) ::: List.fill(7)(0) ::: List.fill(2)(3)

    checkCounts(0, 12, () =>
      for ((i, j) ← (list0_12x1 zip list0to10_4x11)) {
        liftVarTypeStab10Print {
          optionalStaticPrint(i)
          reqStaticPrint(j)
        }
      }, "on recompile inherit unstable count from prev. head 1", stringEvenOdd2(list0_12x1, list0to10_4x11))
    checkCounts(0, 13, () =>
      for ((i, j) ← (list0_12x1_2 zip list0to10_4x11)) {
        liftVarTypeStab10Print {
          optionalStaticPrint(i)
          reqStaticPrint(j)
        }
      }, "on recompile inherit unstable count from prev. head 2", stringEvenOdd2(list0_12x1_2, list0to10_4x11))
    checkCounts(0, 5, () =>
      for ((i, j) ← (list0_4x1_2_7x1_3 zip list5x0_2_6x0_2x3)) {
        liftVarTypeStab10Print {
          optionalStaticPrint(i)
          reqStaticPrint(j)
        }
      }, "keep counts per variant", stringEvenOdd2(list0_4x1_2_7x1_3, list5x0_2_6x0_2x3))
  }
}
