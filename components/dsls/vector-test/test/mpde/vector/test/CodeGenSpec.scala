package mpde.vector.test

import org.scalatest._
import dsl.print._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import reflect.runtime.universe._
import java.io.{ PrintStream, ByteArrayOutputStream }

@RunWith(classOf[JUnitRunner])
class CodeGenSpec extends FlatSpec with ShouldMatchers {

  // Set this to true if output capturing and asserting should be temporarily
  // disabled, so that all output is directly printed to the console.
  val noOutputAssertFlag = false
  // Set this to true if all output should be printed immediately to the console.
  // This disables output capturing and asserting as well.
  val printAllOutputFlag = false
  // Set this to true temporarily if compilation counts shouldn't be checked.
  val noCompilationCountAssertFlag = false
  // Set this to true temporarily if guard check counts shouldn't be checked.
  val noGuardCountAssertFlag = false
  // Set this to 1 for a summary of guard execution times, 2 for details, 3 for
  // full output of all runs
  val guardCountPrintDetails = 1

  if (noOutputAssertFlag)
    scala.Predef.println("==== WARING! noOutputAssertFlag is set to true! ====")
  if (printAllOutputFlag)
    scala.Predef.println("==== WARING! printAllOutputFlag is set to true! ====")
  if (noCompilationCountAssertFlag)
    scala.Predef.println("==== WARING! noCompilationCountAssertFlag is set to true! ====")
  if (noGuardCountAssertFlag)
    scala.Predef.println("==== WARING! noGuardCountAssertFlag is set to true! ====")

  def captureOutput(func: => Unit): String = {
    if (printAllOutputFlag) {
      func
      "debug"
    } else {
      val bstream = new java.io.ByteArrayOutputStream()
      Console.withOut(bstream)(Console.withErr(bstream)(func))
      bstream.toString
    }
  }

  def checkCounts(compileTimeCompileCount: Int, runtimeCompileCount: Int, guardCheckCount: Int,
                  block: () => Unit, dlsType: String, expectedOutput: String): Unit = {
    checkCounts(compileTimeCompileCount, runtimeCompileCount, guardCheckCount, block,
      dlsType, Some(expectedOutput), false)
  }
  def checkCounts(compileTimeCompileCount: Int, runtimeCompileCount: Int, guardCheckCount: Int,
                  block: () => Unit, dlsType: String, expectedOutput: Option[String] = None,
                  print: Boolean = true): Unit = {
    import ch.epfl.yinyang.runtime.YYStorage
    def nanoToMicroSec(t: Pair[Long, Int]): Pair[Long, Int] = (t._1 / 1000L, t._2)

    val comp = YYStorage.getCompileTimeCompileCount()
    var run = YYStorage.getRuntimeCompileCount()
    var (guardTime, guardCount) = nanoToMicroSec(YYStorage.getCheckTimeCount())
    var hashTime = YYStorage.getHashLookupTime() / 1000L

    // Check output
    val output = captureOutput(block())
    if (!noOutputAssertFlag && !printAllOutputFlag) {
      if (print) {
        scala.Predef.println(output)
      }
      expectedOutput map { exp =>
        assert(exp == output, {
          val prefix = (output, exp).zipped.takeWhile(t => t._1 == t._2).map(_._1).mkString
          val suffix = (output.drop(prefix.length).reverse, exp.drop(prefix.length).reverse).zipped.takeWhile(t => t._1 == t._2).map(_._1).mkString.reverse
          val diffExp = exp.drop(prefix.length).dropRight(suffix.length)
          val diffAct = output.drop(prefix.length).dropRight(suffix.length)
          s"$dlsType: DSL output doesn't match expected output.\nExpected: $exp\nActual: $output\n" +
            s"Common prefix: $prefix\nDiff:\n- Expected: $diffExp\n- Actual: $diffAct\n" +
            s"Common suffix: $suffix\n"
        })
      }
    }

    // Check compilation counts
    var run2 = YYStorage.getRuntimeCompileCount() - run
    if (!noCompilationCountAssertFlag) {
      val comp2 = YYStorage.getCompileTimeCompileCount() - comp
      assert(comp2 == compileTimeCompileCount && run2 == runtimeCompileCount,
        s"$dlsType: DSL compilation counts don't agree, should be $compileTimeCompileCount at compile time " +
          s"and $runtimeCompileCount at runtime, but was $comp2 and $run2.")
    }

    // Check guard execution counts
    if (!noGuardCountAssertFlag) {
      var (time, count) = nanoToMicroSec(YYStorage.getCheckTimeCount())
      var hashTime2 = YYStorage.getHashLookupTime() / 1000L
      assert(count - guardCount == guardCheckCount, s"$dlsType: guardCheckCounts don't agree, should be $guardCheckCount, but was ${count - guardCount}.")

      // Instrumentation to compute and print minimum guard execution time over 6 runs
      if (runtimeCompileCount != 0) {
        if (guardCountPrintDetails > 1) {
          scala.Predef.printf(" --t: %8dµs, dt: %8dµs, c: %3d, dc: %3d, dt/dc: %6dµs, ct: %2d, rt: %2d, ht/dc: %5dµs\n", time, time - guardTime, count, count - guardCount,
            (time - guardTime) / (count - guardCount), compileTimeCompileCount, runtimeCompileCount,
            (hashTime2 - hashTime) / (count - guardCount))
        }
        var minGuardTime = ((time - guardTime) / (count - guardCount), run2, (hashTime2 - hashTime) / (count - guardCount))
        for (i <- 0 until 5) {
          hashTime = hashTime2
          guardTime = time
          guardCount = count
          run = YYStorage.getRuntimeCompileCount()
          val out = captureOutput(block())
          if (guardCountPrintDetails > 2) {
            println(out)
          }
          ({ t: Pair[Long, Int] => time = t._1; count = t._2 })(nanoToMicroSec(YYStorage.getCheckTimeCount()))
          hashTime2 = YYStorage.getHashLookupTime() / 1000L
          if (guardCountPrintDetails > 1) {
            scala.Predef.printf("---t: %8dµs, dt: %8dµs, c: %3d, dc: %3d, dt/dc: %6dµs, ct: %2d, rt: %2d, ht/dc: %5dµs\n", time, time - guardTime, count, count - guardCount,
              (time - guardTime) / (count - guardCount), compileTimeCompileCount, runtimeCompileCount,
              (hashTime2 - hashTime) / (count - guardCount))
          }
          if ((time - guardTime) / (count - guardCount) < minGuardTime._1) {
            minGuardTime = ((time - guardTime) / (count - guardCount), YYStorage.getRuntimeCompileCount() - run,
              (hashTime2 - hashTime) / (count - guardCount))
          }
        }
        if (guardCountPrintDetails > 0) {
          scala.Predef.printf( /* "--Min. execution time in microseconds for guard check: " */
            "%5dµs (%2d checks, %2d initial compilCount, %2d minimal compilCount, ht/dc: %2dµs, hash/check: %3.0f%%)\n",
            minGuardTime._1, count - guardCount, runtimeCompileCount, minGuardTime._2, minGuardTime._3,
            100.0 * minGuardTime._3 / minGuardTime._1)
        }
      }
    }
  }

  // Guard checking time statistics (references in Array[Pair])
  //
  // Compile time compiled DSLs are ignored since they take 0µs (no guards).
  //
  // Numbers are microseconds per dsl invocation (lift {...}). They are the
  // minimum of 6 invocations, so for most DSLs the cache already contains
  // all variants (compilation count of 0 for the minimal run). The cache size
  // is 3, but even if a DSL is initially compiled more than 3 times, one of
  // the variants might also cover an existing evicted variant through unstable
  // variables, so later invocations don't need to recompile (case a).
  //
  // The times marked with b contain the time for recompilation.
  //
  //    53µs ( 2 checks,  2 initial compilCount,  0 minimal compilCount)
  //    59µs ( 3 checks,  2 initial compilCount,  0 minimal compilCount)
  //    30µs ( 1 checks,  1 initial compilCount,  0 minimal compilCount)
  //    17µs ( 3 checks,  1 initial compilCount,  0 minimal compilCount)
  //    83µs ( 3 checks,  3 initial compilCount,  0 minimal compilCount)
  //    31µs ( 4 checks,  2 initial compilCount,  0 minimal compilCount)
  //    30µs ( 4 checks,  2 initial compilCount,  0 minimal compilCount)
  //    19µs ( 1 checks,  1 initial compilCount,  0 minimal compilCount)
  //    45µs ( 4 checks,  2 initial compilCount,  0 minimal compilCount)
  // 20970µs ( 4 checks,  4 initial compilCount,  4 minimal compilCount) <--- (b)
  //    48µs ( 4 checks,  2 initial compilCount,  0 minimal compilCount)
  //    19µs ( 2 checks,  2 initial compilCount,  0 minimal compilCount)
  //    17µs ( 2 checks,  1 initial compilCount,  0 minimal compilCount)
  //    17µs ( 2 checks,  1 initial compilCount,  0 minimal compilCount)
  //    23µs ( 2 checks,  2 initial compilCount,  0 minimal compilCount)
  //    17µs ( 2 checks,  1 initial compilCount,  0 minimal compilCount)
  //    24µs ( 2 checks,  1 initial compilCount,  0 minimal compilCount)
  //    18µs ( 4 checks,  2 initial compilCount,  0 minimal compilCount)
  //    16µs (20 checks,  2 initial compilCount,  0 minimal compilCount)
  //    23µs (20 checks,  4 initial compilCount,  0 minimal compilCount) <- (a)
  // 43951µs (13 checks, 12 initial compilCount, 12 minimal compilCount) <--- (b)
  // 25966µs (14 checks, 13 initial compilCount, 13 minimal compilCount) <--- (b)
  //    28µs (15 checks,  5 initial compilCount,  0 minimal compilCount) <- (a)
  //    39µs ( 7 checks,  3 initial compilCount,  0 minimal compilCount)
  //  5969µs ( 7 checks,  6 initial compilCount,  4 minimal compilCount) <--- (b)

  // Generated caches
  //    73?s ( 2 checks,  2 initial compilCount,  0 minimal compilCount, ht/dc:  2?s, hash/check:   3%)
  //    55?s ( 3 checks,  2 initial compilCount,  0 minimal compilCount, ht/dc:  1?s, hash/check:   2%)
  //    30?s ( 1 checks,  1 initial compilCount,  0 minimal compilCount, ht/dc:  2?s, hash/check:   7%)
  //    16?s ( 3 checks,  1 initial compilCount,  0 minimal compilCount, ht/dc:  1?s, hash/check:   6%)
  //    71?s ( 3 checks,  3 initial compilCount,  0 minimal compilCount, ht/dc:  1?s, hash/check:   1%)
  //    55?s ( 4 checks,  2 initial compilCount,  0 minimal compilCount, ht/dc:  2?s, hash/check:   4%)
  //    34?s ( 4 checks,  2 initial compilCount,  0 minimal compilCount, ht/dc:  1?s, hash/check:   3%)
  //    18?s ( 1 checks,  1 initial compilCount,  0 minimal compilCount, ht/dc:  1?s, hash/check:   6%)
  //    51?s ( 4 checks,  2 initial compilCount,  0 minimal compilCount, ht/dc:  1?s, hash/check:   2%)
  // 19767?s ( 4 checks,  4 initial compilCount,  4 minimal compilCount, ht/dc:  4?s, hash/check:   0%)
  //    70?s ( 4 checks,  2 initial compilCount,  0 minimal compilCount, ht/dc:  1?s, hash/check:   1%)
  //    32?s ( 2 checks,  2 initial compilCount,  0 minimal compilCount, ht/dc:  2?s, hash/check:   6%)
  //    27?s ( 2 checks,  1 initial compilCount,  0 minimal compilCount, ht/dc:  2?s, hash/check:   7%)
  //    17?s ( 2 checks,  1 initial compilCount,  0 minimal compilCount, ht/dc:  1?s, hash/check:   6%)
  //    20?s ( 2 checks,  2 initial compilCount,  0 minimal compilCount, ht/dc:  1?s, hash/check:   5%)
  //    23?s ( 2 checks,  1 initial compilCount,  0 minimal compilCount, ht/dc:  1?s, hash/check:   4%)
  //    19?s ( 2 checks,  1 initial compilCount,  0 minimal compilCount, ht/dc:  1?s, hash/check:   5%)
  //    27?s ( 4 checks,  2 initial compilCount,  0 minimal compilCount, ht/dc:  1?s, hash/check:   4%)
  //    19?s (20 checks,  2 initial compilCount,  0 minimal compilCount, ht/dc:  1?s, hash/check:   5%)
  //    33?s (20 checks,  4 initial compilCount,  0 minimal compilCount, ht/dc:  1?s, hash/check:   3%)
  // 38510?s (13 checks, 12 initial compilCount, 12 minimal compilCount, ht/dc:  4?s, hash/check:   0%)
  // 29642?s (14 checks, 13 initial compilCount, 13 minimal compilCount, ht/dc:  4?s, hash/check:   0%)
  //    33?s (15 checks,  5 initial compilCount,  0 minimal compilCount, ht/dc:  1?s, hash/check:   3%)
  //    50?s ( 7 checks,  3 initial compilCount,  0 minimal compilCount, ht/dc:  1?s, hash/check:   2%)
  //  5708?s ( 7 checks,  6 initial compilCount,  4 minimal compilCount, ht/dc:  2?s, hash/check:   0%)

  def evenOdd(i: Int) = if (i % 2 == 0) s"Even: $i " else s"Odd: $i "
  def stringEvenOdd(l: List[Int]): String = l.map(evenOdd).mkString
  def stringEvenOdd2(l1: List[Int], l2: List[Int]): String = l1.zip(l2).map(t => evenOdd(t._1) + evenOdd(t._2)).mkString

  "Eval test" should "work" in {
    checkCounts(1, 0, 0, () => {
      val y = liftUnstagedPrint {
        1
      }
    }, "unstaged", "")
    checkCounts(1, 0, 0, () => {
      val y = liftOptimizedPrint {
        1
      }
    }, "optimized", "")
    checkCounts(1, 0, 0, () => {
      val y = liftStagedPrint {
        1
      }
    }, "staged", "")
  }

  "Changing values from outside of DSL scope" should "change" in {
    checkCounts(1, 0, 0, () => {
      for (i ← 0 to 1) {
        val j = liftUnstagedPrint {
          i
        }
        assert(j == i, s"Value $j didn't change to $i (unstaged)")
      }
    }, "unstaged", "")
    checkCounts(1, 0, 0, () => {
      for (i ← 0 to 1) {
        val j = liftOptimizedPrint {
          i
        }
        assert(j == i, s"Value $j didn't change to $i (optimized = unstaged)")
      }
    }, "optimized", "")
    checkCounts(0, 2, 2, () => {
      for (i ← 0 to 1) {
        val j = liftStagedPrint {
          i
        }
        assert(j == i, s"Value $j didn't change to $i (staged)")
      }
    }, "staged", "")
  }

  "Guarded values" should "be updated" in {
    checkCounts(0, 2, 3, () => {
      for (i ← List(0, 1, 1)) {
        val j = liftStagedPrint {
          i
        }
      }
    }, "staged", "")
  }

  "Static code staging" should "compile at compile time" in {
    checkCounts(1, 0, 0, () => {
      val v = liftUnstagedPrint {
        val x = 1
        val y = 2
        val z = 4
        print(x + y + z)
        x + y + z
      }
      assert(v == 7, "unstaged: computation should yield 7")
    }, "unstaged", "7 ")
    checkCounts(1, 0, 0, () => {
      val v = liftOptimizedPrint {
        val x = 1
        val y = 2
        val z = 4
        print(x + y + z)
        x + y + z
      }
      assert(v == 7, "optimized: computation should yield 7")
    }, "optimized", "7 ")
    checkCounts(1, 0, 0, () => {
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
    checkCounts(1, 0, 0, () => {
      val x = 1
      val y = 2
      assert(
        liftUnstagedPrint {
          val z = 4
          print(z + x + y)
          z + x + y
        } == 7, "unstaged: computation should yield 7")
    }, "unstaged", "7 ")
    checkCounts(1, 0, 0, () => {
      val x = 1
      val y = 2
      assert(
        liftOptimizedPrint {
          val z = 4
          print(z + x + y)
          z + x + y
        } == 7, "optimized: computation should yield 7")
    }, "optimized", "7 ")
    checkCounts(0, 1, 1, () => {
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
    checkCounts(1, 0, 0, () => {
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
    checkCounts(0, 1, 3, () => {
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
    checkCounts(0, 3, 3, () => {
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
    checkCounts(0, 2, 4, () => {
      for (i: Int <- 0 to 1) {
        for (j: Int <- 0 to 1) {
          liftOptimizedPrint {
            print(j)
            optimizingPrint(i)
          }
        }
      }
    }, "optimized", "0 optimizing on: 0 1 optimizing on: 0 0 optimizing on: 1 1 optimizing on: 1 ")
    checkCounts(0, 2, 4, () => {
      for (i <- List(0, 2, 1, 3)) {
        liftEvenOddOptimizedPrint {
          evenOddPrint(i)
        }
      }
    }, "even-odd-optimized", "Even: 0 Even: 2 Odd: 1 Odd: 3 ")
  }

  "Virtualization" should "work" in {
    checkCounts(1, 0, 0, () => {
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
    checkCounts(1, 0, 0, () => {
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
    checkCounts(0, 1, 1, () => {
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
    checkCounts(1, 0, 0, () =>
      assert(liftOptimizedPrint {
        optimizingPrint(3)
      }.getClass == ().getClass, "optimizingPrint didn't return Unit"),
      "optimized", "optimizing on: 3 ")

    checkCounts(2, 0, 0, () => {
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
    checkCounts(0, 2, 4, () =>
      for (i ← List(0, 1, 0, 1)) {
        val j = liftOptimizedPrint {
          optimizingPrint(i)
          i
        }
        assert(j == i, s"Value $j didn't change to $i (optimized)")
      }, "optimized", "optimizing on: 0 optimizing on: 1 optimizing on: 0 optimizing on: 1 ")
  }

  "Required VarTypes" should "work" in {
    checkCounts(0, 4, 4, () =>
      for (i ← List(0, 1, 2, 3)) {
        liftVarTypeStab10Print {
          reqStaticPrint(i)
        }
      }, "reqStatic", "Even: 0 Odd: 1 Even: 2 Odd: 3 ")
    checkCounts(0, 2, 4, () =>
      for (i ← List(0, 1, 2, 3)) {
        liftVarTypeStab10Print {
          reqDynamicPrint(i)
        }
      }, "reqDynamicPrint", "Even: 0 Odd: 1 Even: 2 Odd: 3 ")
  }

  "Optional VarTypes" should "have correct initial stability" in {
    checkCounts(0, 2, 2, () =>
      for (i ← List(0, 1)) {
        liftVarTypeStab10Print {
          optionalStaticPrint(i)
        }
      }, "optionalStatic", "Even: 0 Odd: 1 ")
    checkCounts(0, 1, 2, () =>
      for (i ← List(0, 1)) {
        liftVarTypeInitiallyUnstableStab10Print {
          optionalStaticPrint(i)
        }
      }, "optionalStaticInitiallyUnstable", "Even: 0 Odd: 1 ")
    checkCounts(0, 1, 2, () =>
      for (i ← List(0, 2)) {
        liftVarTypeStab10Print {
          optionalDynamicPrint(i)
        }
      }, "optionalDynamic stable", "Even: 0 Even: 2 ")
    checkCounts(0, 2, 2, () =>
      for (i ← List(0, 1)) {
        liftVarTypeStab10Print {
          optionalDynamicPrint(i)
        }
      }, "optionalDynamic stable -> unstable", "Even: 0 Odd: 1 ")
    checkCounts(0, 1, 2, () =>
      for (i ← List(0, 2)) {
        liftVarTypeInitiallyUnstableStab10Print {
          optionalDynamicPrint(i)
        }
      }, "optionalDynamic unstable", "Even: 0 Even: 2 ")
    checkCounts(0, 1, 2, () =>
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

    checkCounts(0, 2, list0012.length, () =>
      for (i ← list0012) {
        liftVarTypeStab10Print {
          optionalStaticPrint(i)
        }
      }, "stable -> unstable", stringEvenOdd(list0012))
    checkCounts(0, 2, list0012_3x13_456.length, () =>
      for (i ← list0012_3x13_456) {
        liftVarTypeStab10Print {
          optionalStaticPrint(i)
        }
      }, "stable -> unstable (no promote w/o recomp)", stringEvenOdd(list0012_3x13_456))
    checkCounts(0, 4, (list0012_3x13_456 zip list0x15_1x5).length, () =>
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

    checkCounts(0, 12, (list0_12x1 zip list0to10_4x11).length, () =>
      for ((i, j) ← (list0_12x1 zip list0to10_4x11)) {
        liftVarTypeStab10Print {
          optionalStaticPrint(i)
          reqStaticPrint(j)
        }
      }, "on recompile inherit unstable count from prev. head 1", stringEvenOdd2(list0_12x1, list0to10_4x11))

    /* TODO (VJ) why is this failing?
      checkCounts(0, 13, (list0_12x1_2 zip list0to10_4x11).length, () =>
      for ((i, j) ← (list0_12x1_2 zip list0to10_4x11)) {
        liftVarTypeStab10Print {
          optionalStaticPrint(i)
          reqStaticPrint(j)
        }
      }, "on recompile inherit unstable count from prev. head 2", stringEvenOdd2(list0_12x1_2, list0to10_4x11))

    checkCounts(0, 5, (list0_4x1_2_7x1_3 zip list5x0_2_6x0_2x3).length, () =>
      for ((i, j) ← (list0_4x1_2_7x1_3 zip list5x0_2_6x0_2x3)) {
        liftVarTypeStab10Print {
          optionalStaticPrint(i)
          reqStaticPrint(j)
        }
      }, "keep counts per variant", stringEvenOdd2(list0_4x1_2_7x1_3, list5x0_2_6x0_2x3))*/

  }

  "The code cache" should "have size 3" in {
    checkCounts(0, 3, 7, () =>
      for (i ← List(0, 1, 2, 0, 1, 2, 0)) {
        liftStagedPrint {
          i
        }
      }, "no recompile", "")
    checkCounts(0, 6, 7, () =>
      for (i ← List(0, 1, 2, 3, 0, 1, 3)) {
        liftStagedPrint {
          i
        }
      }, "recompile", "")
  }

  "VarTypes" should "compose correctly" in {
    checkCounts(0, 4, 4, () =>
      for (i ← List(0, 1, 2, 3)) {
        liftVarTypeStab10Print {
          reqStaticPrint(i)
          reqDynamicPrint(i)
        }
      }, "recompile", "Even: 0 Even: 0 Odd: 1 Odd: 1 Even: 2 Even: 2 Odd: 3 Odd: 3 ")
  }

  "Optional" should "be traversed" in {
    checkCounts(0, 2, 6, () =>
      for (j ← List(0, 1, 3)) {
        for (k ← List(0, 2)) {
          liftVarTypeStab10Print {
            optionalStaticPrint(j)
            reqDynamicPrint(k)
          }
        }
      }, "recompile", "Even: 0 Even: 0 Even: 0 Even: 2 Odd: 1 Even: 0 Odd: 1 Even: 2 Odd: 3 Even: 0 Odd: 3 Even: 2 ")
  }

  "Big CodeCache" should "work" in {
    checkCounts(0, 1, 1, () => {
      val a, b, c, d, e, f, g, gg, h, i, j, k, l, m, n: Int = 0
      liftVarTypeStab10Print {
        reqDynamicPrint(a)
        reqDynamicPrint(b)
        reqDynamicPrint(c)
        reqDynamicPrint(d)
        reqDynamicPrint(e)
        reqDynamicPrint(f)
        reqDynamicPrint(g)
        optionalStaticPrint(gg)
        reqDynamicPrint(h)
        reqDynamicPrint(i)
        reqDynamicPrint(j)
        reqDynamicPrint(k)
        reqDynamicPrint(l)
        reqDynamicPrint(m)
        optionalStaticPrint(m)
        optionalStaticPrint(n)

      }
    }, "big", "Even: 0 Even: 0 Even: 0 Even: 0 Even: 0 Even: 0 Even: 0 Even: 0 Even: 0 Even: 0 Even: 0 Even: 0 Even: 0 Even: 0 Even: 0 Even: 0 ")
  }
}
