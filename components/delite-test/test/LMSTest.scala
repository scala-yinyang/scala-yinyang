import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import lifted._
import shallow.optiml._

@RunWith(classOf[JUnitRunner])
class OptiMLSpec extends FlatSpec with ShouldMatchers {

  "A basic OptiML test should" should "rewire" in {
    val y = 1
    val x: Int = optiML {
      val x = y
      val z = 1 + x
      val k = z + 1
      k
    }
  }

}

class OptiGraphSpec extends FlatSpec with ShouldMatchers with LanguageOps {

  "A basic OptiGraph test should" should "rewire" in {
    val y = 1
    val x: Int = optiGraph {
      val x = y
      val z = 1 + x
      val k = z + 1
      k
    }
  }

  //This test works
  "test_graphOps" should "compile" in {
    val x: Any = optiGraphDebug {
      val g = Graph()
      val n1 = g.AddNode
      val n2 = g.AddNode
      val e1 = g.AddEdge(n1, n2)
      // repeated edge
      val e2 = g.AddEdge(n1, n2)
      val e3 = g.AddEdge(n2, n1)
      // self-edge
      val e4 = g.AddEdge(n1, n1)
      g.Freeze
      println("Test Graph")
      if (g.NumNodes != 2) {
        println("[FAIL] Wrong number of nodes. Expected value = 2, Actual value " + g.NumNodes)
      } else {
        println("[OK] Number of nodes is correct")
      }

      if (g.NumEdges != 4) {
        println("[FAIL] Wrong number of edges. Expected value = 4, Actual value " + g.NumNodes)
      } else {
        println("[OK] Number of edges is correct")
      }

      if (g.Node(n1.Id).Id != n1.Id) {
        println("[FAIL] Wrong node returned")
      } else {
        println("[OK] Correct node returned")
      }
      if (g.Edge(e1.Id).Id != e1.Id) {
        println("[FAIL] Wrong edge returned")
      } else {
        println("[OK] Correct edge returned")
      }

      val nodes = g.Nodes.toSet
      val edges = g.Edges.toSet
      if (nodes.Size != 2 || edges.Size != 4) {
        println("[FAIL] Wrong nodes/edges collection size")
      } else {
        println("[OK] Correct nodes/edges collection size")
      }
      if ((!nodes.Has(n1)) || (!nodes.Has(n2)) || (!edges.Has(e1)) || (!edges.Has(e2))
        || (!edges.Has(e3)) || (!edges.Has(e4))) {
        println("[FAIL] Wrong nodes/edges collection contents")
      } else {
        println("[OK] Correct nodes/edges collection contents")
      }

      //---------//

      val g2 = DGraph()
      val n3 = g2.AddNode
      val n4 = g2.AddNode
      val e5 = g2.AddEdge(n3, n4)
      val e6 = g2.AddEdge(n3, n4)
      val e7 = g2.AddEdge(n4, n3)
      val e8 = g2.AddEdge(n3, n3)
      val g_s = g2.Snapshot

      if (g_s.NumNodes != g2.NumNodes || g_s.NumEdges != g2.NumEdges) {
        println("[FAIL] Wrong snapshot graph size")
      } else {
        println("[OK] Correct snapshot graph size")
      }

      if (!((g_s.Node(0).Degree == 3 || g_s.Node(1).Degree == 3) &&
        (g_s.Node(0).Degree == 1 || g_s.Node(1).Degree == 1))) {
        println("[FAIL] Wrong snapshot graph node connections")
      } else {
        println("[OK] Correct snapshot graph node connections")
      }

      val g3 = UGraph()
      val n5 = g3.AddNode
      val n6 = g3.AddNode
      val e9 = g3.AddEdge(n5, n6)
      g3.Freeze
      if ((n5.OutDegree != 1) && (n6.OutDegree != 1) && (n5.InDegree != 1) && (n6.InDegree != 1)) {
        println("[FAIL] Undirected graph wrong edge connectivity")
      } else {
        println("[OK] Undirected graph connectivity is correct")
      }
    }
  }

  //this test works
  "test_deferrable" should "compile" in {
    val x: Any = optiGraphDebug {
      println("Test Deferrable")
      val d = Deferrable[Double](1.0)
      if (d.value != 1.0) {
        println("[FAIL] Expected value = 1.0, Actual value = " + d.value)
      } else {
        println("[OK] Current value is correct")
      }
      d <= 2.0
      if (d.value != 1.0) {
        println("[FAIL] After deferral, expected value = 1.0, Actual value = " + d.value)
      } else {
        println("[OK] Deferral did not affect current value")
      }
      d.assign()
      if (d.value != 2.0) {
        println("[FAIL] After assignment, expected value = 2.0, Actual value = " + d.value)
      } else {
        println("[OK] Current value is correct after assignment")
      }

      d.setValue(1.0)
      d.assign()
      if (d.value != 1.0) {
        println("[FAIL] After repeated assignment, expected value = 1.0, Actual value = " + d.value)
      } else {
        println("[OK] Repeated assignment did not affect value")
      }
    }
  }

  "test_reduceable" should "compile" in {
    val x: Any = optiGraphDebug {
      val g = Graph()
      val n1 = g.AddNode
      val n2 = g.AddNode
      val n3 = g.AddNode
      val n4 = g.AddNode
      g.Freeze

      println("Test Reduction Assignments: SUM ")
      val sum = Reduceable[Int](5)
      for (n ← g.Nodes) {
        sum += 1
      }
      if (sum.value != 9) {
        println("[FAIL] Expected value = 9, Actual value = " + sum.value)
      } else {
        println("[OK] Sum is correct.")
      }

      //-------//

      println("Test Reduction Assignments: PROD ")
      val prod = Reduceable[Int](2)
      for (n ← g.Nodes) {
        prod *= 2
      }
      if (prod.value != 32) {
        println("[FAIL] Expected value = 32, Actual value = " + prod.value)
      } else {
        println("[OK] Product is correct.")
      }

      //-------//

      println("Test Reduction Assignments: ALL ")
      val all = Reduceable[Boolean](true)
      val all2 = Reduceable[Boolean](true)
      for (n ← g.Nodes) {
        all &&= false
        all2 &&= true
      }
      if (all.value != false) {
        println("[FAIL] Expected value = false, Actual value = " + all.value)
      } else if (all2.value != true) {
        println("[FAIL] Expected value = true, Actual value = " + all2.value)
      } else {
        println("[OK] ALL is correct.")
      }

      //-------//

      println("Test Reduction Assignments: ANY ")
      val any = Reduceable[Boolean](false)
      val any2 = Reduceable[Boolean](true)
      for (n ← g.Nodes) {
        any ||= true
        any2 ||= false
      }
      if (any.value != true) {
        println("[FAIL] Expected value = true, Actual value = " + any.value)
      } else if (any2.value != true) {
        println("[FAIL] Expected value = true, Actual value = " + any2.value)
      } else {
        println("[OK] ANY is correct.")
      }

      //-------//

      println("Test Reduction Assignments: COUNT ")
      val count = Reduceable[Int](1)
      val count2 = Reduceable[Int](0)
      for (n ← g.Nodes) {
        count ++= true
        count2 ++= false
      }
      if (count.value != 5) {
        println("[FAIL] Expected value = 4, Actual value = " + count.value)
      } else if (count2.value != 0) {
        println("[FAIL] Expected value = 0, Actual value = " + count2.value)
      } else {
        println("[OK] COUNT is correct.")
      }

      println("Test Reduction Assignments: MAX ")
      val max = Reduceable[Int](MIN_INT)

      //-------//
      //WARNING
      //TODO we need way to name implicit objects
      //maybe we can generate name and then make aliases
      //for generated name
      //HERE REWIRING OF OBJECTS FAILS:
      //==> math.this.Ordering.Int
      //<== Int

      //      for (n <- g.Nodes) {
      //        max >= n.Id
      //      }
      //      if (max.value != 3) {
      //        println("[FAIL] Expected value = 3, Actual value = " + max.value)
      //      } else {
      //        println("[OK] MAX is correct.")
      //      }
      //
      //      //-------//
      //
      //      println("Test Reduction Assignments: MIN ")
      //      val min = Reduceable[Int](MAX_INT)
      //      for (n <- g.Nodes) {
      //        min <= 1
      //      }
      //      if (min.value != 1) {
      //        println("[FAIL] Expected value = 1, Actual value = " + min.value)
      //      } else {
      //        println("[OK] MIN is correct.")
      //      }

      //-------//
      //END OF WARNING

      println("Test Reduceable Basic Ops")
      val red = Reduceable[Double](1.0)
      if (red.value != 1.0) {
        println("[FAIL] Expected value = 1.0, Actual value = " + red.value)
      } else {
        println("[OK] Got correct current value.")
      }
      red.setValue(2.0)
      if (red.value != 2.0) {
        println("[FAIL] Expected value = 2.0, Actual value = " + red.value)
      } else {
        println("[OK] Value was set correctly.")
      }
    }
  }

  "test_reductions" should "compile" in {
    val x: Any = optiGraphDebug {
      val g = Graph()
      val n1 = g.AddNode
      val n2 = g.AddNode
      val n3 = g.AddNode
      val n4 = g.AddNode
      g.Freeze
      val np1 = NodeProperty[Int](g, 1)
      val np2 = NodeProperty[Boolean](g, true)
      val ns = NodeSet()

      // check empty/non-empty collections, with/without filters

      println("Test Reduction Expressions: SUM")
      //TODO - problem with classOf[...] - can't rewire type parameter
      //TODO - problem with holes, $x - parameter is not found
      //        val r1 = Sum(g.Nodes){np1(_)}
      //                    val r2 = Sum(g.Nodes, (n: Rep[Node]) => (n.Id == 1)){np1(_)}
      //            val r3 = Sum(ns.Items){np1(_)}
      //
      //      if(r1 != 4) {
      //        println("[FAIL] Expected value = 4, Actual value = " + r1)
      //      } else {
      //        println("[OK] Sum is correct.")
      //      }
      //      if(r2 != 1) {
      //        println("[FAIL] Expected value = 1, Actual value = " + r2)
      //      } else {
      //        println("[OK] Sum is correct.")
      //      }
      //      if(r3 != 0) {
      //        println("[FAIL] Expected value = 0, Actual value = " + r3)
      //      } else {
      //        println("[OK] Sum is correct.")
      //      }
      //
      //      //-------//
      //
      //      println("Test Reduction Expressions: PRODUCT")
      //      r1 = Product(g.Nodes){np1(_)}
      //      r2 = Product(g.Nodes, (n: Rep[Node]) => (n.Id == 1)){ n => unit(5)}
      //      r3 = Product(ns.Items){np1(_)}
      //
      //      if(r1 != 1) {
      //        println("[FAIL] Expected value = 1, Actual value = " + r1)
      //      } else {
      //        println("[OK] Product is correct.")
      //      }
      //      if(r2 != 5) {
      //        println("[FAIL] Expected value = 5, Actual value = " + r2)
      //      } else {
      //        println("[OK] Product is correct.")
      //      }
      //      if(r3 != 0) {
      //        println("[FAIL] Expected value = 0, Actual value = " + r3)
      //      } else {
      //        println("[OK] Product is correct.")
      //      }
      //
      //      //-------//
      //
      //      println("Test Reduction Expressions: COUNT")
      //      r1 = Count(g.Nodes){(n: Rep[Node]) => (n.Id != 1)}
      //      r2 = Count(g.Nodes) {(n: Rep[Node]) => (n.Id == 1)}
      //      r3 = Count(ns.Items){(n: Rep[Node]) => (n.Id == 1)}
      //
      //      if(r1 != 3) {
      //        println("[FAIL] Expected value = 3, Actual value = " + r1)
      //      } else {
      //        println("[OK] Count is correct.")
      //      }
      //      if(r2 != 1) {
      //        println("[FAIL] Expected value = 1, Actual value = " + r2)
      //      } else {
      //        println("[OK] Count is correct.")
      //      }
      //      if(r3 != 0) {
      //        println("[FAIL] Expected value = 0, Actual value = " + r3)
      //      } else {
      //        println("[OK] Count is correct.")
      //      }
      //
      //      //-------//
      //
      //      println("Test Reduction Expressions: MIN")
      //      r1 = Min(g.Nodes){(n: Rep[Node]) => n.Id}
      //      r2 = Min(g.Nodes, (n: Rep[Node]) => (n.Id == 1)) {(n: Rep[Node]) => n.Id}
      //      r3 = Min(ns.Items){np1(_)}
      //
      //      if(r1 != 0) {
      //        println("[FAIL] Expected value = 0, Actual value = " + r1)
      //      } else {
      //        println("[OK] Min is correct.")
      //      }
      //      if(r2 != 1) {
      //        println("[FAIL] Expected value = 1, Actual value = " + r2)
      //      } else {
      //        println("[OK] Min is correct.")
      //      }
      //      if(r3 != MAX_INT) {
      //        println("[FAIL] Expected value = MAX_INT, Actual value = " + r3)
      //      } else {
      //        println("[OK] Min is correct.")
      //      }
      //
      //      //-------//
      //
      //      println("Test Reduction Expressions: MAX")
      //      r1 = Max(g.Nodes){(n: Rep[Node]) => n.Id}
      //      r2 = Max(g.Nodes, (n: Rep[Node]) => (n.Id == 1)) {(n: Rep[Node]) => n.Id}
      //      r3 = Max(ns.Items){np1(_)}
      //
      //      if(r1 != 3) {
      //        println("[FAIL] Expected value = 3, Actual value = " + r1)
      //      } else {
      //        println("[OK] Max is correct.")
      //      }
      //      if(r2 != 1) {
      //        println("[FAIL] Expected value = 1, Actual value = " + r2)
      //      } else {
      //        println("[OK] Max is correct.")
      //      }
      //      if(r3 != MIN_INT) {
      //        println("[FAIL] Expected value = MIN_INT, Actual value = " + r3)
      //      } else {
      //        println("[OK] Max is correct.")
      //      }
      //
      //      //-------//
      //
      //      println("Test Reduction Expressions: ALL")
      //      val r1b = All(g.Nodes){(n: Rep[Node]) => (n.Id < 5)}
      //      val r2b = All(g.Nodes, (n: Rep[Node]) => (n.Id == 1)) {(n: Rep[Node]) => (n.Id == 2)}
      //      val r3b = All(ns.Items){(n: Rep[Node]) => (n.Id < 5)}
      //
      //      if(r1b != true) {
      //        println("[FAIL] Expected value = true, Actual value = " + r1b)
      //      } else {
      //        println("[OK] All is correct.")
      //      }
      //      if(r2b != false) {
      //        println("[FAIL] Expected value = false, Actual value = " + r2b)
      //      } else {
      //        println("[OK] All is correct.")
      //      }
      //      if(r3b != true) {
      //        println("[FAIL] Expected value = true, Actual value = " + r3b)
      //      } else {
      //        println("[OK] All is correct.")
      //      }
      //
      //      //-------//
      //
      //      println("Test Reduction Expressions: ANY")
      //      r1b = Any(g.Nodes){(n: Rep[Node]) => (n.Id > 2)}
      //      r2b = Any(g.Nodes, (n: Rep[Node]) => (n.Id == 1)) {(n: Rep[Node]) => (n.Id == 2)}
      //      r3b = Any(ns.Items){(n: Rep[Node]) => (n.Id < 5)}
      //
      //      if(r1b != true) {
      //        println("[FAIL] Expected value = true, Actual value = " + r1b)
      //      } else {
      //        println("[OK] Any is correct.")
      //      }
      //      if(r2b != false) {
      //        println("[FAIL] Expected value = false, Actual value = " + r2b)
      //      } else {
      //        println("[OK] Any is correct.")
      //      }
      //      if(r3b != false) {
      //        println("[FAIL] Expected value = false, Actual value = " + r3b)
      //      } else {
      //        println("[OK] Any is correct.")
      //      }
    }
  }

  //this test doesn't work
  //    "Program with method" should "work" in {
  //      val x: Int = optiGraphDebug {
  //              def test_graphOps() {
  //                val g = Graph()
  //
  //                val n1 = g.AddNode
  //
  //              }
  //              test_graphOps()
  //              1
  //      }
  //    }

}