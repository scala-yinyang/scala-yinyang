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

class OptiGraphSpec extends FlatSpec with ShouldMatchers {

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