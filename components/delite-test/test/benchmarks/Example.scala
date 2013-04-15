/*import shallow.optigraph._
import lifted._
object Example {
  optiGraphAnalysis {
    val g: Graph = Graph()
    val n1: Node = ??? //g.AddNode
    val n2: Node = ??? //g.AddNode
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

    //TODO (Feature Analyzer) - toSet problem
    //        val nodes = g.Nodes.toSet
    //        val edges = g.Edges.toSet
    //        if (nodes.Size != 2 || edges.Size != 4) {
    //          println("[FAIL] Wrong nodes/edges collection size")
    //        } else {
    //          println("[OK] Correct nodes/edges collection size")
    //        }
    //        if ((!nodes.Has(n1)) || (!nodes.Has(n2)) || (!edges.Has(e1)) || (!edges.Has(e2))
    //          || (!edges.Has(e3)) || (!edges.Has(e4))) {
    //          println("[FAIL] Wrong nodes/edges collection contents")
    //        } else {
    //          println("[OK] Correct nodes/edges collection contents")
    //        }

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
}*/ 