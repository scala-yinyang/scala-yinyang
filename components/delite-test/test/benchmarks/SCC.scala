import shallow.optigraph._
import lifted._

/* For not found.
object SCC {
  optiGraphAnalysis {
    def rand_graph(): Graph = {
      val g = Graph()
      val n1 = g.AddNode
      val n2 = g.AddNode
      val n3 = g.AddNode
      val n4 = g.AddNode
      val n5 = g.AddNode

      g.AddEdge(n1, n2)
      g.AddEdge(n1, n3)
      g.AddEdge(n2, n4)
      g.AddEdge(n3, n5)
      g.Snapshot
    }

    val G = rand_graph()
    val CompID = NodeProperty[Int](G)

    val start_time = wall_time()
    var numC = 0
    val P = NodeOrder()
    CompID.setAll(-1)

    For /*[Node, GIterable[Node]]*/ (G.Nodes, (t: Node) ⇒ !(P.Has(t))) { t ⇒
      //TODO: how do we should change unit in def InDFS:
      //InDFS(G, t, (s: Node) => !P.Has(s), { t => unit()},
      //simply to () ?
      InDFS(G, t, (s: Node) ⇒ !P.Has(s), { t ⇒ () },
        { InPost(s ⇒ P.PushFront(s)) })
    }

    //TODO: change CompID(s) > -1 to CompID(s) == -1
    For(P.Items, (s: Node) ⇒ CompID(s) > -1) { s ⇒
      InBFS(G^, s, /*(t:Rep[Node]) => CompID(s) == -1,*/ { t ⇒
        if (CompID(s) == -1) CompID(t) = numC
      })
      numC += 1;
    }
    println("TIME_LOOP: " + (wall_time() - start_time))
    println("NumC = " + numC)
  }
}*/