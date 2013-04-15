/*trait ConductanceApp extends lifted.OptiGraph {
  def mainDelite(): Any = ()
  override def main(): Unit = {
    val G = RandUniformGraph(100000, 800000, 1997L)
    val CompID = NodeProperty[Int](G)

    val start_time = wall_time()
    var numC = 0
    val P = NodeOrder()
    CompID.setAll(-1)

    For /*[Node, GIterable[Node]]*/ (G.Nodes, (t: Rep[Node]) ⇒ !(P.Has(t))) { t ⇒
      InDFS(G, t, (s: Rep[Node]) ⇒ !P.Has(s), { t ⇒ unit() },
        { InPost(s ⇒ P.PushFront(s)) })
    }

    For(P.Items, (s: Rep[Node]) ⇒ CompID(s) == -1) { s ⇒
      InBFS(G^, s, /*(t:Rep[Node]) => CompID(s) == -1,*/ { t ⇒
        if (CompID(s) == -1) CompID(t) = numC
      })
      numC += 1;
    }
    println("TIME_LOOP: " + (wall_time() - start_time))
    println("NumC = " + numC)
  }
}*/