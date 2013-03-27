import shallow.optigraph._; import lifted._; object SCC extends LanguageOps {
  optiGraphAnalysis {
    val G = RandUniformGraph(100000, 800000, 1997L)
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
}