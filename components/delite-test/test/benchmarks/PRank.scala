import shallow.optigraph._;import lifted._;
object PR extends LanguageOps {
  optiGraphAnalysis {
	val G = graph_load("/home/viq/delite/Delite/test.bin")

      val e = 0.001
      val d = 0.85
      val max_iter = 6
      val PR = NodeProperty[Double](G)

      val diff = Reduceable[Double](0.0)
      var cnt = 0
      val N = G.NumNodes.asInstanceOf[Double]
      PR.setAll(1.0 / N)

      println("G.N " + N)

      // move to ds
      val deg = NewArray[Int](G.NumNodes)
      for (t ← G.Nodes) {
        deg(t.Id) = t.OutDegree
      }

      tic(G, PR, deg)
      //var num_abs = 0
      //var v = 0.0
      var cond = true
      //val n = G.Node(0)

      diff.setValue(0.0)
      for (t ← G.Nodes) {
        val Val: Double = ((1.0 - d) / N) + d * Sum(t.InNbrs) {
          w ⇒ PR(w) / deg(w.Id) //w.OutDegree
        }
        //val Val = v
        PR <= (t, Val)

        //TODO: Question for Math.abs definition (see in LanguageOps for shallow - is it correct)
        diff += Math.abs(Val - PR(t))
        //num_abs += 1
        //v += 1.0
      }
      PR.assignAll()
      cnt += 1
      cond = (diff.value > e) && (cnt < max_iter)
      //          }

      println("count = " + cnt)
      //println("abs times = " + num_abs)

      //TODO (FEATURE ANALYZER) problem with deps: Any* parameter and empty parameter list
      toc(())
  }
}