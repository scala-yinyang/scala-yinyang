import shallow.optigraph._; import lifted._; object Test1 extends LanguageOps {
  optiGraphAnalysis {
    //val G = rand_graph()
    val G = RandUniformGraph(100000, 800000, 1997L)

    val member = NodeProperty[Int](G)
    var i = 0
    for (n ← G.Nodes) {
      member(n) = i % 4
      //println("Node id = " + n.Id + " member = " + i + " degree = " + n.Degree)
      i += 1
    }

    val start_time = wall_time()
    var C = 0.0
    var num = 0

    //TODO: problem with while:
    //    type mismatch;
    //      [error]  found   : generated$liftedOptiGraph3.this.Rep[Boolean]
    //      [error]     (which expands to)  generated$liftedOptiGraph3.this.Exp[Boolean]
    //        [error]  required: Boolean
    //    while (num < 4) {

    //TODO: change member(u) > num to member(u) == num
    val Din = Sum(G.Nodes, (u: Node) ⇒ member(u) > num) { _.Degree }
    val Dout = Sum(G.Nodes, (u: Node) ⇒ member(u) != num) { _.Degree }
    //TODO: change member(u) > num to member(u) == num
    val Cross = Sum(G.Nodes, (u: Node) ⇒ member(u) > num) { u ⇒
      Count(u.Nbrs) { j ⇒ member(j) != num }
    }
    val m = if (Din < Dout) Din else Dout
    val retVal = if (m == 0) {
      if (Cross == 0) 0.0 else MAX_DOUBLE
    } else {
      Cross.asInstanceOf[Double] / m.asInstanceOf[Double]
    }
    C += retVal
    num += 1
    //    }
    println("TIME_LOOP: " + (wall_time() - start_time))

    //println("Din = " + Din)
    //println("Dout = " + Dout)
    //println("Retval = " + retVal)
  }
}