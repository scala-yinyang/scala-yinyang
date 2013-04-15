/*trait SCCDelite extends lifted.OptiGraph {
  def mainDelite(): Any = ()
  override def main(): Unit = {
    val G = RandUniformGraph(100000, 800000, 1997L)

    val member = NodeProperty[Int](G)
    var i = 0
    for (n ← G.Nodes) {
      member(n) = i % 4
      i += 1
    }

    val start_time = wall_time()
    var C = 0.0
    var num = 0
    while (num < 4) {

      val Din = Sum(G.Nodes, (u: Rep[Node]) ⇒ member(u) == num) { _.Degree }
      val Dout = Sum(G.Nodes, (u: Rep[Node]) ⇒ member(u) != num) { _.Degree }
      val Cross = Sum(G.Nodes, (u: Rep[Node]) ⇒ member(u) == num) { u ⇒
        Count(u.Nbrs) { j ⇒ member(j) != num }
      }
      val m = if (Din < Dout) Din else Dout

      val retVal = if (m == 0) {
        if (Cross == 0) 0.0 else MAX_DOUBLE
      } else {
        Cross.asInstanceOf[Rep[Double]] / m.asInstanceOf[Rep[Double]]
      }
      C += retVal
      num += 1
    }
    println("TIME_LOOP: " + (wall_time() - start_time))
  }

}*/