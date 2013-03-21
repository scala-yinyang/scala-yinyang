package shallow.optiml

trait LanguageOps extends OverloadHack {

  /** Iterations */

  // sequential for loop
  def For[T, GT <: GIterable[T]](items: GT)(block: T ⇒ Unit): Unit = ???
  //def For[T: Manifest, GT <: GIterable[T]](items: GT)(block: T ⇒ Unit): Unit = ???
  def For[T, GT <: GIterable[T]](items: GT, filter: T ⇒ Boolean)(block: T ⇒ Unit): Unit = ???
  //def For[T: Manifest, GT <: GIterable[T]](items: GT, filter: T ⇒ Boolean)(block: T ⇒ Unit): Unit = ???

  // parallel for-each loop
  def Foreach[T](items: GIterable[T])(block: T ⇒ Unit): Unit = ???
  //  def Foreach[T: Manifest](items: GIterable[T])(block: T ⇒ Unit): Unit = ???
  // parallel for-each loop with filter
  def Foreach[T](items: GIterable[T], pred: T ⇒ Boolean)(block: T ⇒ Unit): Unit = ???
  //  def Foreach[T: Manifest](items: GIterable[T], pred: T ⇒ Boolean)(block: T ⇒ Unit): Unit = ???

  /** Reductions (optional filter predicate) */

  def Sum[T, A](items: GIterable[T])(block: T ⇒ A): A = ???
  //def Sum[T: Manifest, A: Manifest: Numeric](items: GIterable[T])(block: T ⇒ A): A = ???
  def Sum[T, A](items: GIterable[T], filter: T ⇒ Boolean)(block: T ⇒ A): A = ???
  //def Sum[T, A: Numeric](items: GIterable[T], filter: T ⇒ Boolean)(block: T ⇒ A): A = ???
  def Product[T, A](items: GIterable[T])(block: T ⇒ A): A = ???
  //def Product[T: Manifest, A: Manifest: Numeric](items: GIterable[T])(block: T ⇒ A): A = ???
  def Product[T, A](items: GIterable[T], filter: T ⇒ Boolean)(block: T ⇒ A): A = ???
  //def Product[T: Manifest, A: Manifest: Numeric](items: GIterable[T])(block: T ⇒ A): A = ???
  def Max[T, A](items: GIterable[T])(block: T ⇒ A): A = ???
  //  def Max[T: Manifest, A: Manifest: Ordering](items: GIterable[T])(block: T ⇒ A): A = ???
  def Max[T, A](items: GIterable[T], filter: T ⇒ Boolean)(block: T ⇒ A): A = ???
  //  def Max[T: Manifest, A: Manifest: Ordering](items: GIterable[T], filter: T ⇒ Boolean)(block: T ⇒ A): A = ???
  def Min[T, A](items: GIterable[T])(block: T ⇒ A): A = ???
  //  def Min[T: Manifest, A: Manifest: Ordering](items: GIterable[T])(block: T ⇒ A): A = ???
  def Min[T, A](items: GIterable[T], filter: T ⇒ Boolean)(block: T ⇒ A): A = ???
  def Count[T](items: GIterable[T])(block: T ⇒ Boolean): Int = ???
  //  def Count[T: Manifest](items: GIterable[T])(block: T ⇒ Boolean): Int = ???
  def All[T](items: GIterable[T])(block: T ⇒ Boolean): Boolean = ???
  //  def All[T: Manifest](items: GIterable[T])(block: T ⇒ Boolean): Boolean = ???
  def All[T](items: GIterable[T], filter: T ⇒ Boolean)(block: T ⇒ Boolean): Boolean = ???
  //  def All[T: Manifest](items: GIterable[T], filter: T ⇒ Boolean)(block: T ⇒ Boolean): Boolean = ???
  def Any[T](items: GIterable[T])(block: T ⇒ Boolean): Boolean = ???
  //  def Any[T: Manifest](items: GIterable[T])(block: T ⇒ Boolean): Boolean = ???
  def Any[T](items: GIterable[T], filter: T ⇒ Boolean)(block: T ⇒ Boolean): Boolean = ???
  //  def Any[T: Manifest](items: GIterable[T], filter: T ⇒ Boolean)(block: T ⇒ Boolean): Boolean = ???

  /** Traversals (optional navigator and post-traversal clauses) */

  // DFS order traversal (sequential)

  def InDFS(g: Graph, from: Node, block: Node ⇒ Unit): Unit = ???
  def InDFS(g: Graph, from: Node, filter: Node ⇒ Boolean, block: Node ⇒ Unit): Unit = ???
  def InDFS(g: Graph, from: Node, block: Node ⇒ Unit, inPost: Node ⇒ Unit)(implicit o: Overloaded1): Unit = ???
  def InDFS(g: Graph, from: Node, filter: Node ⇒ Boolean, block: Node ⇒ Unit, inPost: Node ⇒ Unit): Unit = ???
  // post-dfs-traversal clause
  def InPost(block: Node ⇒ Unit): Node ⇒ Unit = ???

  // BFS order traversal (parallel on every level)

  def InBFS(g: Graph, from: Node, block: Node ⇒ Unit): Unit = ???
  def InBFS(g: Graph, from: Node, filter: Node ⇒ Boolean, block: Node ⇒ Unit): Unit = ???
  def InBFS(g: Graph, from: Node, block: Node ⇒ Unit, inReverse: Node ⇒ Unit)(implicit o: Overloaded1): Unit = ???
  def InBFS(g: Graph, from: Node, filter: Node ⇒ Boolean, block: Node ⇒ Unit, inReverse: Node ⇒ Unit): Unit = ???
  // post-bfs-traversal clause
  def InReverse(block: Node ⇒ Unit): Node ⇒ Unit = ???

  /** Graph Kernels */

  // def btw_centrality()...

  /** MISC */

  // Random graph generators
  //def RandDGraph(numNodes: Int], numEdges: Int]): Graph] = new_rand_graph(numNodes, numEdges)
  //def RandUGraph(numNodes: Int], numEdges: Int]): Graph] = new_rand_graph(numNodes, numEdges)

  def wall_time(): Double = ???
  def tic(deps: Any*): Unit = ???
  def toc(deps: Any*): Unit = ???

  def profile_start(deps: Seq[Any]): Unit = ???
  def profile_stop(deps: Seq[Any]): Unit = ???

  /* I/O */
  //def loadGraph(filename: String]) = GraphInputReader.read(filename, delim)

  /** Special values */

  def MAX_INT = scala.Int.MaxValue
  def MAX_FLOAT = scala.Float.MaxValue
  def MAX_DOUBLE = scala.Double.MaxValue
  def MIN_INT = scala.Int.MinValue
  def MIN_FLOAT = scala.Float.MinValue
  def MIN_DOUBLE = scala.Double.MinValue

  /*def INF*/
}