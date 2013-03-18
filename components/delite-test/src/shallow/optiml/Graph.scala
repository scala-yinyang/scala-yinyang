package shallow.optiml

/** Directed graph constructors */
object Graph {
  def apply(): Graph = ??? //GraphOps.dgraph_new()
}
object DGraph {
  def apply(): Graph = ??? //GraphOps.dgraph_new()
}
/** Undirected graph constructors */
object UGraph {
  def apply(): Graph = ??? //GraphOps.ugraph_new()
}

object RandUniformGraph {
  def apply(numNodes: Int, numEdges: Int, seed: Long): Graph = ???
}

//  implicit def repGraphToGraphOps(g: Graph) = new GraphOpsCls(g)

/** Operations on Graphs */
class Graph(g: Graph) {
  /** Returns all the nodes in the graph */
  def Nodes: GIterable[Node] = ???
  /** Returns all the edges in the graph */
  def Edges: GIterable[Edge] = ???
  /** Returns the number of nodes in the graph */
  def NumNodes: Int = ???
  /** Returns the number of edges in the graph */
  def NumEdges: Int = ???
  /** Returns the node with the given id */
  def Node(nodeId: Int): Node = ???
  /** Returns the edge with the given id */
  def Edge(edgeId: Int): Edge = ???
  /** Flips the direction of edges in the graph (TODO) */
  def ^ : Graph = ???
  /** BFS traversals (with optional filter and inReverse clauses) */
  def InBFS(from: Node, block: Node ⇒ Unit): Unit = ???
  def InBFS(from: Node, filter: Node ⇒ Boolean, block: Node ⇒ Unit): Unit = ???
  //    def InBFS(from: Node, block: Node => Unit, inReverse: Node => Unit)(implicit o: Overloaded2): Unit = ???
  def InBFS(from: Node, filter: Node ⇒ Boolean, block: Node ⇒ Unit, inReverse: Node ⇒ Unit): Unit = ???
  /** DFS traversals (with optional filter and inPost clauses) */
  def InDFS(from: Node, block: Node ⇒ Unit): Unit = ???
  def InDFS(from: Node, filter: Node ⇒ Boolean, block: Node ⇒ Unit): Unit = ???
  //    def InDFS(from: Node, block: Node => Unit, inPost: Node => Unit)(implicit o: Overloaded2): Unit = ???
  def InDFS(from: Node, filter: Node ⇒ Boolean, block: Node ⇒ Unit, inPost: Node ⇒ Unit): Unit = ???
  // mutable graph ops (for testing)
  /** Adds a new node to the graph and returns this node */
  def AddNode: Node = ???
  /** Adds a new edge to the graph and returns this edge */
  def AddEdge(from: Node, to: Node): Edge = ???
  /** Freeze the graph, making it immutable */
  def Freeze: Unit = ???
  /** Returns an immutable snapshot of the graph */
  def Snapshot: Graph = ???
}

