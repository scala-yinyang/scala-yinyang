package shallow.optigraph

//  // context for operations during BFS traversals
//  val bfsVisitedDynVar = new DynamicVariable[Array[Int]](null)

/** Operations on Nodes */
class Node extends ppl.dsl.optigraph.Node {
  /** Returns the nodes that node n has edges to */
  def Nbrs: GIterable[Node] = ???
  /** Returns the nodes that node n has edges to */
  def OutNbrs: GIterable[Node] = ???
  /** Returns the nodes that have edges to node n */
  def InNbrs: GIterable[Node] = ???
  /**
   * During BFS: returns the in-neighbors that are closer to the BFS root node
   * than the current node n, in hop distance
   */
  def UpNbrs: GIterable[Node] = ???
  /**
   * During BFS: returns the out-neighbors that are farther to the BFS root node
   * than the current node n, in hop distance
   */
  def DownNbrs: GIterable[Node] = ???
  /** Returns the outgoing edges of this node */
  def Edges: GIterable[Edge] = ???
  /** Returns the outgoing edges of this node */
  def OutEdges: GIterable[Edge] = ???
  /** Returns the incoming edges of node n */
  def InEdges: GIterable[Edge] = ???
  /** During BFS: returns the edges from upNeighbors */
  def UpEdges: GIterable[Edge] = ???
  /** During BFS: returns the edges to downNeighbors */
  def DownEdges: GIterable[Edge] = ???
  /** Returns the number of nodes that n has edges to */
  def NumNbrs: Int = ???
  /** Returns the number of nodes that n has edges to */
  def NumOutNbrs: Int = ???
  /** Returns the number of nodes that have edges to n */
  def NumInNbrs: Int = ???
  /** Returns the number of outgoing edges */
  def Degree: Int = ???
  /** Returns the number of outgoing edges */
  def OutDegree: Int = ???
  /** Returns the number of incoming edges */
  def InDegree: Int = ???
  /** Returns the id of the node (unique per graph) */
  def Id: Int = ???
}

/** NodeProperty constructors */
object NodeProperty {
  /** Create a new node property (each node's value is set to default of the type) */
  def apply[A](g: Graph): NodeProperty[A] = ???
  /** Create a new node property (each node's value is set to init) */
  def apply[A](g: Graph, init: A): NodeProperty[A] = ???
}
/** Alias for NodeProperty */
object NP {
  def apply[A](g: Graph) = NodeProperty.apply(g)
  def apply[A](g: Graph, init: A) = NodeProperty.apply(g, init)
}

/** Operations on NodeProperties */
class NodeProperty[A](np: NodeProperty[A]) extends ppl.dsl.optigraph.NodeProperty[A] {
  /** Return the property value of node n */
  def apply(n: Node): A = ???
  /** Update the property value of node n to x */
  def update(n: Node, x: A): Unit = ???
  /** Set the value of all the nodes to x (parallel operation) */
  def setAll(x: A): Unit = ???
  /** Defer assigning value x to node n (any other previously deferred value will be overwritten) */
  def <=(n: Node, x: A): Unit = ???
  /** Assign the value deferred for node n (the latest deferred value) */
  def assign(n: Node): Unit = ???
  /** Assign the values deferred for all the nodes (parallel operation) */
  def assignAll(): Unit = ???

  def dcApply(idx: Int): A = ???
  def dcSize: Int = ???
  def dcUpdate(idx: Int, x: A): Unit = ???
}
