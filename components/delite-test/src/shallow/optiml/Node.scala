package shallow.optiml

//  // context for operations during BFS traversals
//  val bfsVisitedDynVar = new DynamicVariable[Array[Int]](null)

/** Operations on Nodes */
class Node {
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
