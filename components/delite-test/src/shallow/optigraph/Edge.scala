package shallow.optigraph

class Edge {
  /** Source node */
  def From: Node = ???
  /** Destination node */
  def To: Node = ???
  /** Edge id (unique per graph) */
  def Id: Int = ???
}

object EdgeProperty {
  /** Create a new edge property (each edge's value is set to default of the type) */
  def apply[A](g: Graph): EdgeProperty[A] = ???
  /** Create a new edge property (each edge's value is set to init) */
  def apply[A](g: Graph, init: A): EdgeProperty[A] = ???
}
/** Alias for EdgeProperty */
object EP {
  def apply[A](g: Graph): EdgeProperty[A] = ???
  def apply[A](g: Graph, init: A): EdgeProperty[A] = ???
}

/** Operations on EdgeProperties */
class EdgeProperty[A](ep: EdgeProperty[A]) {
  /** Return the property value of edge e */
  def apply(e: Edge): A = ???
  /** Update the property value of edge e to x */
  def update(e: Edge, x: A): Unit = ???
  /** Set the value of all the edges to x (parallel operation) */
  def setAll(x: A): Unit = ???
  /** Defer assigning value x to edge e (any other previously deferred value will be overwritten) */
  def <=(e: Edge, x: A): Unit = ???
  /** Assign the value deferred for edge e (the latest deferred value) */
  def assign(e: Edge): Unit = ???
  /** Assign the values deferred for all the edges (parallel operation) */
  def assignAll(): Unit = ???
}