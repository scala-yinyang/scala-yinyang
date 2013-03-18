package shallow.optiml

//  trait Graph {
//    // directed
//    type DGraph = Graph
//    // undirected
//    type UGraph = Graph
//  }

//  trait Node
//  trait Edge

/* Properties (for immutable graphs only) */

/**
 * Used to associate data with the nodes in the graph
 * Note: properties can be associated only with immutable graph instances
 */
trait NodeProperty[T] {
  type NP[T] = NodeProperty[T]
}
/**
 * An EdgeProperty object is used to associate data with graph edges
 * Note: properties can be associated only with immutable graph instances
 */
trait EdgeProperty[T] {
  type EP[T] = EdgeProperty[T]
}

/* Collection types */

/** Unordered collection of unique elements */
trait GSet[A] {
  type NodeSet = GSet[Node]
  type NS = GSet[Node]
  type EdgeSet = GSet[Edge]
  type ES = GSet[Edge]

  def Items: GIterable[A] = ???
  /** Returns true if the collection contains the element */
  def Has(e: A): Boolean = ???
  /** Returns the size of the collection */
  def Size: Int = ???
  /** Adds a new element to the set */
  def Add(e: A): Unit = ???
  /** Adds all the elements from s2 to the set */
  def AddSet(s2: GSet[A]): Unit = ???
  /** Removes the element from the set */
  def Remove(e: A): Unit = ???
  /** Removes all the elements in s2 from the set */
  def RemoveSet(s2: GSet[A]): Unit = ???
  /** Removes all the elements from the set */
  def Clear: Unit = ???
  /** Returns a copy of this set */
  def cloneL(): GSet[A] = ???

}
/** Ordered collection of unique elements */
trait GOrder[T] {
  type NodeOrder = GOrder[Node]
  type NO = GOrder[Node]
  type EdgeOrder = GOrder[Edge]
  type EO = GOrder[Edge]
}
/** Ordered collection of elements (with duplicates possible) */
trait GSeq[T] {
  type NodeSeq = GSeq[Node]
  type NS = GSeq[Node]
  type EdgeSeq = GSeq[Edge]
  type ES = GSeq[Edge]
}

/* Other */

/** Iterable/reduceable collection of graph items (nodes or edges) */
class GIterable[T](var data: Array[T], offset: Int, size: Int) {

  def apply(i: Int): T = ???
  def length: Int = size //data.length
  def toList: List[T] = ???
  def toSet: GSet[T] = ???

  def unsafeSetData(xs: Array[T], len: Int): Unit = ???

  // required by DeliteCollection
  def dcSize: Int = ???
  def dcApply(i: Int): T = ???
  def dcUpdate(i: Int, n: T): Unit = ???
}

/** Can be used in reduction assignments (in a parallel context) */
trait Reduceable[T]

/** Deferrable constructor */
object Deferrable {
  /** Creates a new Deferrable with initial value init */
  def apply[T](init: T): Deferrable[T] = ???
  //TODO TOASK (with Manifest doesn't work)
  //def apply[T: Manifest](init: T): Deferrable[T] = ???
  //original method
  //does we need manifest here only because T is type parameter of Rep
  //def apply[T:Manifest](init:Rep[T]) = def_new(init)
}

/** Operations on Deferrables */
//class Deferrable[T: Manifest](d: Deferrable[T]) {
trait Deferrable[T] {
  /**
   * Assign the latest deferred value
   *  No effect if no value was deferred since last assignment
   */
  def assign(): Unit = ???
  /** Get the current value (latest value assigned) */
  def value: T = ???
  /** Set the current value */
  def setValue(v: T): Unit = ???
  /** Defer a value assignment */
  def <=(v: T): Unit = ???

}

