package shallow.optigraph

//  trait Graph {
//    // directed
//    type DGraph = Graph
//    // undirected
//    type UGraph = Graph
//  }

//  trait Node
//  trait Edge

/* Collection types */

/** Unordered collection of unique elements */

object NodeSet {
  def apply(): GSet[Node] = ???
}

trait GSet[A] extends ppl.dsl.optigraph.GSet[A] {
  // type NodeSet = GSet[Node]
  // type NS = GSet[Node]
  // type EdgeSet = GSet[Edge]
  // type ES = GSet[Edge]

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

/** NodeOrder constructors */
object NodeOrder {
  def apply(): GOrder[Node] = ???
}
object NO {
  def apply(): GOrder[Node] = ???
}

/** EdgeOrder constructors */
object EdgeOrder {
  def apply(): GOrder[Edge] = ???
}
object EO {
  def apply(): GOrder[Edge] = ???
}

/** Ordered collection of unique elements */
trait GOrder[T] extends ppl.dsl.optigraph.GOrder[T] {
  // type NodeOrder = GOrder[Node]
  // type NO = GOrder[Node]
  // type EdgeOrder = GOrder[Edge]
  // type EO = GOrder[Edge]

  /** Returns all the items in the collection */
  def Items: GIterable[T] = ???
  /** Returns true if the collection contains the element */
  def Has(e: T): Boolean = ???
  /** Returns the size of the collection */
  def Size: Int = ???
  /** Returns the first element in the order */
  def Front: T = ???
  /** Returns the last element in the order */
  def Back: T = ???
  /** Adds a new element to the front of the order */
  def PushFront(e: T): Unit = ???
  /** Adds a new element to the back of the order */
  def PushBack(e: T): Unit = ???
  /** Prepends all the elements of o2 (in order) to the order */
  def PushFrontOrder(o2: GOrder[T]): Unit = ???
  /** Appends all the elements of o2 (in order) to the order */
  def PushBackOrder(o2: GOrder[T]): Unit = ???
  /** Removes and returns the first element in the order */
  def PopFront(): T = ???
  /** Removes and returns the last element in the order */
  def PopBack(): T = ???
  /**
   * Lookup the element at position idx in the order
   *  RuntimeException if idx is out of bounds
   */
  def apply(idx: Int): T = ???

}

object NodeSeq {
  def apply(): GSeq[Node] = ???
}
object NQ {
  def apply(): GSeq[Node] = ???
}

/** EdgeSeq constructors */
object EdgeSeq {
  def apply(): GSeq[Edge] = ???
}
object EQ {
  def apply(): GSeq[Edge] = ???
}

/** Ordered collection of elements (with duplicates possible) */
trait GSeq[T] extends ppl.dsl.optigraph.GSeq[T] {
  /*type NodeSeq = GSeq[Node]
  type NS = GSeq[Node]
  type EdgeSeq = GSeq[Edge]
  type ES = GSeq[Edge]*/

  /** Returns all the items in the collection */
  def Items: GIterable[T] = ???
  /** Returns true if the collection contains the element */
  def Has(e: T): Boolean = ???
  /** Returns the size of the collection */
  def Size: Int = ???
  /** Returns the first element in the sequence */
  def Front: T = ???
  /** Returns the last element in the sequence */
  def Back: T = ???
  /** Adds a new element to the front of the sequence */
  def PushFront(e: T): Unit = ???
  /** Adds a new element to the back of the sequence */
  def PushBack(e: T): Unit = ???
  /** Prepends all the elements of s2 (in order) to the sequence */
  def PushFrontSeq(s2: GSeq[T]): Unit = ???
  /** Appends all the elements of s2 (in order) to the sequence */
  def PushBackSeq(s2: GSeq[T]): Unit = ???
  /** Removes and returns the first element in the sequence */
  def PopFront(): T = ???
  /** Removes and returns the last element in the sequence */
  def PopBack(): T = ???
  /**
   * Lookup the element at position idx in the sequence
   *  RuntimeException if idx is out of bounds
   */
  def apply(idx: Int): T = ???
}

/* Other */

object NewArray extends scala.AnyRef {
  def apply[T](n: Int): scala.Array[T] = ???
}
//object Array extends scala.AnyRef {
//  def apply[T](xs : T*)(implicit evidence$5 : scala.Predef.Manifest[T]) : ArrayOps.super[Base/*scala.virtualization.lms.common.Base*/].Rep[scala.Array[T]] = ???
//}

/** Iterable/reduceable collection of graph items (nodes or edges) */
class GIterable[T](var data: Array[T], offset: Int, size: Int) extends ppl.dsl.optigraph.GIterable[T] {

  def apply(i: Int): T = ???
  def length: Int = size //data.length
  def toList: List[T] = ???

  /** Returns the elements as a set */
  def toSet(): GSet[T] = ???

  def unsafeSetData(xs: Array[T], len: Int): Unit = ???

  // required by DeliteCollection
  def dcSize: Int = ???
  def dcApply(i: Int): T = ???
  def dcUpdate(i: Int, n: T): Unit = ???

  /** Parallel iteration */
  def foreach(block: T ⇒ Unit): Unit = ???
  /** Sequential iteration */
  def forseq(block: T ⇒ Unit): Unit = ???
  def forseq(filter: T ⇒ Boolean, block: T ⇒ Unit): Unit = ???
  /** Returns a filtered GIterable collection of elements */
  def filter(pred: T ⇒ Boolean): GIterable[T] = ???
  /** Reductions */
  def sum[A: Manifest: Numeric](block: T ⇒ A): A = ???
  def sum[A: Manifest: Numeric](filter: T ⇒ Boolean, block: T ⇒ A): A = ???
  def product[A: Manifest: Numeric](block: T ⇒ A): A = ???
  def product[A: Manifest: Numeric](filter: T ⇒ Boolean, block: T ⇒ A): A = ???
  def max[A: Manifest: Ordering](block: T ⇒ A): A = ???
  def max[A: Manifest: Ordering](filter: T ⇒ Boolean, block: T ⇒ A): A = ???
  def min[A: Manifest: Ordering](block: T ⇒ A): A = ???
  def min[A: Manifest: Ordering](filter: T ⇒ Boolean, block: T ⇒ A): A = ???
  // counts the number of elements for which the predicate holds
  def count(pred: T ⇒ Boolean): Int = ???
  // boolean AND
  def all(block: T ⇒ Boolean): Boolean = ???
  def all(filter: T ⇒ Boolean, block: T ⇒ Boolean): Boolean = ???
  // boolean OR
  def any(block: T ⇒ Boolean): Boolean = ???
  def any(filter: T ⇒ Boolean, block: T ⇒ Boolean): Boolean = ???

}

object Reduceable {
  /** Creates a new Reduceable with initial value init */
  def apply[T](init: T): Reduceable[T] = ???
}

///** Can be used in reduction assignments (in a parallel context) */
trait Reduceable[T] extends ppl.dsl.optigraph.Reduceable[T] {

  /** Returns the current value of r*/
  def value: T = ???
  /** Sets the current value of r to v */
  def setValue(v: T): Unit = ???

  /** Reduction assignments */
  // sum
  def +=(v: T)(implicit a: Numeric[T]): Unit = ???
  // product
  def *=(v: T)(implicit a: Numeric[T]): Unit = ???
  // min
  def <=(v: T)(implicit a: Ordering[T]): Unit = ???
  // max
  def >=(v: T)(implicit a: Ordering[T]): Unit = ???
  // count (TODO: how to constrain method to work on Reduceable[Int] only?)
  def ++=(v: Boolean): Unit = ???
  // all (boolean AND)
  def &&=(v: Boolean): Unit = ???
  // any (boolean OR)
  def ||=(v: Boolean): Unit = ???
}

/** Deferrable constructor */
object Deferrable {
  /** Creates a new Deferrable with initial value init */
  def apply[T](init: T): Deferrable[T] = ???
  //def apply[T: Manifest](init: T): Deferrable[T] = ???
}

/** Operations on Deferrables */
//class Deferrable[T: Manifest](d: Deferrable[T]) {
trait Deferrable[T] extends ppl.dsl.optigraph.Deferrable[T] {
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
