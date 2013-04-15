package shallow.optigraph

trait OverloadHack extends scala.AnyRef {
  implicit object Overloaded1
  implicit object Overloaded2
  implicit object Overloaded3
  implicit object Overloaded4

  // implicit val overloaded1 = new Overloaded1
  // implicit val overloaded2 = new Overloaded2
  // implicit val overloaded3 = new Overloaded3
  // implicit val overloaded4 = new Overloaded4
}