package shallow.optiml

trait OverloadHack extends scala.AnyRef {
  class Overloaded1
  class Overloaded2
  class Overloaded3
  class Overloaded4

  implicit val overloaded1 = new Overloaded1
  implicit val overloaded2 = new Overloaded2
  implicit val overloaded3 = new Overloaded3
  implicit val overloaded4 = new Overloaded4
}