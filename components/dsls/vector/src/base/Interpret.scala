package base

trait Interpret {

  def main(): Any
  def interpret[T](): T

}
