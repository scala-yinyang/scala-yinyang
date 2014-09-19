package base

trait Base {
  def main(): Any
}

trait RepBase extends Base {
  type R[T]
}