package base

trait ScalaVirtualizedBase extends RepBase {
  def __app[U](f: R[() => U]): R[U] = ???
  def __app[T_1, U](f: R[T_1 => U])(v1: R[T_1]): R[U] = ???
  def __app[T_1, T_2, U](f: R[(T_1, T_2) => U])(v1: R[T_1], v2: R[T_2]): R[U] = ???
  def __lam[U](f: () => R[U]): R[() => U] = ???
  def __lam[T_1, U](f: R[T_1] => R[U]): R[T_1 => U] = ???
  def __lam[T_1, T_2, U](f: (R[T_1], R[T_2]) => R[U]): R[(T_1, T_2) => U] = ???
}