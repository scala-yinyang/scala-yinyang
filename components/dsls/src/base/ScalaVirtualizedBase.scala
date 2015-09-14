package base

trait VirtualControlsBase extends RepBase {
  def __ifThenElse[T](cnd: R[Boolean], thn: R[T], els: R[T]): R[T]
  def __return(expr: R[Any]): R[Nothing]
  def __whileDo(cnd: R[Boolean], body: R[Unit]): R[Unit]
  def __doWhile(body: R[Unit], cond: R[Boolean]): R[Unit]
  def __try[T](body: R[T], b: R[Throwable => T], fin: R[T]): T
  def __throw(e: R[Throwable]): R[Nothing]
}

trait VirtualVariablesBase extends RepBase {
  def __valDef[T](init: R[T]): R[T]
  def __varDef[T](init: R[T]): R[T]
  def __lazyValDef[T](init: R[T]): R[T]
  def __read[T](init: R[T]): R[T]
  def __assign[T](lhs: R[T], rhs: R[T]): R[Unit]
}

trait VirtualFunctionsBase extends RepBase {
  def __app[U](f: R[() => U]): () => R[U]
  def __app[T_1, U](f: R[T_1 => U]): R[T_1] => R[U]
  def __app[T_1, T_2, U](f: R[(T_1, T_2) => U]): (R[T_1], R[T_2]) => R[U]

  def __lambda[U](f: () => R[U]): R[() => U]
  def __lambda[T_1, U](f: R[T_1] => R[U]): R[T_1 => U]
  def __lambda[T_1, T_2, U](f: (R[T_1], R[T_2]) => R[U]): R[(T_1, T_2) => U]
}
