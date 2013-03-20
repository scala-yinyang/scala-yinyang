package ch.epfl.lamp.yinyang.api

trait BaseYinYang {

  // TODO this is a hack
  def manifest[T]()(implicit m: Manifest[T]) = m

  def stagingAnalyze(): List[Int]

  trait LiftEvidence[T, Ret] {
    def hole(tpe: Manifest[Any], symbolId: Int): Ret
    def lift(v: T): Ret
  }

  def hole[T, Ret](tpe: Manifest[T], symbolId: Int)(implicit liftEv: LiftEvidence[T, Ret]): Ret =
    liftEv hole (tpe.asInstanceOf[Manifest[Any]], symbolId)

  def lift[T, Ret](v: T)(implicit liftEv: LiftEvidence[T, Ret]): Ret =
    liftEv lift (v)

}