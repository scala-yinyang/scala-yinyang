package ch.epfl.lamp.yinyang.api

trait BaseYinYang {

  def stagingAnalyze(allHoles: List[scala.Int]): List[Int]

  abstract class LiftEvidence[T: Manifest, Ret] {
    def hole(tpe: Manifest[Any], symbolId: Int): Ret
    def lift(v: T): Ret
  }

  def hole[T, Ret](tpe: Manifest[T], symbolId: Int)(implicit liftEv: LiftEvidence[T, Ret]): Ret =
    liftEv hole (tpe.asInstanceOf[Manifest[Any]], symbolId)

  def lift[T, Ret](v: T)(implicit liftEv: LiftEvidence[T, Ret]): Ret =
    liftEv lift (v)

}