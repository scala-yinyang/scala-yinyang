package base
import ch.epfl.yinyang.api._

trait Base extends BaseYinYangTypeTag {
  def main(): Any
}

trait RepBase extends Base {
  type R[T]
}