package base
import ch.epfl.yinyang.api._

trait Base extends BaseYinYangTypeTag with CodeGenerator {
  def main(): Any
}

trait PolymorphicBase extends Base {
  type R[+T]
}
