package ch.epfl.yinyang

package object api {
  def unstage[T](block: => T): T = block
}
