package collections

import collections.Date

object Date {
  def apply(str: String): Date = new Date(str)
  private[collections] def apply(d: Date): Date = new Date(d.toString)
}

class Date(str: String) {

  private[collections] val inner: Date = Date(str)

  def <=(rd: Date): Boolean = inner <= rd.inner

  def >=(rd: Date): Boolean = inner >= rd.inner

  def <(rd: Date): Boolean = inner < rd.inner

  def >(rd: Date): Boolean = inner > rd.inner

  def +(years: Int, months: Int, days: Int): Date = Date(inner + (years, months, days))

}