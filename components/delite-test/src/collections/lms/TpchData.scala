package collections.lms

object TpchData extends collections.TpchDataLike {
  type Date = collections.lms.Date
  def date(str: String): Date = collections.lms.Date(str)
}
