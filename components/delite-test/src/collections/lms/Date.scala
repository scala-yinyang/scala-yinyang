package collections.lms

case class Date(year: Int, month: Int, day: Int) {
  assert(month > 0 && month <= 12, "invalid month in date: " + month)
  assert(day > 0 && day <= 31, "invalid day in date: " + day)

  // Add interval
  def +(years: Int, months: Int, days: Int) = {
    import java.util.Calendar
    val date = new java.util.GregorianCalendar(year, month - 1, day)
    date.add(Calendar.YEAR, years)
    date.add(Calendar.MONTH, months)
    date.add(Calendar.DAY_OF_MONTH, days)
    Date(date.get(Calendar.YEAR), date.get(Calendar.MONTH) + 1, date.get(Calendar.DAY_OF_MONTH))
  }

  // Comparisons
  def <=(that: Date) = {
    if (year != that.year)
      year < that.year
    else if (month != that.month)
      month < that.month
    else day <= that.day
  }
  def >=(that: Date) = that <= this

  def <(that: Date) = (year < that.year) || (year == that.year && (month < that.month || (month == that.month && day < that.day)))
  def >(that: Date) = that < this

  override def toString() = year + "-" + month + "-" + day
}

object Date {
  def apply(s: String): Date = {
    val tokens = s.split("[-\\s:]")
    assert(tokens.size == 3, "expected 3 tokens in date, got: " + tokens.size + " in " + s)
    Date(tokens(0).toInt, tokens(1).toInt, tokens(2).toInt)
  }
}
