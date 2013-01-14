package base

trait Interpret {
  
  def main()
  
  def interpret() = {
    val res = main()
    // return a regular scala type (Unit for now)
    ()
  }
  
}