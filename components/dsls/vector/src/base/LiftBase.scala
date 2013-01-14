package base

/*
 * This is a prototype implementation of the embedded DSL. In this prototype we will use the of 
 * polymorphic embedding of DSLs.  
 * 
 * DSL. Once the library is complete all method implementations should
 * remain empty so users can build their own embedded compilers. There is need to enforce more than lifting on the library user.  
 */
trait LiftBase {

  trait LiftEvidence[T, U] {
    def lift(v: T): U
  }

  def liftTerm[T, Ret](v: T)(implicit liftEv: LiftEvidence[T, Ret]): Ret = liftEv.lift(v)
}