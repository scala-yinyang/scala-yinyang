package mpde.vector.test

import dsl.la.norep._
import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

class required extends scala.annotation.StaticAnnotation

@RunWith(classOf[JUnitRunner])
class OptionalStagingSpec extends FlatSpec with ShouldMatchers {

  def matches(text: String, regex: String @required): Boolean = text.matches(regex)

  /*
   * If all required values are present DSL compilation (staging) can be done completely at compile time.
   * Otherwise compilation is done at runtime.
   *
   * Idea: Add the warning system for compilation that is moved to runtime.
   * Idea: An optimization step (or analysis phase) can follow the decision on weather the required values are present.
   *   - matches("texttomatch", "text" + "to" + "match") should be accepted at compilation time. This requires DSL developer input since context of operations is unknown.
   *
   * Pros: Reduced initial runtime cost.
   *       Additional analysis that can be done at compile time that is not type system related.
   *
   * Cons: Nothing
   *
   * Scala implementation for compile time generation:
   *   If staging is run at compile time the macro should produce:
   *     val code = eval {
   *       val dsl = new DSL {
   *         def main() = body
   *       }
   *       dsl.generateCode
   *     }
   *     addToClassPath(new ScalaCompiler.compile(code))
   *
   *     c.Expr[T](newMethod(missing parameters))
   *     // how to inject Scala code here and compile it again?
   *       // 1. It is doable with quasiquotes.
   *       // 2. the method can be generated with all the missing values as arguments
   *          Add an object to the class path with the DSL method:
   *     )
   *
   * On the DSL side:
   *   The DSL must support holes in its framework. The holes must have a name and a type. The names of all holes must be in the method signature.
   *   Also the order of the holes must match the order that the macro will give.
   */
  //  "Constant as an argument" should "be staged at compilation" in {
  //    matches("abc", "abc") should equal (true)
  //  }
  //
  //  "Constant in a val" should "be staged at compilation" {
  //    val regex = "abc"
  ////    matches("abc", regex) shouldBe true
  //  }
  //
  //  "Constant in a var" should "be staged at runtime" {
  //    var regex = "abd"
  //    regex = "abc"
  ////    matches("abc", regex) should equal (true)
  //     1
  //  }

}
