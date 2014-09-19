package mpde.vector.test

import org.scalatest._
import dsl.la._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import reflect.runtime.universe._
import java.io.{ PrintStream, ByteArrayOutputStream }

@RunWith(classOf[JUnitRunner])
class TranslationSpec extends FlatSpec with ShouldMatchers {
  "Lambdas" should "be translated" in {
    la {
      val id = (x: Int) => x
      id
    }
  }
}
