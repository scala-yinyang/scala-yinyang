package mpde.vector.test

import org.scalatest._
import dsl.la._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import reflect.runtime.universe._
import java.io.{ PrintStream, ByteArrayOutputStream }

@RunWith(classOf[JUnitRunner])
class GenericTranslationSpec extends FlatSpec with ShouldMatchers {
  "Generic translation" should "work for val definitions" in {
    intercept[NotImplementedError] {
      la {
        val v = 1
        v
      }
    }
  }

  it should "work for vars" in {
    intercept[NotImplementedError] {
      la {
        var v = 1
        v = 2
        v
      }
    }
  }

  it should "work for lambdas" in {
    intercept[NotImplementedError] {
      la {
        val id = (x: Int) => x
        id
      }
    }
  }

  it should "work for application" in {
    intercept[NotImplementedError] {
      la {
        val id = (x: Int) => x
        id(1)
      }
    }
  }

  it should "work for all types of application" in {
    intercept[NotImplementedError] {
      la {
        val id0 = () => 1
        val id1 = (x: Int) => x
        val id2 = (x: Int, y: Int) => x + y
        id0()
        id1(1)
        id2(1, 2)
      }
    }
  }

  it should "work for AnyRef functions" in {
    intercept[NotImplementedError] {
      la {
        val id0 = 1
        val hash = 1.hashCode
        val hash2 = 2.hashCode
        hash != hash2
      }
    }
  }

  it should "work for locally defined functions" in {
    intercept[NotImplementedError] {
      val x: Int = la {
        def id[T](x: T): T = x
        def id2[T, U <: AnyRef](p1: T, p2: U): Unit = ()
        id2[Int, String](1, "string")
        id[Int](1)
      }
    }
  }

  it should "work for type aliases" in {
    intercept[NotImplementedError] {
      la {
        type X = Int
        type Y = dsl.la.Vector[X]
        val x: Y = dsl.la.Vector(1, 2, 3)
        x
      }
    }
  }

  it should "work with captured variables" in {
    val captured = 1
    val captured1 = 2
    val captured2 = 3
    intercept[NotImplementedError] {
      la {
        val x: Int = captured + captured1 + captured2
        x + 1
      }
    }
  }

}
