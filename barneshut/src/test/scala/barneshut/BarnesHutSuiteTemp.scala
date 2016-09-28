package barneshut

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.math._
import scala.collection.parallel._
import barneshut.conctrees.ConcBuffer

@RunWith(classOf[JUnitRunner])
class BarnesHutSuiteTest extends FunSuite {

  // test cases for quad tree

import FloatOps._

  test("Fork with 4 empty quadrants"){

    val f = Fork(Empty(0,0,0),Empty(0,0,0),Empty(0,0,0),Empty(0,0,0))

    assert(f.mass==0.0)

    assert(f.massX == 0.0)

  }
}
