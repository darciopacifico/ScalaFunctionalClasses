package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }


  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }

  test("upsweep par should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val tree = upsweep(Array[Float](0f, 1f, 8f, 9f), 1, 4, 1)
    assert(tree.maxPrevious == 4f)
  }


  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("downsweep (parallel) should correctly handle a 4 element array when the starting angle is zero") {
    val input: Array[Float] = Array[Float](0f, 1f, 8f, 9f)
    val output = new Array[Float](4)
    val tree = upsweep(input,0,input.length,1)
    downsweep(input, output, 0f, tree)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("downsweep (seq) should correctly handle a 5 element array when the starting angle is zero") {
    //                                      0     1          2        3      4
    val input: Array[Float] = Array[Float]( 0.0f, 7.0f, 7.0f*2, 11.0f*3, 12f*4 )
    val output = new Array[Float](input.length)
    downsweepSequential(input, output, 0, 0, input.length)
    assert(output.toList == List(0.0f, 7.0f, 7.0f, 11.0f, 12.0f))
  }


  test("downsweep (parallel) should correctly handle a 5 element array when the starting angle is zero") {
    val input: Array[Float] = Array[Float]( 0.0f, 7.0f, 7.0f*2, 11.0f*3, 12f*4 )
    val output = new Array[Float](5)
    val tree = upsweep(input,0,input.length,1)
    downsweep(input, output, 0f, tree)
    assert(output.toList == List(0.0f, 7.0f, 7.0f, 11.0f, 12.0f))
  }


}


//0.0, 7.0, 7.0, 11.0, 12.0

