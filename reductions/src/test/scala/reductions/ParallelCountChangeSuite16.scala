package reductions

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import reductions.ParallelCountChange._

@RunWith(classOf[JUnitRunner])
class ParallelCountChangeSuite16 extends FunSuite {

  test("parCountChange should work as coursera site case") {
    val money: Int = 16
    val coins: List[Int] = List(1)
    val threshold: Threshold = moneyThreshold(16)
    val count: Int = parCountChange(money,coins,threshold)
    assert(count === countChange(money,coins))
  }


}
