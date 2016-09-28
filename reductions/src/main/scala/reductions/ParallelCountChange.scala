package reductions

import org.scalameter._
import common._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange {

  /** Returns the number of ways change can be made from the specified list of
    * coins for the specified amount of money.
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def count(money: Int, cIndex: Int): Int = {
      if(money==0){
        1
      } else if (money < 0 || cIndex < 0) {
        0 // or doesn't take a zeroed change or coins options have been finished
      } else if (money == 0) {
        // take a zeroed change combination. Increment the counter
        1 // its ok.. take one
      } else {
        // these clever double call actually is a matrix path, that expands up to n*m calls
        // the stop condition of matrix formation is a change not zeroed or no more coin options
        count(money, cIndex - 1) + count(money - coins(cIndex), cIndex) // it' not over. Iterate again...
      }

    }

    count(money, coins.length - 1)

  }


  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
    * specified list of coins for the specified amount of money.
    */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {

    def count(money: Int, coinId: Int): Int = {

      if (money < 0 || coinId < 0) 0 else if (money == 0) 1 else {

        if (threshold(money, coins)) {
          //countChange(money,coins)
          count(money, coinId - 1)+ count(money - coins(coinId), coinId)

        } else {
          val (countA, countB) = parallel(count(money, coinId - 1), count(money - coins(coinId), coinId))
          countA + countB // it' not over. Iterate again...

        }
      }
    }

    count(money, coins.length-1)
  }


  /**
   * Threshold heuristic based on the starting money.
   *
   * First, implement the moneyThreshold method, which creates a threshold function that
   * returns true when the amount of money is less than or equal to 2 / 3 of the starting amount:
   *
   * Remember that a / b will return the integer division of a and b when both operands are Ints.
   * To avoid this problem, be sure to always do the multiplication of startingMoney
   * by 2 before doing the division by 3.
   **/
  def moneyThreshold(startingMoney: Int): Threshold = {
    val limit = (startingMoney * 2) / 3

    (money: Int, coins: List[Int]) => {
      money <= limit
    }
  }


  /** Threshold heuristic based on the total number of initial coins.
    *
    * The previous heuristic did not take into account how many coins were left on the coins
    * list, so try two other heuristics. Implement the method totalCoinsThreshold, which returns
    * a threshold function that returns true when the number of coins is less
    * than or equal to the 2 / 3 of the initial number of coins:
    *
    * */
  def totalCoinsThreshold(totalCoins: Int): Threshold = {
    val limit = (totalCoins * 2) / 3

    (money: Int, coins: List[Int]) => {
      coins.length <= limit
    }
  }

  /** Threshold heuristic based on the starting money and the initial list of coins.
    *
    * Then, implement the method combinedThreshold, which returns a threshold function
    * that returns true when the
    * amount of money multiplied with the number of remaining coins
    * is less than or equal to the
    * starting money multiplied with the initial number of coins divided by 2:
    * */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {

    val limit = (startingMoney * allCoins.length) / 2

    (money: Int, coins: List[Int]) => {
      val x = money * coins.length
      x <= limit
    }
  }
}
