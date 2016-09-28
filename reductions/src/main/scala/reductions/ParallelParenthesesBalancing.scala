package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {

    @tailrec
    def balance(count: Int, chars: List[Char]): Boolean = {
      if (count < 0)
        false
      else {
        chars match {
          case ')' :: xs =>
            balance(count - 1, xs)
          case '(' :: xs =>
            balance(count + 1, xs)
          case _ :: xs =>
            balance(count, xs)
          case Nil =>
            count == 0
        }
      }
    }

    balance(0, chars.toList)
  }


  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {


    def traverse(from: Int, until: Int, balOpens: Int, balCloses: Int): (Int, Int) = {

      def traverse(listChar: List[Char], balOpens: Int, balCloses: Int): (Int, Int) =
        listChar match {
          case Nil =>
            (balOpens, balCloses) // finally

          case '(' :: tailChars =>
            traverse(tailChars, balOpens + 1, balCloses) // incrementing opening brace

          case ')' :: tailChars if balOpens > 0 =>
            traverse(tailChars, balOpens - 1, balCloses) // OK. compensating some opened brace

          case ')' :: tailChars =>
            traverse(tailChars, balOpens, balCloses + 1) // extra closes at this side

          case _ :: tailChars =>
            traverse(tailChars, balOpens, balCloses) // nothing todo. ignore char
        }


      traverse(chars.view.slice(from, until).toList, balOpens, balCloses)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {

      if (until - from <= threshold)
        traverse(from, until, 0, 0)

      else {
        val median = (from + until) / 2

        val ((lOpens, lCloses), (rOpens, rCloses)) = parallel(reduce(from, median), reduce(median, until))

        val bal = scala.math.min(lOpens, rCloses) // compensate extra opens and extra closes

        (lOpens + rOpens - bal, lCloses + rCloses - bal)

      }

    }

    reduce(0, chars.length) ==(0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
