package calculator

import calculator.TweetLength.MaxTweetLength
import org.junit.runner.RunWith
import org.scalatest.{FunSuite, _}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /** ****************
    * * TWEET LENGTH **
    * *****************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  //-3.0 was not 12.0 plus or minus 1.0E-5



  test("regular formula") {

    val a: Signal[Expr] = Signal(Literal(2))
    val b: Signal[Expr] = Signal(Literal(3))
    val c: Signal[Expr] = Signal(Times(a.apply(), b.apply()))

    val res = Calculator.computeValues(Map(
      "a" -> a,
      "b" -> b,
      "c" -> c))

    assert(res("c").apply() == 6)

  }

  test("cyclic formula") {

    val a: Signal[Expr] = Signal(Literal(2))
    val b: Signal[Expr] = Signal(Ref("c"))
    val c: Signal[Expr] = Signal(Times(a.apply(), b.apply()))

    val res = Calculator.computeValues(Map(
      "a" -> a,
      "b" -> b,
      "c" -> c))

    assert(res("c").apply().isNaN)

  }


  test("polynimial test 1") {
    val a = Signal(-3.0)
    val b = Signal(2.0)
    val c = Signal(1.0)

    val d = Polynomial.computeDelta(a, b, c)

    assert(d.apply() == 16.0)

    val res = Polynomial.computeSolutions(a,b,c,d)

    assert(res.apply().contains(-1.0/3.0) && res.apply().contains(1))

  }

  test("polynimial test 2") {
    val a = Signal(3.0)
    val b = Signal(2.0)
    val c = Signal(1.0)

    val d = Polynomial.computeDelta(a, b, c)

    assert(d.apply() == -8)

    val res = Polynomial.computeSolutions(a,b,c,d)

    assert(res().isEmpty)

  }


  test("polynimial test 3") {
    val a = Signal(-31.12323)
    val b = Signal(123.21212)
    val c = Signal(1.4234)

    val d = Polynomial.computeDelta(a, b, c)

    //assert(d.apply() == 15358.430)

    val res = Polynomial.computeSolutions(a,b,c,d)

    assert(res.apply().contains(-0.012) && res.apply().contains(3.970))

  }

}
