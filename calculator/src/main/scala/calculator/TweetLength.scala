package calculator

object TweetLength {
  final val MaxTweetLength = 140

  def tweetRemainingCharsCount(tweetText: Signal[String]): Signal[Int] = {
    val MAX_LOW_SURROGATE = '\uDFFF';
    val MIN_LOW_SURROGATE = '\uDC00'
    val MAX_HIGH_SURROGATE = '\uDBFF'
    val MIN_HIGH_SURROGATE = '\uD800'


    def isLowSurrogate(ch: Char): Boolean = {
      ch >= MIN_LOW_SURROGATE && ch < (MAX_LOW_SURROGATE + 1)
    }

    def isHighSurrogate(ch: Char): Boolean = {
      ch >= MIN_HIGH_SURROGATE && ch < (MAX_HIGH_SURROGATE + 1)
    }


    /**
     *      if (isHighSurrogate(a[i++]) && i < endIndex &&
                isLowSurrogate(a[i])) {
                n--;
                i++;
            }
     * @param chars
     * @param curSize
     * @return
     */
    def checkSize(chars: List[Char], curSize: Int): Int = {
      chars match {
        case c1 :: c2 :: cs =>
          if(isHighSurrogate(c1) && isLowSurrogate(c2)){
            checkSize(cs,curSize-1)
          }else{
            checkSize(c2::cs,curSize)
          }
        case c1 :: cs =>
          checkSize(cs,curSize)
        case Nil =>
          curSize
      }
    }


    Signal {
      val text: String = tweetText.apply()
      //val count: Int = text.toCharArray.length
      val count = checkSize(text.toCharArray.toList,text.length)

      MaxTweetLength - count

    }
  }

  def colorForRemainingCharsCount(remainingCharsCount: Signal[Int]): Signal[String] = {
    Signal {
      /*
      If there are 15 or more characters left, the color "green"
      If there are between 0 and 14 characters left, included, the color "orange"
      Otherwise (if the remaining count is negative), the color "red"
      */

      if (remainingCharsCount.apply() >= 15) {
        "green"
      } else if (remainingCharsCount.apply() >= 0 && remainingCharsCount.apply() <= 14) {
        "orange"
      } else {
        "red"
      }

    }
  }

  /** Computes the length of a tweet, given its text string.
    * This is not equivalent to text.length, as tweet lengths count the number
    * of Unicode *code points* in the string.
    * Note that this is still a simplified view of the reality. Full details
    * can be found at
    * https://dev.twitter.com/overview/api/counting-characters
    */
  private def tweetLength(text: String): Int = {
    /* This should be simply text.codePointCount(0, text.length), but it
     * is not implemented in Scala.js 0.6.2.
     */
    if (text.isEmpty) 0
    else {
      text.length - text.init.zip(text.tail).count(
        (Character.isSurrogatePair _).tupled)
    }
  }
}
