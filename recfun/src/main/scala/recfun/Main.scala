package recfun


import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    +-----------------------------------+  prog
   0|                 1                 |   => 0   i
   1|               1   1               |   => 1   p
   2|             1   2   1             |   => 2   i
   3|           1   3   3   1           |   => 3   p
   4|         1   4   6   4   1         |   => 4   i
   5|       1   5  10   10  5   1       |   => 5   p
   6|     1   6  15   20  15  6   1     |   =>     i
   7|   1   7  21  35   35  21  7   1   |   =>     p
   8| 1   8  28  56   70  56  28  8   1 |   =>     i
    +-----------------------------------+
      0   1   2   3    4   5   6  7   8
  **/

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {

    if(c<0) throw new IllegalArgumentException(  " c cannot be lower than 0 ")
    if(r<0) throw new IllegalArgumentException(  " r cannot be lower than 0 ")
    if(c>r) throw new IllegalArgumentException(  " c is greater then r ")

    def pascRec (c: Int, r: Int): Int={

      if(c>r || c<0){
        //doesn't take a value inside the triangle
        //just return the a
        //println("tentou um zero")
        0
      }else if (c==0 && r==0) {
        //return the top of pyramid
        //println("Topo")
        1
      }else{
        //
        pascRec(c-1, r-1) + pascRec(c,r-1)
      }
    }

    pascRec(c,r)

  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def balance(count: Int, chars: List[Char]):Boolean = {
      if(count<0){
        false
      }else{
        chars match {
          case ')' :: xs =>
            balance(count-1, xs)

          case '(' :: xs =>
            balance(count+1, xs)

          case _ :: xs =>
            balance(count, xs)

          case Nil =>
            count==0
        }
      }
    }

    balance(0,chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def count (n:Int , m :Int): Int ={

      if(n < 0 || m < 0){
        0 // or doesn't take a zeroed change or coins options have been finished
      }else if (n==0 ){ // take a zeroed change combination. Increment the counter
        1 // its ok.. take one
      }else{
        // these clever double call actually is a matrix path, that expands up to n*m calls
        // the stop condition of matrix formation is a change not zeroed or no more coin options
        count(n, m-1) + count(n - coins(m),m) // it' not over. Iterate again...
      }

    }

    count(money,coins.length-1)
  }
}




