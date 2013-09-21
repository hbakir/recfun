package recfun
import common._

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
   * Exercise 1
   */
  def pascal(c: Int, r: => Int): Int = {
    if (c == 0 || r == 0 || r == 1) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceCount(chars: List[Char], count: Int): Int = {
      /* if the count is greater than zero for the tail, there is a mismatch */
      if (count >= 0 && !chars.isEmpty) {
        if (chars.head == '(') balanceCount(chars.tail, count + 1)
        else if (chars.head == ')') balanceCount(chars.tail, count - 1)
        else balanceCount(chars.tail, count)
      }else{
        count
      }
    }
    balanceCount(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    var count: Int = 0

    def countChangeIter(remain: Int, coins: List[Int]): Unit = {
      if (remain <= 0 || coins.isEmpty) /* nothing to do */ count = count + 0
      else countCoin(remain, 0, coins.head, coins.tail)
    }
    def countCoin(remain: Int, headCount: Int, head: Int, tail: List[Int]): Unit = {
      if (headCount * head == remain) {
        /* count increment condition is met */
        count = count + 1

        /* proceed with more coins */
        countCoin(remain, headCount + 1, head, tail)
      } else if (headCount * head < remain) {
        /* continue condition */
        countChangeIter(remain - headCount * head, tail)

        /* proceed with more coins */
        countCoin(remain, headCount + 1, head, tail)
      }
    }

    /* start process */
    countChangeIter(money, coins)

    count
  }

}
