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

    //    println("----------------")
    //    val s2 = ":-)"
    //    println(s"Balance $s2: ${balance(s2.toList)}")
    //
    //    val s3 = "())("
    //    println(s"Balance $s3: ${balance(s3.toList)}")
    //
    //    val s1 = "(if (zero? x) max (/ 1 x))"
    //    println(s"Balance $s1: ${balance(s1.toList)}")
    //    println("----------------")
    //    val ll = countChange(4, List(1, 2))
    //    println(s"countChange $ll")
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1 else
      pascal(c - 1, r - 1) + pascal(c, r - 1)


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def go(chars: List[Char], n: Int): Boolean = {
      if (chars.isEmpty) n == 0 else {
        if (n < 0) false else {
          if (chars.head == ')') go(chars.tail, n - 1) else {
            if (chars.head == '(') go(chars.tail, n + 1)
            else go(chars.tail, n)
          }
        }
      }
    }

    go(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1 else if (money < 0) 0 else if (coins.isEmpty) 0 else
      countChange(money - coins.head, coins) + countChange(money - coins.head, coins.tail)
  }
}
