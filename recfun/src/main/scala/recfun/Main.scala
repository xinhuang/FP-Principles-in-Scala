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
  def pascal(c: Int, r: Int): Int = pascal(r)(c)

  def pascal(r: Int): List[Int] = r match {
    case 0 => List(1)
    case n => {
      val preRow = (0 :: pascal(n - 1) ++ List(0))
      val adjacents = preRow.sliding(2)
      adjacents.flatMap { pair =>
        List(pair(0) + pair(1))
      }.toList
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = balance(0, chars)
  
  def balance(lCount: Int, chars: List[Char]): Boolean = chars match {
    case ')'::tail => if (lCount == 0) false else balance(lCount - 1, tail)
    case '('::tail => balance(lCount + 1, tail)
    case Nil => lCount == 0
    case _::tail => balance(lCount, tail)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0) 0
    else if (money == 0) 1
    else coins match {
      case Nil => 0
      case c::tail => countChange(money - c, coins) + countChange(money, tail)
    }
  }
}
