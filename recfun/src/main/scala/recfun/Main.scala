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
  def pascal(c: Int, r: Int): Int = 
    if (c == 0 || c == r) {
      return 1
    } else {
      return pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    
    /* Method to keep tally and check open parens */
    def open(chars2: List[Char], count: Int): Int = 
      // If count < 0, parens out of order, eg ')(' return false
      if (chars2.isEmpty || count < 0) {
        return count
      }
      else if (chars2.head == '(') {
        return open(chars2.tail, count +1)
      }
      else if (chars2.head == ')') {
        return open(chars2.tail, count - 1)
      }
      else {
        return open(chars2.tail, count)
      }

    /* check open method has even zero tally */
    return open(chars, 0) == 0
  }

  /**
   * Exercise 3
   * Idea is we iterate through the coin list and branch off into two directions.
   * The first direction is trying to deplete change with the first coin. This 
   * branch also further spins off the other branch, which gives up on the current 
   * coin and starts with the next coin
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    
    def numDivisible(change: Int, denominations: List[Int]): Int = 
      if (change < 0) {
        return 0
      }
      else if (change == 0) {
        return 1
      }
      else if (change > 0 && denominations.isEmpty) {
        return 0
      }
      else {
        val nextCoin = denominations.head
        val nextDenoms = denominations.tail
        return numDivisible(change - nextCoin, denominations) + numDivisible(change, nextDenoms)
      }
    
    return numDivisible(money, coins)
  }
}
