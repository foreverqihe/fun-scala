package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println()
    println("balance")

    val tests = Array("()()", "(()", ")()(", "(ds)sdss()");
    tests.foreach { test => println(test + " : " + balance(test.toList)) }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) {
      1
    } else {
      pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def sub(chars: List[Char], count: Int): Int = {
      if (chars.isEmpty || count < 0) {
        count
      } else {
        if (chars.head == '(')
          sub(chars.tail, count + 1)
        else if (chars.head == ')')
          sub(chars.tail, count - 1)
        else
          sub(chars.tail, count)
      }
    }

    0 == sub(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def sub(money: Int, coins: List[Int], count: Int): Int = {
      if (coins.isEmpty) count
      else if (money == 0) 1 + count
      else if (coins.length == 1 && money % coins.head == 0) 1 + count
      else if (coins.head > money) sub(money, coins.tail, count)
      else {
        sub(money, coins.tail, count + sub(money - coins.head, coins, 0))
      }
    }
    sub(money, coins, 0)
  }
}
