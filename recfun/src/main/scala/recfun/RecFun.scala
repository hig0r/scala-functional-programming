package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println(countChange(10,List(1,2,5)))

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    (c, r) match {
      case (0, _) => 1
      case (c, r) if c == r => 1
      case (c, r) => pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    def run(xs: List[Char], l: Int): Boolean =
      (xs, l) match {
        case (Nil, l) => l == 0
        case (xs, l) if xs.head == ')' && l == 0 => false
        case (xs, l) if xs.head == '(' => run(xs.tail, l + 1)
        case (xs, l) if xs.head == ')' => run(xs.tail, l - 1)
        case _ => false
      }

    run(chars.filter(x => x == '(' || x == ')'), 0)
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    (money, coins) match {
      case (_, Nil) => 0
      case (0, _) => 1
      case (m, _) if m < 0 => 0
      case (money, coins) => {
        val xsCoins = coins.reverse.drop(1)
        val c = coins.last
        countChange(money, xsCoins) + countChange(money - c, coins)
      }
    }