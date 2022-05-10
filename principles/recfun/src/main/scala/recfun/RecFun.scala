package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do print(s"${pascal(col, row)} ")
      println()

  /** Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if c < 0 || c > r then 0
    else if r == 0 && c == 0 then 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /** Exercise 2
    */
  def balance(chars: List[Char]): Boolean =
    def equivalent(head: Char): Int =
      if head == '(' then 1 else if head == ')' then -1 else 0

    @tailrec
    def innerRec(countOpen: Int, left: List[Char]): Boolean =
      if left.isEmpty then countOpen == 0
      else
        val eq = equivalent(left.head)
        if countOpen == 0 && eq == -1 then false
        else innerRec(countOpen + eq, left.tail)

    innerRec(0, chars)

  /** Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
    if money == 0 then 1
    else if coins.isEmpty then 0
    else
      (if money - coins.head >= 0
       then countChange(money - coins.head, coins)
       else 0)
        + countChange(money, coins.tail)
