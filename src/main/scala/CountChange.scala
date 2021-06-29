/* Exercise 3: Counting Change
Write a recursive function that counts how many different ways you can make change for an amount, given a list of coin denominations.
For example, there are 3 ways to give change for 4 if you have coins with denomiation 1 and 2: 1+1+1+1, 1+1+2, 2+2.

Do this exercise by implementing the countChange function in Main.scala.
This function takes an amount to change, and a list of unique denominations for the coins.

Once again, you can make use of functions isEmpty, head and tail on the list of integers coins.

Hint: Think of the degenerate cases.
How many ways can you give change for 0 CHF?
How many ways can you give change for >0 CHF, if you have no coins?*/

import scala.collection.mutable.ArrayDeque
import scala.collection.mutable.Stack

class CountChange {
  def countChange(money: Int, coins: List[Int]): Int = {
    var combinations = new Stack[Stack[Int]]()

    def get_combinations(
                          money: Int,
                          coins: ArrayDeque[Int],
                          combinations: => ArrayDeque[Stack[Int]],
                          change: Stack[Int] = Stack()
                        ): Unit = {

      if (money > 0 && coins.nonEmpty) {
        for (_ <- 0 until coins.length) {
          var remaining_money = money
          val coin = coins(0)
          coins.dropInPlace(1)

          var updated_change = change.clone()
          while (remaining_money >= coin) {
            updated_change.push(coin)
            remaining_money -= coin
            get_combinations(remaining_money, coins.clone(), combinations, updated_change.clone())
          }
        }
      }
      else if (money == 0 && change.nonEmpty) {
        combinations.addOne(change.clone())
      }
    }

    val coins_deque = new ArrayDeque[Int]().addAll(coins)
    get_combinations(money, coins_deque, combinations)
    combinations.length
  }
}
