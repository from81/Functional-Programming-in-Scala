package recfun

import scala.collection.mutable.ArrayDeque
import scala.collection.mutable.Stack

object RecFun extends RecFunInterface {
  //The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the two numbers above it.
  //Write a function that computes the elements of Pascal's triangle by means of a recursive process.
  //
  //Do this exercise by implementing the pascal function in Main.scala, which takes a column c and a row r,
  //counting from 0 and returns the number at that spot in the triangle. For example, pascal(0,2)=1, pascal(1,2)=2 and pascal(1,3)=3.
  //
  //    1
  //   1 1
  //  1 2 1
  // 1 3 3 1
  //1 4 6 4 1
  def pascal(c: Int, r: Int): Int = {
    def pascal_recursive (r: Int, current_row: Array[Int] , row_idx: Int = 0): Array[Int] = {
      /* strategy: copy previous row, append 1 from the tail, and for 1 to n-1 perform += previous_row(i-1)*/
      if (r == row_idx) {
        return current_row
      }
      var next_row = current_row :+ 1
      if (next_row.length > 2) {
        for (i <- 1 until next_row.length - 1) {
          next_row (i) = next_row (i) + current_row (i - 1)
        }
      }
      pascal_recursive (r, next_row, row_idx + 1)
    }

    // get r-th row
    var row = pascal_recursive (r, Array (1) )

    // get c-th column of the row
    row (c)
  }

  /* Exercise 2: Parentheses Balancing
  Write a recursive function which verifies the balancing of parentheses in a string,
  which we represent as a List[Char] not a String.

  Do this exercise by implementing the balance function in Main.scala.
  There are three methods on List[Char] that are useful for this exercise:

  - chars.isEmpty: Boolean returns whether a list is empty
  - chars.head: Char returns the first element of the list
  - chars.tail: List[Char] returns the list without the first element

  Hint: you can define an inner function if you need to pass extra parameters to your function.

  Testing: You can use the toList method to convert from a String to a List[Char]: e.g. "(just an) example".toList.
  * */
  def balance(chars: List[Char]): Boolean = {
    var n: Int = 0

    for (i <- 0 until chars.length) {
      chars(i) match {
        case '('  => n += 1
        case ')'  => n -= 1
        // catch the default with a variable so you can print it
        case _  => n += 0
      }
      if (n < 0) {
        return false
      }
    }
    n == 0
  }

  /* Exercise 3: Counting Change
  Write a recursive function that counts how many different ways you can make change for an amount, given a list of coin denominations.
  For example, there are 3 ways to give change for 4 if you have coins with denomiation 1 and 2: 1+1+1+1, 1+1+2, 2+2.

  Do this exercise by implementing the countChange function in Main.scala.
  This function takes an amount to change, and a list of unique denominations for the coins.

  Once again, you can make use of functions isEmpty, head and tail on the list of integers coins.

  Hint: Think of the degenerate cases.
  How many ways can you give change for 0 CHF?
  How many ways can you give change for >0 CHF, if you have no coins?
  */
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
