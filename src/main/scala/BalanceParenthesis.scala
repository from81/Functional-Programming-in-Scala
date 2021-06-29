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

class BalanceParenthesis {
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
}
