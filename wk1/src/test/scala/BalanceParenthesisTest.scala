import org.scalatest.FunSuite

class BalanceParenthesisTest extends FunSuite {
  test("BalanceParenthesis.balance") {
    val solution = new BalanceParenthesis()
    assert(solution.balance("(if (zero? x) max (/ 1 x))".toList) == true)
    assert(solution.balance("I told him (that it's not (yet) done). (But he wasn't listening)".toList) == true)
    assert(solution.balance(":-)".toList) == false)
    assert(solution.balance("())(".toList) == false)
  }
}