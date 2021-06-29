import org.scalatest.FunSuite

class CountChangeTest extends FunSuite {
  test("CountChange.balance") {
    val solution = new CountChange()
    assert(solution.countChange(4, List(1, 2)) == 3)
  }
}