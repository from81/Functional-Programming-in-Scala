import org.scalatest.FunSuite

class PascalTest extends FunSuite {
  test("Pascal.get_pascal") {
    val solution = new Pascal()
    assert(solution.pascal(2, 4) == 6)
    assert(solution.pascal(0, 2) == 1)
    assert(solution.pascal(1, 2) == 2)
    assert(solution.pascal(1, 3) == 3)
  }
}