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
class Pascal {

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
}
