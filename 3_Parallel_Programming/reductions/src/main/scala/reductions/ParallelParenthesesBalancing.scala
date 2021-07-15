package reductions

import scala.annotation.*
import org.scalameter.*
import util.control.Breaks._

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface:

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean =
    var n = 0
    breakable(
      for (char <- chars) do
        char match
          case '('  => n += 1
          case ')'  => n -= 1
          case _  =>
        if (n < 0) then break
    )
    n == 0
  end balance

  /** Implement the parBalance method, which checks if the parentheses in the input array
   * are balanced using two helper methods reduce and traverse.
   * These methods implement the parallel reduction and the sequential traversal part, respectively
   */

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean =

    def traverse(from: Int, until: Int, left: Int, right: Int): (Int, Int) =
      if from == until then (left, right)
      else chars(from) match
        case '('  => traverse(from+1, until, left+1, right)
        case ')'  =>
          if left > 0 then traverse(from+1, until, left-1, right)
          else traverse(from+1, until, left, right+1)
        case _  => traverse(from+1, until, left, right)
    end traverse

    def reduce(from: Int, until: Int): (Int, Int) =
      if until - from <= threshold then
        traverse(from, until, 0, 0)
      else
        val mid = from + (until - from) / 2
        val ((open_L, close_L), (open_R, close_R)) = parallel(reduce(from, mid), reduce(mid, until))
        if open_L > close_R then (open_L - close_R + open_R, open_L)
        else (open_R, close_R - open_L + close_L)
    end reduce

    reduce(0, chars.length) == (0, 0)
