package DP

import scala.math.{floor, log, min, pow}

/*
 * Range Minimum Query
 * https://www.hackerrank.com/challenges/range-minimum-query/problem
 *
 * Find Minimum in a range in O(1) time, having precomputed a table using DP in O(nlgn)
 */

object RangeMinimumQuery {
  case class RangeMinimumQuery(array: Array[Int]) {

    import Discrete.Arithmetic.lg

    val n = array.length
    val k = floor(lg(n)).toInt + 1
    var table: Array[Array[Int]] = Array.fill(k)(Array.fill(n)(Int.MaxValue))

    private def buildTable(): Unit = {
      table(0) = array

      for (j <- 1 until k) {
        for (i <- 0 until n) {
          val p = pow(2, j - 1).toInt
          if (i + p < n)
            table(j)(i) = min(table(j - 1)(i), table(j - 1)(i + p))
        }
      }
    }

    buildTable()

    def findMinInRange(l: Int, r: Int): Int = {
      val k = floor(log(r - l + 1) / log(2)).toInt
      val start = table(k)(l)
      val m = r - pow(2, k).toInt + 1
      val end = table(k)(m)
      min(start, end)
    }
  }
}
