package Challenges
import collection.mutable.TreeSet

/*
 * Maximum Subarray Modular Sum
 * https://www.hackerrank.com/challenges/maximum-subarray-sum/problem
 *
 * Find maximum subarray modular sum in O(n lgn).
 */

object MaximumSubarrayModSum {
  def maximumSum(a: Array[Long], m: Long): Long = {
    var s = new TreeSet[Long]() // Red-Black Tree
    var prefixSum = 0L
    var max = 0L
    for (i <- a.indices) {
      prefixSum = (prefixSum + a(i))%m
      if (prefixSum > max) max = prefixSum
      s += prefixSum
      val ls = s.iteratorFrom(prefixSum+1)
      if (ls.hasNext) {
        val contender = (prefixSum - ls.next + m)%m
        if (contender > max) max = contender
      }
    }
    max
  }
}
