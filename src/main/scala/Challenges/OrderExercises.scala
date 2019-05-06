package Challenges

import DS.Arrays.{Data, SubArraySumSolver}

/*
 * Order exercises
 * https://www.hackerrank.com/challenges/order-exercises/problem
 *
 * Find and sort by sum disjoint subarrays of a given array
 */
object OrderExercises {

  def arrayOrdering(array: Array[Long], k: Int): List[Long] = {

    def merge(l: List[Data], r: List[Data]): List[Data] = (l, r) match {
      case (Nil, Nil) => Nil
      case (xs, Nil) => xs
      case (Nil, ys) => ys
      case (x :: xs, y :: ys) => if (x > y) x :: merge(xs, r) else y :: merge(l, ys)
    }

    def arrayOrderingTR(array: Array[Long]): List[Data] = {
      val solver = SubArraySumSolver(array)
      val data = solver.largestSubArraySum
      if (data.sum > 0) {
        val left = arrayOrderingTR(array.take(data.from))
        val right = arrayOrderingTR(array.drop(data.to+1))
        data :: merge(left, right)
      } else Nil
    }

    val x = arrayOrderingTR(array)
    x.map(_.sum).take(k)
  }

}
