package Challenges

/*
 * Triple Sum
 * https://www.hackerrank.com/challenges/triple-sum/problem
 *
 * Count the number of triplets in three different arrays such that p <=q && q>= r,  forall p in arr1, q in arr2, r in arr3
 */

object TripleSum {

  def triplets(a: Array[Int], b: Array[Int], c: Array[Int]): Long = {
    val as = a.sorted
    val bs = b.sorted
    val cs = c.sorted

    def tripletsTraverse(i:Int, j:Int, k:Int, res: Long): Long = {
      if (j == bs.length) res
      else {
        val pred = (as(i) <= bs(j)) && (bs(j) >= cs(k))
        if (pred)
        {
          val gain:Long = bs.length - j
          if (k < cs.length-1 && i < as.length-1) {
            if (as(i+1) <= cs(k+1))
              tripletsTraverse(i+1, j, k, res + gain)
            else
              tripletsTraverse(i, j, k+1, res + gain)
          } else if (i < as.length-1) {
            tripletsTraverse(i + 1, j, k, res + gain)
          } else if (k < cs.length-1) {
            tripletsTraverse(i, j, k + 1, res + gain)
          } else {
            tripletsTraverse(i, j+1, k, res+gain)
          }
        } else {
          tripletsTraverse(i, j+1, k, res)
        }
      }
    }

    tripletsTraverse(0,0,0,0L)
  }

}
