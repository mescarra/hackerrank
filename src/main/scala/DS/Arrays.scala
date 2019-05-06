package DS

object Arrays {

  case class Data(from: Int, to: Int, sum: Long) {
    def >(d2: Data): Boolean = (this, d2) match {
      case (x, y) if x.sum > y.sum => true
      case (x, y) if x.sum == y.sum && x.from < y.from => true
      case (x, y) if x.sum == y.sum && x.from == y.from && x.to < y.to => true
      case _ => false
    }

    def max(d2: Data): Data = if (this > d2) this else d2
  }

  case class SubArraySumSolver(array: Array[Long]) {

    def subarraySum(i: Int, j: Int): Long = array.slice(i, j).sum

    def largestSubArraySum: Data = {
      def findMaxCrossingArray(i: Int, mid: Int, j: Int): Data = {
        var sum = 0L
        var maxLeft, maxRight = Data(Int.MaxValue, Int.MaxValue, Long.MinValue)

        for (k <- (mid - 1) to i by -1) {
          sum += array(k)
          val newRange = Data(k, mid, sum)
          if (newRange > maxLeft)
            maxLeft = newRange
        }

        sum = 0
        for (k <- mid until j) {
          sum += array(k)
          val newRange = Data(mid, k, sum)
          if (newRange > maxRight)
            maxRight = newRange
        }

        Data(maxLeft.from, maxRight.to, maxLeft.sum + maxRight.sum)
      }

      def lsum(i: Int, j: Int): Data = {
        if ((j - i) == 1) {
          Data(i, j, array(i))
        }
        else {
          val mid = (j + i) / 2
          val l = lsum(i, mid)
          val r = lsum(mid, j)
          val t = findMaxCrossingArray(i, mid, j)
          (l max t) max r
        }
      }

      if (array.length == 0)
        Data(0, 0, 0)
      else
        lsum(0, array.length)
    }


  }

}