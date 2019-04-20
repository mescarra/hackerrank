package Sorting

import math._

object CountingSort {

  // Assumes non-negative integers
  case class CountingSort(A: Array[Int], m:Option[Int] = None) {
    val n = A.length
    val B = A
    val maxInt = m.getOrElse(A.max) + 1
    val C = Array.fill(maxInt)(0)

    def insert(x:Int): Unit = C(x) += 1

    def delete(x:Int): Unit = C(x) = max(C(x)-1, 0)

    private def initCount(): Unit = {
      for (i <- A.indices)
        insert(A(i))
    }
    initCount()

    def sort(): Unit = {
      var k = 0
      for (i <- C.indices)
        for (j <- 0 until C(i)) {
          B(k) = i
          k += 1
        }
    }

    def getSortedArray: Array[Int] = B

    def getMedian(): Double = {
      var sum = 0
      var last = 0
      for (i <- C.indices)
      {
        if (C(i) > 0){
          sum += C(i)
          if (sum > n/2) {
            if (n % 2 == 1 || (sum - C(i)) < n/2) return i.toDouble
            else return (i.toDouble + last.toDouble)/2D
          }
          last = i
        }
      }
      throw new Exception("Invalid Count Array")
    }
  }

}
