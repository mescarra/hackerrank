package Challenges
import math._

/*
* Candies
* https://www.hackerrank.com/challenges/candies/problem
*
* Find the minimum number of candies for each children in O(n) using DP.
*/
object Candies {

  def candies(n: Int, arr: Array[Int]): Long = {
    var candies = Array.fill(n)(0L)

    def gt(i:Int, j:Int): Boolean = {
      if (j < 0 || j >= n) false
      else (arr(i) > arr(j))
    }

    def candiesDiscover(i:Int): Long = {
      if (i < 0 || i >= n) 0L
      else if (candies(i) > 0) candies(i)
      else {
        if (!gt(i, i-1) && !gt(i,i+1)) {
          candies(i) = 1
        } else if (gt(i,i-1) != gt(i,i+1)) {
          if (i > 0 && candies(i-1) > 0 && gt(i,i-1))
            candies(i) = candies(i-1) + 1
          else if (i < n-1 && candiesDiscover(i+1) > 0 && gt(i,i+1))
            candies(i) = candies(i+1) + 1
          else
            candies(i) = min(candiesDiscover(i-1), candiesDiscover(i+1)) + 1
        } else {
          candies(i) = max(candiesDiscover(i-1), candiesDiscover(i+1)) + 1
        }

        candies(i)
      }
    }

    var sum = 0L
    for (i <- 0 until n)
    {
      val c = candies(i)
      if (c == 0)
        sum += candiesDiscover(i)
      else
        sum += c
    }
    sum
  }


}
