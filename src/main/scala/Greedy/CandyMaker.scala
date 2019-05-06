package Greedy

import math._

/*
 * Making Candies
 * https://www.hackerrank.com/challenges/making-candies/problem
 *
 * O(sqrt(N))
 */

object CandyMaker {

  def minimumPasses(m: Long, w: Long, p: Long, n: Long): Long = {

    def purchasing(machines: Long, workers: Long, candies: Long): (Long, Long, Long) = {
      val purch:Long = candies/p
      val save = candies%p

      if (workers >= machines + purch) {
        (machines+purch, workers, save)
      } else if (machines >= workers + purch){
        (machines, workers+purch, save)
      } else {
        val total:Long = purch + machines + workers
        val newMachines = total/2L
        (newMachines, total - newMachines, save)
      }
    }

    def fastForwardMake(candies:Long, curProdCapacity: Long): Long = {
      if (candies > p) 0L
      else {
        val goal = min(p-candies, n)
        ceil(goal/curProdCapacity.toDouble).toLong
      }
    }

    def minimumPassesIter(machines: Long, workers: Long, candies:Long, iters:Long, savingIters:Long): Long = {
      if (machines > Long.MaxValue/workers) (iters+1)
      else {
        val curProdCapacity = machines*workers
        val newIters = fastForwardMake(candies, curProdCapacity)
        val curIters = iters + newIters
        val newCandies = candies + newIters*curProdCapacity

        if (newCandies >= n) {
          min(curIters, savingIters)
        } else {
          val savingItersLeft = ceil((n - newCandies)/curProdCapacity.toDouble).toLong
          val (newMachines, newWorkers, candiesLeft) = purchasing(machines, workers, newCandies)
          val newSavingIters = min(savingIters, curIters + savingItersLeft)
          if (newSavingIters <= curIters)
            newSavingIters
          else
            minimumPassesIter(newMachines, newWorkers, candiesLeft, curIters, newSavingIters)
        }
      }
    }

    minimumPassesIter(m,w,0L,0L, Long.MaxValue)
  }
}
