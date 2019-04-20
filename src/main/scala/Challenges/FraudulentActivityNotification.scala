package Challenges

import Sorting.CountingSort.CountingSort

/*
 * Fradulent Activity Notification
 * https://www.hackerrank.com/challenges/fraudulent-activity-notifications/problem
 *
 * Traverse an array looking at the median of the last d elements and comparing it to the current value
 */
object FraudulentActivityNotification {

  def activityNotifications(expenditure: Array[Int], d: Int): Int = {
    val cs = new CountingSort(expenditure.slice(0, d), Some(expenditure.max))
    def anTR(i:Int): Int = {
      if (i >= expenditure.length) 0
      else {
        if (i > d) {
          cs.delete(expenditure(i-d-1))
          cs.insert(expenditure(i-1))
        }
        val median = cs.getMedian()
        if (expenditure(i) >= 2*median)
          anTR(i+1) + 1
        else
          anTR(i+1)
      }
    }
    anTR(d)
  }

}
