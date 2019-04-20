package Sorting

object BubbleSort {

  case class BubbleSort[A](array: Array[A])(implicit order: Ordering[A]) {
    var sorting = array
    def swap(i:Int, j:Int): Unit = {
      val x = sorting(i)
      sorting(i) = sorting(j)
      sorting(j) = x
    }

    def sort(): (Array[A], Int) = {
      var swapCount = 0
      for (_ <- sorting.indices)
        for (j <- 0 until sorting.length - 1)
          if (order.gt(sorting(j), sorting(j+1))){
            swap(j, j+1)
            swapCount += 1
          }

      (sorting, swapCount)
    }

  }
}
