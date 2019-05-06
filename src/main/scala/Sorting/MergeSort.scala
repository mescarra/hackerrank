package Sorting

object MergeSort {

  case class MergeSortList[A](list: List[A])(implicit order: Ordering[A]) {

    def sort(): List[A] = {
      def merge(l:List[A], r:List[A]): List[A] = {
        (l, r) match {
          case (xs, Nil)          => xs
          case (Nil, ys)          => ys
          case (x :: xs, y :: ys) => if (order.lteq(x, y)) x::merge(xs, r) else y:: merge(l, ys)
        }
      }

      def mergeSort(list: List[A]): List[A] =
        if (list.length <= 1) list
        else {
          val mid = list.length / 2
          val l = list.take(mid)
          val r = list.drop(mid)
          merge(mergeSort(l), mergeSort(r))
        }

      mergeSort(list)
    }
  }

  case class MergeSortImperative(array: Array[Int]) {
    var A = array
    var c = 0L

    def sort(): Unit = {

      def merge(p: Int, q: Int, r: Int): Unit = {
        val n1 = q - p + 1
        val n2 = r - q
        val L = Array.fill(n1 + 2)(Int.MaxValue)
        val R = Array.fill(n2 + 2)(Int.MaxValue)
        for (i <- 1 to n1)
          L(i) = A(p + i - 1)
        for (j <- 1 to n2)
          R(j) = A(q + j)

        var i = 1
        var j = 1
        for (k <- p to r) {
          if (L(i) <= R(j)) {
            A(k) = L(i)
            i += 1
          } else {
            A(k) = R(j)
            j += 1
            c += (n1 - i + 1)
          }
        }
      }

      def mergeSort(p: Int, r: Int): Unit = {
        if (p < r) {
          val q = (p + r) / 2
          mergeSort(p, q)
          mergeSort(q + 1, r)
          merge(p, q, r)
        }
      }

      mergeSort(0, A.length - 1)
    }
  }

}
