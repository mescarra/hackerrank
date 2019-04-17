package DS
import math._

object Arrays {

  case class Data(from: Int, to: Int, sum: Int) {
    def >(d2: Data): Boolean = (this, d2) match {
      case (x, y) if x.sum > y.sum => true
      case (x, y) if x.sum == y.sum && x.from < y.from => true
      case (x, y) if x.sum == y.sum && x.from == y.from && x.to < y.to => true
      case _ => false
    }

    def max(d2: Data): Data = if (this > d2) this else d2
  }

  case class SubArraySumSolver(array: Array[Int]) {

    def subarraySum(i: Int, j: Int): Int = array.slice(i, j).sum

    def largestSubArraySum: Data = {
      def findMaxCrossingArray(i: Int, mid: Int, j: Int): Data = {
        var sum = 0
        var maxLeft, maxRight = Data(Int.MaxValue, Int.MaxValue, -1)

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
          val res = Data(i, j, array(i))
          res
        }
        else {
          val mid = i + (j - i) / 2
          val l = lsum(i, mid)
          val r = lsum(mid, j)
          val t = findMaxCrossingArray(i, mid, j)
          val res = (l max t) max r
          res
        }
      }

      if (array.length == 0)
        Data(0, 0, 0)
      else
        lsum(0, array.length)
    }
  }

  def arrayOrdering(array: Array[Int], k: Int): List[Int] = {

    def merge(l: List[Data], r: List[Data]): List[Data] = (l, r) match {
      case (Nil, Nil) => Nil
      case (xs, Nil) => xs
      case (Nil, ys) => ys
      case (x :: xs, y :: ys) => if (x > y) x :: merge(xs, r) else y :: merge(l, ys)

    }

    def arrayOrderingTR(array: Array[Int]): List[Data] = {
      val solver = SubArraySumSolver(array)
      val data = solver.largestSubArraySum
      if (data.sum > 0) {
        val left = arrayOrderingTR(array.take(data.from))
        val right = arrayOrderingTR(array.drop(data.to))
        data :: merge(left, right)
      } else Nil
    }

    val x = arrayOrderingTR(array)
    x.map(_.sum).take(k)
  }

  case class RangeMinimumQuery(array: Array[Int]) {

    import Discrete.Arithmetic.lg

    val n = array.length
    val k = floor(lg(n)).toInt + 1
    var table: Array[Array[Int]] = Array.fill(k)(Array.fill(n)(Int.MaxValue))

    private def buildTable(): Unit = {
      table(0) = array

      for (j <- 1 until k) {
        for (i <- 0 until n) {
          val p = pow(2, j - 1).toInt
          if (i + p < n)
            table(j)(i) = min(table(j - 1)(i), table(j - 1)(i + p))
        }
      }
    }

    buildTable()

    def findMinInRange(l: Int, r: Int): Int = {
      val k = floor(log(r - l + 1) / log(2)).toInt
      val start = table(k)(l)
      val m = r - pow(2, k).toInt + 1
      val end = table(k)(m)
      min(start, end)
    }
  }
}

//  import scala.collection.mutable.LinkedHashMap
//  import DisjointSets._
//  case class SubArraySumSolver(array: Array[Int]) {
//
//
//    val arrayLength = array.length
//    var dictionary: LinkedHashMap[Int, Data] = LinkedHashMap.empty
//
//    // Disjoint Sets : Int
//    def initSets: DisjointSet[Int] = {
//      val sets = DisjointSet[Int]()
//      for (i <- 0 until arrayLength){
//        sets.makeSet(i)
//        dictionary += (i -> Data(i, i, array(i)))
//      }
//      sets
//    }
//
//    def dataPass(it: Iterable[(Int, Int)], sets: DisjointSet[Int], map: LinkedHashMap[Int, Data])
//          : (DisjointSet[Int], LinkedHashMap[Int, Data]) =
//      it match {
//        case Nil => (sets,map)
//        case (i,j)::xs =>
//          val ran1 = dictionary(i)
//          val ran2 = dictionary(j)
//          val sum  = ran1.sum + ran2.sum + array.slice(ran1.to+1, ran2.from).sum
//          if (sum >= ran1.sum && sum >= ran2.sum) {
//            (ran1.to+1 to ran2.from).map(x => sets.union(x-1, x))
//            map += (sets.findSet(ran1.from) -> Data(ran1.from, ran2.to, sum))
//          } else {
//            if (!map.keySet.contains(sets.findSet(i)) && ran1.sum > 0)
//              map += (i -> ran1)
//            if (!map.keySet.contains(sets.findSet(j)) && ran2.sum > 0)
//              map += (j -> ran2)
//          }
//
//          dataPass(xs, sets, map)
//      }
//
//    def buildSets(sets: DisjointSet[Int]): DisjointSet[Int] = {
//      if (dictionary.size == 0)
//        sets
//      else {
//        val ijs = (dictionary.keys.toList zip dictionary.keys.toList.tail)
//        val (newSets, newDictionary) = dataPass(ijs, sets, LinkedHashMap.empty)
//        if (newDictionary.size == dictionary.size)
//          newSets
//        else {
//          dictionary = newDictionary
//          if (dictionary.size == 1)
//            newSets
//          else
//            buildSets(newSets)
//        }
//      }
//    }
//
//    def takeSets(k:Int, sets: DisjointSet[Int]): List[Int] = {
//      val r = dictionary.keys.toList
//      r.sortWith{ (i:Int, j:Int) =>
//        (dictionary(i), dictionary(j)) match {
//          case (x,y) if x.sum > y.sum => true
//          case (x,y) if x.sum == y.sum && x.from < y.from => true
//          case (x,y) if x.sum == y.sum &&  x.from == y.from && x.to < y.to => true
//          case _ => false
//        }
//      }.map(dictionary(_).sum).take(k)
//    }
//
//    def arrayOrdering(k:Int): List[Int] = takeSets(k, buildSets(initSets))
//
//  }

