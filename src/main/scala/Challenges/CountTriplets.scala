package Challenges

/*
 * Count Triplets
 * https://www.hackerrank.com/challenges/count-triplets-1/problem
 *
 * Count the number of triplets in a geometric progression in a given array in O(n)
 */
object CountTriplets {
  import DS.Dictionary.addOrUpdate
  import Discrete.Combinatorial.choose

  def countTriplets(arr: Array[Long], r: Long): Long = {
    if (r == 1) {
      arr.groupBy(identity).values.map(n => choose(n.length, 3)).sum
    }
    else {
      val mono: collection.mutable.Map[Long, Long] = collection.mutable.Map.empty
      val bi: collection.mutable.Map[Long, Long] = collection.mutable.Map.empty
      var sum:Long = 0
      for (i <- arr.indices){
        val n      = arr(i)
        val square = r*n
        val cube   = r*r*n
        addOrUpdate(mono, cube  , (cube,1L)  , (x:Long) => x+1L)
        mono.get(square).foreach(x => addOrUpdate(bi, square, (square,x), (y:Long) => x+y))
        bi.get(n).foreach{x => sum += x}
      }

      sum
    }
  }

  def main(args: Array[String]) {
    import io._

    val nr = StdIn.readLine.replaceAll("\\s+$", "").split(" ")
    val n = nr(0).toInt
    val r = nr(1).toLong
    val arr = StdIn.readLine.replaceAll("\\s+$", "").split(" ").map(_.trim.toLong)
    val ans = countTriplets(arr, r)

    println(ans)

  }

}
