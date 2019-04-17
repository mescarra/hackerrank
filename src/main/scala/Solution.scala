import DS.Arrays.RangeMinimumQuery

object Solution {
  def main(args: Array[String]): Unit = {
    import scala.io.StdIn
    val nm = StdIn.readLine.trim.split(" ").map(_.toInt)
    val ls = StdIn.readLine.trim.split(" ").map(_.toInt)
    val rmq = RangeMinimumQuery(ls)

    for (_ <- 0 until nm(1)){
      val ij = StdIn.readLine.trim.split(" ").map(_.toInt)
      println(rmq.findMinInRange(ij(0), ij(1)))
    }
  }
}