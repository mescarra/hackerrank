package Greedy

/*
 * Triple Sum
 * https://www.hackerrank.com/challenges/triple-sum/problem
 *
 * Count the number of triplets in three different arrays such that p <=q && q>= r,  forall p in arr1, q in arr2, r in arr3
 */
object TripleSum {
  import math._

  def forward(a:Array[Int], i: Int, l:Int): Int = {
    if(i >= a.length) a.length
    else if (a(i) <= l) forward(a,i+1,l)
    else i
  }

  def triplets(a: Array[Int], b: Array[Int], c: Array[Int]): Long = {
    var as = a.distinct.sorted
    val bs = b.distinct.sorted
    var cs = c.distinct.sorted

    var sum = 0L
    var i,k = 0

    for (j <- bs.indices) {
      i = forward(as,i, bs(j))
      k = forward(cs,k, bs(j))

      sum += (i * k)
    }

    sum
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val lenaLenbLenc = stdin.readLine.split(" ")

    val lena = lenaLenbLenc(0).trim.toInt
    val lenb = lenaLenbLenc(1).trim.toInt
    val lenc = lenaLenbLenc(2).trim.toInt

    val arra = stdin.readLine.split(" ").map(_.trim.toInt)

    val arrb = stdin.readLine.split(" ").map(_.trim.toInt)

    val arrc = stdin.readLine.split(" ").map(_.trim.toInt)
    val ans = triplets(arra, arrb, arrc)

    println(ans)
  }

}
