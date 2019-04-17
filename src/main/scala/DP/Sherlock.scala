package DP

/*
 * Sherlock and the Maze
 * https://www.hackerrank.com/challenges/sherlock-and-the-maze/problem
 *
 * Number of Right/Down paths in a board to position (N,M) having at most K turns
 */
object Sherlock {

  val MAX = (1e9+7).toLong

  trait Data
  case object Unvisited extends Data
  case class Visited(pathsDown: Array[Long], pathsRight: Array[Long]) extends Data
  val tableWidth = 100
  val tableHeight = 100
  val maxTurns = 100

  var dpTable: Array[Array[Data]] = Array.fill(tableHeight)(Array.fill(tableWidth)(Unvisited))
  dpTable(0)(0) = Visited(Array.empty, Array.empty)

  def merge(a:Array[Long], b:Array[Long]):Array[Long] = {
    if (a.length > 0 && b.length > 0)
      ((a.head + b.head)%MAX) +: merge(a.tail, b.tail)
    else if (a.length == 0)
      b
    else
      a
  }

  def explore(row:Int, col:Int): Visited = dpTable(row)(col) match {
    case Visited(a,b) => Visited(a,b)
    case Unvisited if col == 0 => Visited(Array(1), Array.empty)
    case Unvisited if row == 0 => Visited(Array.empty, Array(1))
    case _ =>
      val left = explore(row, col-1) //Visited(arr,arr)
    val up   = explore(row-1, col)

      val Visited(dl,rl) = left
      val leftDown  = 0L+:dl
      val leftRight = rl

      val Visited(du,ru) = up
      val upDown  = du
      val upRight = 0L+:ru

      val res = Visited(merge(upDown, upRight), merge(leftDown, leftRight))
      dpTable(row)(col) = res
      res
  }

  def getMaxWithKTurns(row:Int, col:Int, k:Int): Long = {
    if (row==1 && col==1) 1
    else {
      val Visited(a1, a2) = explore(row-1,col-1)
      (a1.take(k+1).sum + a2.take(k+1).sum)%MAX
    }
  }
}
