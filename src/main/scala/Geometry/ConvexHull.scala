package Geometry

import General._

object ConvexHull {

  /*
    * Jarvis's march implementation: O(nh), where h is the number of points in the hull
    */
  def convexHull(points: Array[Point], smallest:Boolean = true): Array[Point] = {
    val pointSet = points.distinct
    val lowest = pointSet.minBy(_.y)
    val highest = pointSet.maxBy(_.y)
    var goingUp = true

    def nextPoint(cur: Point, points: Array[Point]): (Point, Array[Point]) = {
      def nextPointTR(i:Int, minAng: Double, minPoint: Int): Int =
        if (i == points.length)
          minPoint
        else {
          val p = points(i)
          if (cur == p)
            nextPointTR(i+1, minAng, minPoint)
          else {
            val newAng = angle(cur, p, goingUp)
            if (newAng == minAng) {
              val d1 = distance2(cur, p)
              val d2 = if (minPoint == 0) d1 else distance2(cur, points(minPoint))
              val comp = if (smallest) d1 > d2 else d1 < d2
              if (points(minPoint) != lowest && comp) {
                nextPointTR(i+1, newAng, i)
              } else {
                nextPointTR(i+1, newAng, minPoint)
              }
            } else if (newAng < minAng) {
              nextPointTR(i+1, newAng, i)
            } else {
              nextPointTR(i+1, minAng, minPoint)
            }
          }
        }

      val p = nextPointTR(0, Double.MaxValue, -1)
      val ps = points.take(p) ++ points.drop(p+1)
      (points(p), ps)
    }

    def chain(cur: Point, points: Array[Point]): Array[Point] = {
      val (chi,ps) = nextPoint(cur, points)
      if (chi == lowest)
        Array(lowest)
      else {
        if (chi == highest)
          goingUp = false

        val l = chain(chi, ps)
        chi +: l
      }
    }

    if (points.length == 0)
      Array.empty
    else
      chain(lowest, pointSet)
  }

  def IsConvex(poly: Array[Point]): Boolean = {
    convexHull(poly, false).length == poly.length
  }

}
