package Geometry

import scala.math._

object General {
  case class Point(x:Double, y:Double)

  def angle(a:Point, b:Point, up: Boolean): Double = {
    val ang = atan2(b.y - a.y, b.x - a.x)
    if (up && ang < 0)
      2*Pi + ang
    else if (up)
      ang
    else
      ang + Pi
  }

  def distance2(a:Point, b:Point): Double =  sqrt(pow(a.x - b.x,2) + pow(a.y - b.y,2))

  def perimeter(points: Array[Point]): Double = {
    def perimeterTR(i:Int): Double = {
      if (i == points.length) 0D
      else distance2(points(i), points((i+1)%points.length)) + perimeterTR(i+1)
    }

    perimeterTR(0)
  }

}
