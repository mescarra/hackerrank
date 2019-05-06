package Challenges
import math._

object SpecialPalindrome {

  def gauss(n: Int): Long = (n*(n+1))/2

  def count(cs:List[Char], c: Char, l: Int): Long = cs match {
    case Nil => gauss(l)
    case x::xs =>
      if (x == c) {
        count(xs, c, l+1)
      } else {
        val rest = xs.takeWhile(y => (y == c))
        val r = min(rest.length, l)
        val sum = (gauss(l) + r)

        sum + count(xs, x, 1)
      }
  }

  def substrCount(n: Int, s: String): Long = {
    val cs = s.toList
    count(cs.tail, cs.head, 1)
  }



}
