package Challenges
import collection.mutable.Map
import DS.Dictionary._

object Abbreviation {
  def abbreviation(a: String, b: String): String = {
    var map = Map.empty[Char,Int]

    def finish(i:Int): Boolean = {
      if (i == a.length) map.values.forall(_>=0)
      else {
        if (a(i).isUpper) addOrUpdate(map, a(i), (a(i), -1), ((x:Int) => x - 1))

        finish(i+1)
      }
    }

    def abbrIter(i:Int, j:Int): Boolean = {
      //println(i,j, map)
      if (j == b.length) finish(i)
      else if (i == a.length) false
      else if (a(i) == b(j)) abbrIter(i+1,j+1)
      else if (a(i).toUpper == b(j)) {
        val ku = a(i).toUpper
        addOrUpdate(map, ku, (ku, 1), ((x:Int) => x + 1))
        abbrIter(i+1,j+1)
      }
      else {
        if (a(i).isUpper) addOrUpdate(map, a(i), (a(i), -1), ((x:Int) => x - 1))
        abbrIter(i+1,j)
      }
    }

    if (abbrIter(0,0)) "YES"
    else "NO"
  }
}
