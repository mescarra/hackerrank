package DS

import scala.collection.mutable.LinkedHashMap

object DisjointSets {
  /*
   * Forest implementation
   */
  case class DisjointSet[A]() {

    case class Metadata[A](var parent: A, var rank: Int)
    type Data[A] = LinkedHashMap[A, Metadata[A]]

    var data: Data[A] = LinkedHashMap.empty

    def makeSet(x: A): Unit = {
      data += (x -> Metadata(x, 0))
    }

    def findSet(x:A): A = {
      if (x != data(x).parent)
        data(x).parent = findSet(data(x).parent)

      data(x).parent
    }

    def union(x:A, y:A): A = link(findSet(x), findSet(y))

    def link(x:A, y:A): A = {
      if (data(x).rank > data(y).rank){
        data(y).parent = x
        x
      } else {
        data(x).parent = y
        if (data(x).rank == data(y).rank)
          data(y).rank = data(y).rank + 1
        y
      }
    }

    override def toString: String = data.mkString(" ")
  }

}
