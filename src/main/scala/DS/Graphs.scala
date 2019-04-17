package DS

import math._
import DS.DisjointSets._

object Graphs {

  type Vertex = Int
  case class Edge(from:Int, to:Int)

  case class Graph(vertices: Set[Vertex], edges: Set[Edge])

  def makeConnectedComponents(g:Graph): DisjointSet[Vertex] = {
    val sets = new DisjointSet[Vertex]()

    for (v <- g.vertices)
      sets.makeSet(v)

    for (e <- g.edges) {
      val fs1 = sets.findSet(e.from)
      val fs2 = sets.findSet(e.to)
      if (fs1 != fs2)
        sets.link(fs1, fs2)
    }
    sets
  }

  def connectedComponents(g:Graph): Set[Graph] = {
    val sets = makeConnectedComponents(g)

    g.vertices.groupBy(sets.findSet).values.map { vs =>
      val es = g.edges.filter(e => vs.contains(e.from) || vs.contains(e.to))
      Graph(vs,es)
    }.toSet
  }

  def connectedComponentsSizes(g:Graph): List[Int] = {
    val sets = makeConnectedComponents(g)
    g.vertices.groupBy(sets.findSet).values.map(_.size).toList
  }

  def cost(inmates: Graph): Int = {
    val components = connectedComponentsSizes(inmates)
    components.map(g=>ceil(sqrt(g))).sum.toInt
  }
}
