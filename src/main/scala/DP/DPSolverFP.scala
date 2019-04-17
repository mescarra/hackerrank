package DP

object DPSolverFP {

  trait Data[A]

  case class  Unvisited[A]() extends Data[A]
  case class  Visited[A](data: A) extends Data[A]

  type UpdateFunc[A] = Int => Int => Table[A] => Table[A]
  type Table[A] = Array[Array[Data[A]]]

  def init[A](width: Int, height:Int): Table[A] =
    Array.fill(height)(Array.fill(width)(new Unvisited))

  def explore[A](row: Int, col: Int, table: Table[A], update: UpdateFunc[A]): Table[A] =
    table(row)(col) match {
      case Visited(_) => table
      case Unvisited() => update(row)(col)(table)
    }

}
