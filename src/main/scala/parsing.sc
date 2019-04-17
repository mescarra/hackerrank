

object Solution {
  def main(args: Array[String]) {
    import reflect.ClassTag
    import math._
    import io._
    val t = StdIn.readLine.trim.toInt
    for (_ <- 0 until t) {
      val charList = StdIn.readLine.trim.toList
      val intArray  = StdIn.readLine.trim.split(" ").map(_.toInt)
      val longArray = StdIn.readLine.trim.split(" ").map(_.toLong)
    }
  }
}

//def main(args: Array[String]) {
//  import io._
//  val t = StdIn.readLine.trim.toInt
//  for (_ <- 0 until t) {
//    val tup = StdIn.readLine.trim.split(" ").map(_.toInt)
//    println(DiceRoll.getMax(tup(0)-1, tup(1)-1))
//  }
//}

//import all.BinTree._
//    import io.StdIn
//    val n = StdIn.readLine.trim.toInt
//
//    def entry(i:Int):(Int,Int) = {
//      val ch = StdIn.readLine.trim.split(" ").map(_.toInt)
//      (ch(0), ch(1))
//    }
//
//    val treeArr:Array[(Int, Int)] = Array.tabulate(n)(entry)
//
//    def constructTree(a: Array[(Int, Int)], i:Int): Tree[Int] = {
//      a(i-1) match {
//        case (-1, -1) => Node(Leaf,Leaf, i)
//        case (-1, ri) => Node(Leaf,constructTree(a,ri), i)
//        case (li, -1) => Node(constructTree(a,li), Leaf, i)
//        case (li, ri) => Node(constructTree(a,li), constructTree(a,ri), i)
//      }
//    }
//
//    var tree: Tree[Int] = constructTree(treeArr, 1)
//
//    val t = StdIn.readLine.trim.toInt
//    for (_ <- 0 until t) {
//      val k = StdIn.readLine.trim.toInt
//      tree = swapAtHeight(tree, k)
//      println(inOrder(tree).mkString(" "))
//    }
//}