package DS

object GenTree {

  case class GenTree[A](var value: A, var children: Array[GenTree[A]])  {
    def changeValue(x:A) =
      value = x

    def print = println(value)

    def insertChild(x:A, i:Int = 0) =
      children = children.take(i) ++ Array(GenTree(x, Array.empty[GenTree[A]])) ++ children.drop(i)

    def deleteChild(i:Int) = {
      children = children.take(i) ++ children.drop(i+1)
    }
  }

  object TreeManager {
    var tree: GenTree[Int] = GenTree(0, Array.empty)
    var curPath: List[Int] = Nil

    def goTo(path: List[Int]): GenTree[Int] = {
      def goToR(p: List[Int], t:GenTree[Int]): GenTree[Int] = p match {
        case Nil => t
        case x::xs => goToR(xs, t.children(x))
      }

      goToR(path, tree)
    }

    def change(x:Int) = goTo(curPath).changeValue(x)

    def print = goTo(curPath).print

    def delete = {
      val t = goTo(curPath.init)
      t.deleteChild(curPath.last)
      curPath = curPath.init
    }

    def insertLeft(x:Int) = {
      val t = goTo(curPath.init)
      t.insertChild(x, curPath.last)
      curPath = curPath.init ++ List(curPath.last+1)
    }

    def insertRight(x:Int) = {
      val t = goTo(curPath.init)
      t.insertChild(x, curPath.last + 1)
    }

    def insertChild(x:Int) = {
      val t = goTo(curPath)
      t.insertChild(x)
    }

    def visitLeft = {
      curPath = curPath.init ++ List(curPath.last-1)
    }

    def visitRight = {
      curPath = curPath.init ++ List(curPath.last+1)
    }

    def visitParent = {
      if (curPath.length > 0)
        curPath = curPath.init
    }

    def visitChild(n:Int) = {
      curPath = curPath ++ List(n-1)
    }

  }

}
