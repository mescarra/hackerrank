package DS

object BinTree {

  trait Tree[+A] {
    def isEmpty: Boolean
  }

  case class Node[+A](l: Tree[A], r: Tree[A], e: A) extends Tree[A] {
    def isEmpty = false
  }

  case object Leaf extends Tree[Nothing] {
    def isEmpty = true
  }

  def buildBST[A](ordered: List[A]): Tree[A] = ordered match {
    case Nil => Leaf
    case _ => {
      val m = ordered.length / 2
      val s1 = ordered.take(m)
      val s2 = ordered.drop(m + 1)
      Node(buildBST(s1), buildBST(s2), ordered(m))
    }
  }

  def inOrder[A](t: Tree[A]): List[A] = t match {
    case Leaf => Nil
    case Node(l, r, e) => inOrder(l) ++ List(e) ++ inOrder(r)
  }

  def preOrder[A](t: Tree[A]): List[A] = t match {
    case Leaf => Nil
    case Node(l, r, e) => List(e) ++ preOrder(l) ++ preOrder(r)
  }

  def swap[A](t: Tree[A]): Tree[A] = t match {
    case Leaf => Leaf
    case Node(l, r, e) => Node(r, l, e)
  }

  def swapAtHeight[A](t: Tree[A], k: Int): Tree[A] = {

    def swapAtHeightTR[A](tr: Tree[A], i: Int): Tree[A] = tr match {
      case Leaf => Leaf
      case Node(l, r, e) if (i == 1) => Node(swapAtHeightTR(r, k), swapAtHeightTR(l, k), e)
      case Node(l, r, e) => Node(swapAtHeightTR(l, i - 1), swapAtHeightTR(r, i - 1), e)
    }

    swapAtHeightTR(t, k)
  }

}
