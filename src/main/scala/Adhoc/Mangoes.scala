package Adhoc

/*
 * Mangoes
 * https://www.hackerrank.com/challenges/mango/problem
 *
 * Biggest subset with a sum smaller than a constant
 */
object Mangoes {
  import math._
  import DS.BinTree._

  case class Friend(hunger:Long, happiness:Long) {
    def apply(x: Long) = happiness*(x-1) + hunger
  }

  def inviteFriends(friends: List[Friend], mangoes: Long): Int = {

    def smallestSubsetSum(k:Int):Long = {
      friends.map(_(k)).sorted.take(k).sum
    } //O(nlgn)

    def binarySearch(tree: Tree[Int], m:Int): Int = tree match {
      case Leaf => m
      case Node(l,r,e) =>
        val neededMangoes = smallestSubsetSum(e)
        if (neededMangoes == mangoes)
          e
        else if (neededMangoes < mangoes)
          binarySearch(r, max(m,e))
        else
          binarySearch(l, m)
    } //O(lgn)

    val tree = buildBST((1 to friends.length).toList)
    binarySearch(tree, 0)
  }
}
