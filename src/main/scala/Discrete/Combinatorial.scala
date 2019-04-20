package Discrete

object Combinatorial {
  import math._

  def dpChoose(n: Int, k: Int, max: Long = Long.MaxValue): Long = {
    val dpCount: Array[Array[Long]] = Array.fill(10001)(Array.fill(10001)(0))
    var kP = k
    if (k > n / 2)
      kP = n - k

    for (i <- 0 to n)
      for (j <- 0 to min(kP, i)) {
        if (dpCount(i)(j) <= 0)
          if (j == 0 || j == i)
            dpCount(i)(j) = 1
          else
            dpCount(i)(j) = (dpCount(i - 1)(j) + dpCount(i - 1)(j - 1)) % max
      }

    dpCount(n)(kP)
  }

  def factorial(n:Long): Long = {
    def factTR(k:Long, acc:Long):Long = if (k<=1) acc else factTR(k-1, k*acc)

    factTR(n, 1)
  }

  def choose(n:Int, k:Int): Long = {
    var ki = min(2,k)
    var fact: Long = 1L
    for (ni <- n until (n-k) by -1){
      fact *= ni.toLong
      if (ki <= k && (fact % ki == 0)){
        fact /= ki.toLong
        ki += 1
      }
    }

    for (j <- ki to k)
      fact /= j.toLong

    fact
  }

  def permutations[A](elements: List[A]): List[List[A]] = {

    def interleave(perm: List[A], x:A): List[List[A]] = {
      def interleaveTR(i:Int, acc:List[List[A]]):List[List[A]] = i match {
        case 0 => (x::perm) :: acc
        case _ => interleaveTR(i-1, (perm.take(i) ++ List(x) ++ perm.drop(i)) :: acc)
      }
      interleaveTR(perm.length, Nil)
    }

    def permsTR(elems:List[A]): List[List[A]] = elems match {
      case Nil => List(Nil)
      case x::xs => permsTR(xs).flatMap(p => interleave(p, x))
    }

    permsTR(elements)
  }

  def powerSet[A](t: Set[A]): Set[Set[A]] = {
    def pwr(t: Set[A], ps: Set[Set[A]]): Set[Set[A]] =
      if (t.isEmpty) ps
      else pwr(t.tail, ps ++ (ps map (_ + t.head)))

    pwr(t, Set(Set.empty[A])) //Powerset of ∅ is {∅}
  }


}
