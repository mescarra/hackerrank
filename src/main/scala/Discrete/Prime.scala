package Discrete

object Prime {
  import math._

  def isPrime(n:Int):Boolean = {
    def isPrimeTR(k:Int): Boolean = {
      if (n==k) true
      else if (n%k==0) false
      else isPrimeTR(k+1)
    }
    if (n < 2) false
    else isPrimeTR(2)
  }

  def primeFactors(n: Long): Map[Long, Long] = {
    def primeFactTR(k:Long, i: Long, s:Map[Long, Long]): Map[Long, Long] = {
      if (k == 1) s
      else if (k%i == 0)
        try {
          primeFactTR(k/i, i, s + (i -> (s(i)+1)))
        } catch {
          case _:Throwable => primeFactTR(k/i, i, s + (i -> 1))
        }
      else
        primeFactTR(k, i+1, s)
    }
    primeFactTR(n, 2, Map.empty)
  }

  def maps(ns: Array[Long], ms: Array[Long]): (Map[Long,Long],Map[Long,Long]) = {
    val nsMap = ns.map(primeFactors).flatten.groupBy(_._1).mapValues(_.map(_._2).sum)
    val msMap = ms.map(primeFactors).flatten.groupBy(_._1).mapValues(_.map(_._2).sum)

    (nsMap, msMap)
  }

  def commonPrimeFactors(ns: Array[Long], ms: Array[Long]): Array[(Long, Long)] = {
    val (nsMap, msMap) = maps(ns,ms)
    var arr:Array[(Long,Long)] = Array.empty
    var gcd:Long = 1
    for (i <- nsMap.keys) {
      if (msMap.keys.exists(_==i)){
        val p = min(nsMap(i), msMap(i))
        arr +:= (i,p)
      }
    }
    arr
  }
}
