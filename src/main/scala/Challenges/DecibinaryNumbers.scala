package Challenges

/*
* Decibinary
* https://www.hackerrank.com/challenges/candies/problem
*
* Return the ith decibinary number for multiple subsequent queries
*/
object DecibinaryNumbers {
  import math._

  val MAX_NUMBER = 10000

  var m = collection.mutable.Map.empty[(Int,Int), Long]
  var maxi = collection.mutable.Map.empty[Int, Int]
  val positions = Array.fill(MAX_NUMBER)(0L)

  for (i <- 0 to 9) m((i,1)) = 1L
  maxi(0) = 1
  maxi(1) = 1
  positions(0) = 1
  positions(1) = 2

  def lg(x:Int): Int = floor(log(x.toDouble) / log(2D)).toInt

  def pw(x:Int): Int = pow(2D, x).toInt

  def decibinaryCount(x:Int): Unit = {
    def decibinaryCountIter(x: Int): Long = {
      val l = lg(x)
      var totalSum = 0L
      for (i <- 1 to l) {
        var iSum = 0L
        val p = pw(i)
        for (j <- (x/p) to 1 by -1) {
          val have = j * p
          val rest = x - have

          if (rest == 0)
            iSum += 1L
          else
            iSum += m.getOrElse((rest,min(i,maxi(rest))), 0L)

        }
        val s = (iSum + m.getOrElse((x,i), 0L))
        m += ((x,i+1) -> s)
        maxi += (x -> (i+1))
        positions(x) = positions(x-1) + s
        totalSum += iSum
      }

      if (x <= 9) totalSum += 1
      totalSum
    }

    maxi.get(x) match {
      case Some(j) => m(x,j)
      case None =>
        for (i <- (maxi.keys.max+1) until x) {
          decibinaryCountIter(i)
        }

        positions(x) = positions(x-1) + decibinaryCountIter(x)
    }
  }

  def decibinary2Decimal(a:Array[Int]): Long = {
    var s = 0L
    for (i <- 0 until a.length)
      s += (pw(i) * a(i))
    s
  }

  def nextDecibinary(x:Int, digits:Int, db: Array[Int]): Array[Int] = {
    var next = db
    var i = 0
    var a = db(0)
    var b = if (digits > 1) db(1) else 9

    while ((i < digits-1) && (a < pw(i+1) || b == 9)) {
      i += 1
      a = db(i)
      b = db(i+1)
    }

    if (i >= digits-1) throw new Exception("no next decibinary")

    next(i+1) += 1

    calibrate(x, i+1, next)
  }

  def calibrate(x:Int, digits:Int, db:Array[Int]): Array[Int] = {
    val calibrating = db
    for (i <- (digits-1) to 0 by -1) {
      val min = if (i == digits-1) 1 else 0
      while(calibrating(i) >= min && decibinary2Decimal(calibrating) > x)
      {
        calibrating(i) = calibrating(i) - 1
      }
      if (i > 0)
        calibrating(i) = calibrating(i) + 1
    }
    calibrating
  }

  def buildDecibinary(x:Int, digits:Int, seq:Long): Long = {
    val first = calibrate(x, digits, Array.fill(digits)(9))

    (first zip (0 until digits)).foldLeft(0L)((s, x) => s + x._1 * math.pow(10, x._2).toLong)
  }

  def decibinaryGenerate(x:Int, seq: Long): Long = {
    decibinaryCount(x)
    var lastCount = 0L
    for (i <- 1 to maxi(x)){
      val countByNumberOfDigits = m.get(x,i).getOrElse(0L)
      if (countByNumberOfDigits >= seq)
        return buildDecibinary(x, i, seq-lastCount)

      lastCount = countByNumberOfDigits
    }
    throw new Exception("sequence number out of range")
  }

}
