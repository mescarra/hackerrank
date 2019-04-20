package Challenges

/*
 * Super Digit
 * https://www.hackerrank.com/challenges/super-digit/problem
 *
 * Digit sum of a large number
 */
object Digit {

  def superDigit(n:List[Char], k:Long): Long = {
    def superDigitTR(xs:List[Char], acc:Long, i:Long): Long = xs match {
      case Nil =>
        val nuN = acc*i
        if (nuN < 10L) nuN
        else superDigitTR(nuN.toString.toList, 0L, 1L)

      case y :: ys => superDigitTR(ys, y.toString.toInt + acc, i)
    }

    superDigitTR(n, 0L, k)
  }

}
