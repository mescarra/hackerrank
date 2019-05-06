package DP

object LongestCommonSubsequence {
  case class LCS[A](seq1: Array[A], seq2:Array[A]) {
    trait Direction
    case object NoWhere extends Direction
    case object UpLeft extends Direction
    case object Up extends Direction
    case object Left extends Direction

    case class Data(max: Int, direction: Direction)

    var dp:Array[Array[Data]] = Array.fill(seq1.length+1)(Array.fill(seq2.length+1)(Data(0, NoWhere)))

    for (i <- 1 to seq1.length)
      for (j <- 1 to seq2.length)
        if (seq1(i-1) == seq2(j-1)) {
          val Data(m, _) = dp(i-1)(j-1)
          dp(i)(j) = Data(m+1, UpLeft)
        } else if (dp(i-1)(j).max >= dp(i)(j-1).max) {
          val Data(m, _) = dp(i-1)(j)
          dp(i)(j) = Data(m, Up)
        } else {
          val Data(m, _) = dp(i)(j-1)
          dp(i)(j) = Data(m, Left)
        }
    
    def length: Int = dp(seq1.length)(seq2.length).max
    
    def sequence: List[A] = {
      def seqR(i:Int, j:Int): List[A] = {
        if (i == 0 || j == 0) Nil
        else if(dp(i)(j).direction == UpLeft) seqR(i-1,j-1) ++ List(seq1(i-1))
        else if(dp(i)(j).direction == Up) seqR(i-1,j)
        else seqR(i,j-1)
      }
      
      seqR(seq1.length, seq2.length)
    }
  }
}
