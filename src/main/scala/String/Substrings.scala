package String

object Substrings {

  /*
   * Substring Searching
   * https://www.hackerrank.com/challenges/kmp-fp/problem
   *
   * Substring search in O(n)*
   */
  def kmp(str:List[Char], word: Array[Char]): Boolean = {
    val wlen = word.length

    def kmpTR(str:List[Char], word: Array[Char], i:Int, t:Int): Boolean = str match {
      case Nil => (i == wlen)
      case x :: xs => i match {
        case n if (n == wlen)    => true
        case _ if (x == word(i) && x == word(t))  => kmpTR(xs, word, i+1, t+1)
        case _ if (x == word(i))  => kmpTR(xs, word, i+1, 0)
        case _ if (x != word(i) && x == word(t))  => kmpTR(xs, word, t+1, 0)
        case _ if (x == word(0)) => kmpTR(xs,word,1,0)
        case _ => kmpTR(xs,word,0,0)
      }
    }

    kmpTR(str, word, 0,0)
  }

}
