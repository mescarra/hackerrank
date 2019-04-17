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

  /*
   * Password Cracker FP
   * https://www.hackerrank.com/challenges/password-cracker-fp/problem
   *
   * Write a string as a composition of a set of strings
   */
  def crack(passwords: Set[String], testing: List[Char], current: String): List[String] = (testing, current) match {
    case (Nil, c) if c.length == 0 => Nil
    case (Nil, c) => throw new Exception("Unmatched")
    case (c::cs, cur) =>
      val newCur = cur + c
      if (passwords.exists(_==newCur)){
        try {
          newCur::crack(passwords,cs, "")
        } catch {
          case _:Throwable =>
            crack(passwords,cs, newCur)
        }
      } else {
        crack(passwords, cs, newCur)
      }
  }

  def protectedCrack(passwords: Set[String], test: List[Char]): List[String] = {
    try {
      crack(passwords, test, "")
    } catch {
      case _:Throwable => Nil
    }
  }

  def minimize(passwords: List[String]): Set[String] = {
    def minimizeTR(pass: List[String], passFiltered: List[String]): List[String] = pass match {
      case Nil => passFiltered
      case p :: ps => protectedCrack((ps ++ passFiltered).toSet, p.toList) match {
        case Nil => minimizeTR(ps, p :: passFiltered)
        case _ => minimizeTR(ps, passFiltered)
      }
    }

    minimizeTR(passwords.sortBy(_.length), Nil).toSet
  }

  def hack(passwords:List[String], test: List[Char]): List[String] = {
    protectedCrack(minimize(passwords), test)
  }

}
