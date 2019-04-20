package Challenges

/*
 * Password Cracker FP
 * https://www.hackerrank.com/challenges/password-cracker-fp/problem
 *
 * Write a string as a composition of a set of strings
 */
object PasswordCracker {

  def crack(passwords: Set[String], testing: List[Char], current: String): List[String] = (testing, current) match {
    case (Nil, c) if c.length == 0 => Nil
    case (Nil, c) => throw new Exception("Unmatched")
    case (c::cs, cur) =>
      val newCur = cur + c
      if (passwords.contains(newCur)){
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
