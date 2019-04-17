package DP

/*
 * Dice Path
 * https://www.hackerrank.com/challenges/dice-path/problem
 *
 * Largest sum of a rolling dice across any path in a board
 */
object DicePath {

  trait Rotation
  case object RotateRight extends Rotation
  case object RotateDown extends Rotation
  type Path = List[Rotation]

  case class Dice(top:Int, left:Int, front:Int) {
    def back = 7-front
    def right = 7-left
    def bottom = 7-top
  }
  case class DiceTrace(dice: Dice, max: Int)

  trait Data
  case object Unvisited extends Data
  case class Visited(dices: Set[DiceTrace]) extends Data

  val tableWidth = 60
  val tableHeight = 60
  var diceMax: Array[Array[Data]] = Array.fill(tableHeight)(Array.fill(tableWidth)(Unvisited))

  val initDice = Dice(1,3,2)
  diceMax(0)(0) = Visited(Set(DiceTrace(initDice, initDice.top)))


  def rotateDice(rotation: Rotation)(dice: Dice): Dice = rotation match {
    case RotateRight => Dice(dice.left,dice.bottom,dice.front)
    case RotateDown  => Dice(dice.back,dice.left,dice.top)
  }

  def roll(rotation:Rotation)(data: Data): Data = data match {
    case Unvisited => Unvisited
    case Visited(ts)   =>
      val newTraces = ts.map {t =>
        val newD = rotateDice(rotation)(t.dice)
        DiceTrace(newD, t.max + newD.top)
      }
      Visited(newTraces)
  }

  def rollRight: Data => Data = roll(RotateRight)
  def rollDown: Data => Data = roll(RotateDown)

  def initDiceMax = {
    for (i <- 1 until tableHeight)
      diceMax(i)(0) = roll(RotateDown)(diceMax(i-1)(0))

    for (i <- 1 until tableWidth)
      diceMax(0)(i) = roll(RotateRight)(diceMax(0)(i-1))
  }
  initDiceMax

  def explore(row:Int, col:Int): Data = diceMax(row)(col) match {
    case Visited(ts) => Visited(ts)
    case Unvisited =>
      val left = explore(row,col-1)
      val up = explore(row-1,col)
      (rollRight(left), rollDown(up)) match {
        case (Visited(ts1), Visited(ts2)) => {
          val m = (ts1 union ts2) groupBy (_.dice) mapValues (_.map(_.max).max)
          val newTs:Set[DiceTrace] = m.toSet.map{ (e:(Dice,Int)) => DiceTrace(e._1,e._2) }
          diceMax(row)(col) = Visited(newTs)
          diceMax(row)(col)
        }
        case _ => throw new Exception("Invalid state")
      }
  }

  def getMax(row:Int, col:Int) = explore(row,col) match {
    case Visited(ts) => ts.map(t => t.max).max
    case _ => throw new Exception("Invalid state")
  }

  def validDices: Set[Dice] = {
    var s = Set.empty[Dice]
    val iOptions = 1 to 6
    for (i <- iOptions) {
      val jOptions = iOptions.filterNot(x=> x==i || x==(7-i))
      for (j <- jOptions) {
        val kOptions = jOptions.filterNot(x=> x==j || x==(7-j))
        for (k <- kOptions)
          s += Dice(i,j,k)
      }
    }
    s
  }

  //  def explore(row:Int, col: Int): Data = {
//    if (diceMax(row)(col)._1 > 0) {
//      diceMax(row)(col)
//    } else
//    {
//
//    }
//  }

//  def findTop(path: Path): Int = {
//
//    def findTopTR(p:Path, d:Dice): Int = p match {
//      case Nil => d.top
//      case x::xs => findTopTR(xs, rotateDice(x, d))
//    }
//
//    findTopTR(path, initDice)
//  }
//
//  def constructPaths(row:Int, col:Int): List[Path] = {
//    if (col == 1)
//      List(("D"*(row-1)).toList)
//    else if (row == 1)
//      List(("R"*(col-1)).toList)
//    else
//      constructPaths(row-1,col).map('D'::_) ++ constructPaths(row,col-1).map('R'::_)
//  }
//
//  def diceExplore(row:Int, col:Int): Int = {
//
//    def diceExploreTR(row: Int, col: Int): (Int, List[Path]) = {
//      if (diceMax(row)(col)._1 > 0)
//        diceMax(row)(col)
//      else
//      {
//        var maxUp: (Int, List[Path]) = (Int.MinValue, List(Nil))
//        var maxLeft: (Int, List[Path]) = (Int.MinValue, List(Nil))
//
//        if (row > 0)
//          maxUp = diceExploreTR(row - 1, col)
//
//        if (col > 0)
//          maxLeft = diceExploreTR(row, col - 1)
//
//
//        val pathsUp:List[Path] = maxUp._2.map(_ ++ List('D'))
//        val pathsLeft:List[Path] = maxLeft._2.map(_ ++ List('R'))
//
//        val maxsUp = pathsUp.map(x => (maxUp._1 + findTop(x),x))
//        val maxsLeft = pathsLeft.map(x=> (maxLeft._1 + findTop(x),x))
//        val all = maxsUp ++ maxsLeft
//        val allMax = all.map(_._1).max
//        val allPath = all.filter(_._1 == allMax).map(_._2)
//        diceMax(row)(col) = (allMax, allPath)
//        diceMax(row)(col)
//      }
//    }
//
//    diceExploreTR(row-1,col-1)._1
//  }

}
