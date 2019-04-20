package Challenges

object FrequencyQuery {

  import DS.Dictionary.addOrUpdate

  case class FrequencyQuery() {
    var elemToFreq = collection.mutable.Map.empty[Int, Int]
    var freqToElem = collection.mutable.Map.empty[Int, Int]

    def insert(x:Int): Unit = {
      addOrUpdate(elemToFreq, x, (x,1), (n:Int) => n+1)
      val f = elemToFreq(x)

      if (f > 1)
        freqToElem(f-1) -= 1

      addOrUpdate(freqToElem, f, (f,1), (n:Int) => n+1)
    }

    def delete(x:Int): Unit = {
      elemToFreq.get(x) match {
        case Some(f) if f > 0 =>
          freqToElem(elemToFreq(x)) -= 1
          elemToFreq(x) -= 1

          if (elemToFreq(x) > 0)
            freqToElem(elemToFreq(x)) += 1
        case _ => ()
      }
    }

    def frequency(f:Int): Boolean = freqToElem.get(f) match {
      case Some(ff) => ff > 0
      case _        => false
    }
  }

  def freqQuery(queries: Array[Array[Int]]): Array[Int] = {
    val fq = new FrequencyQuery()
    queries.flatMap { q =>
      (q(0), q(1)) match {
        case (1, x) => fq.insert(x); None
        case (2, x) => fq.delete(x); None
        case (3, f) => if (fq.frequency(f)) Some(1)  else Some(0)
      }
    }
  }

  def main(args: Array[String]) {
    import io._

    val q = StdIn.readLine.trim.toInt

    val queries = Array.ofDim[Int](q, 2)

    for (i <- 0 until q) {
      queries(i) = StdIn.readLine.replaceAll("\\s+$", "").split(" ").map(_.trim.toInt)
    }

    val ans = freqQuery(queries)

    println(ans.mkString("\n"))
  }


}
