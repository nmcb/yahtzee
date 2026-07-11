package nmcb.yahtzee

import nmcb.yahtzee.Dices.casts

object Rounds:

  type Rounds = Vector[Vector[Dices]]

  val all: Rounds =
    val result =
      for
        t1 <- 0 to 5
        t2 <- 0 to 5
        t3 <- 0 to 5
        if t1 + t2 + t3 == 5
      yield
        for
          c1 <- casts(t1)
          c2 <- casts(t2)
          c3 <- casts(t3)
        yield
          Vector(c1, c2, c3)
    result.flatten.toVector

  def main(args: Array[String]): Unit =
    val outcome = all.groupBy(v => Dices(v.mkString("")))
    outcome.toVector.sortBy(_._2.size).foreach((o, r) => println(s"$o - ${r.size}"))

