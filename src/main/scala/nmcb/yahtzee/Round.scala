package nmcb.yahtzee

object Round:

  import Dices.*

  private type Keeps = Vector[Dices]
  
  private val keepsCache: Map[Dices, Vector[Keeps]] =
    val generated =
      for
        nrOfKeeps1 <- 0 to MaxNrOfDices
        nrOfKeeps2 <- 0 to MaxNrOfDices
        nrOfKeeps3 <- 0 to MaxNrOfDices
        if nrOfKeeps1 + nrOfKeeps2 + nrOfKeeps3 == MaxNrOfDices
      yield
        for
          keeps1 <- casts(nrOfKeeps1)
          keeps2 <- casts(nrOfKeeps2)
          keeps3 <- casts(nrOfKeeps3)
        yield
          Vector(keeps1, keeps2, keeps3)
    generated.flatten.groupMap(_.result)(identity).map((result, fromKeeps) => result -> fromKeeps.toVector)
    
  extension (keeps: Keeps)

    def result: Dices =
      keeps.reduce(_ + _)

  def main(args: Array[String]): Unit =
    keepsCache.foreach(println)


