package nmcb.yahtzee

object Main:

  extension [A](i: Iterable[A])

    def countElements: Map[A, Int] =
      i.groupMapReduce(identity)(_ => 1)(_ + _)

  type Dices = String

  def cast(nr: Int): Vector[Dices] =
    (0 until 6)
      .map(side => ('1'.toInt + side).toChar)
      .foldLeft("")((result, dice) => result + List.fill(nr)(dice).mkString(""))
      .combinations(nr)
      .distinct
      .toVector
      .map(_.normalize)



  sealed trait Result(evaluate: Dices => Int)
  case object Ones         extends Result(_.count(_ == '1') * 1)
  case object Twos         extends Result(_.count(_ == '2') * 2)
  case object Threes       extends Result(_.count(_ == '3') * 3)
  case object Fours        extends Result(_.count(_ == '4') * 4)
  case object Fives        extends Result(_.count(_ == '5') * 5)
  case object Sixes        extends Result(_.count(_ == '6') * 6)
  case object ThreeOfAKind extends Result(_.foldLeft(0)(_ + _.asDigit))
  case object Carre        extends Result(_.foldLeft(0)(_ + _.asDigit))
  case object FullHouse    extends Result(_ => 25)
  case object SmallStreet  extends Result(_ => 30)
  case object LargeStreet  extends Result(_ => 40)
  case object Yahtzee      extends Result(_ => 50)
  case object Chance       extends Result(_.foldLeft(0)(_ + _.asDigit))

  type Outcome = (cast: Dices, chance: Double)

  extension (dices: Dices)

    def normalize: Dices =
      countSame
        .toVector
        .map(_.swap)
        .sorted.reverse
        .flatMap((count, dice) => Vector.fill(count)(dice))
        .mkString("")

    def countSame: Map[Char, Int]    = dices.countElements
    def has(others: String): Boolean = others.forall(dices.contains)

    def isThreeOfAKind: Boolean = dices.countSame.values.max == 3 && !isFullHouse
    def isCarre: Boolean        = dices.countSame.values.max == 4
    def isFullHouse: Boolean    = dices.countSame.size == 2 && !isCarre
    def isSmallStreet: Boolean  = (dices.has("1234") || dices.has("2345") || dices.has("3456")) && !isLargeStreet
    def isLargeStreet: Boolean  = dices.has("12345") | dices.has("23456")
    def isYahtzee: Boolean      = dices.countSame.size == 1

    def choices: Vector[Choice] =
      type Combination = (keep: Dices, cast: Dices)
      val combinations: Vector[Combination] =
        for
          nr   <- (0 to 5).toVector
          keep <- dices.combinations(nr).map(_.sorted).distinct
          cast <- cast(5 - nr)
        yield
          (keep, cast)

      combinations
        .groupMap(_.keep)(_.cast)
        .toVector
        .map: (keep, casts) =>
          val chance  = 1.0 / casts.size
          keep -> casts.map: cast =>
            (cast, chance)
        .sortBy(_._1.size)


  type Choice = (keep: Dices, casts: Vector[Outcome])


  def main(args: Array[String]): Unit =
    cast(5).filter(_.isCarre).sorted.foreach(println)
    "11122".choices.foreach(println)


