package nmcb.yahtzee

import scala.math.Ordered.orderingToOrdered

object Yahtzee:

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


  enum Value(rank: Dices) derives CanEqual:
    case OneOfAKind(rank: Dices)   extends Value(rank)
    case Pair(rank: Dices)         extends Value(rank)
    case TwoPair(rank: Dices)      extends Value(rank)
    case ThreeOfAKind(rank: Dices) extends Value(rank)
    case FullHouse(rank: Dices)    extends Value(rank)
    case FourOfAKind(rank: Dices)  extends Value(rank)
    case Street(rank: Dices)       extends Value(rank)
    case FiveOfAKind(rank: Dices)  extends Value(rank)

  object Value:

    given Ordering[Value] with
      def compare(a: Value, b: Value): Int =
        (a, b) match
          case (FiveOfAKind(l), FiveOfAKind(r))   => l.compare(r)
          case (FiveOfAKind(_), _)                => 1
          case (_, FiveOfAKind(_))                => -1
          case (Street(l), Street(r))             => l.compare(r)
          case (Street(_), _)                     => 1
          case (_, Street(_))                     => -1
          case (FourOfAKind(l), FourOfAKind(r))   => l.compare(r)
          case (FourOfAKind(_), _)                => 1
          case (_, FourOfAKind(_))                => -1
          case (FullHouse(l), FullHouse(r))       => l.compare(r)
          case (FullHouse(_), _)                  => 1
          case (_, FullHouse(_))                  => -1
          case (ThreeOfAKind(l), ThreeOfAKind(r)) => l.compare(r)
          case (ThreeOfAKind(_), _)               => 1
          case (_, ThreeOfAKind(_))               => -1
          case (TwoPair(l), TwoPair(r))           => l.compare(r)
          case (TwoPair(_), _)                    => 1
          case (_, TwoPair(_))                    => -1
          case (Pair(l), Pair(r))                 => l.compare(r)
          case (Pair(_), _)                       => 1
          case (_, Pair(_))                       => -1
          case (OneOfAKind(l), OneOfAKind(r))     => l.compare(r)

  type Outcome = (cast: Dices, chance: Double, value: Value, winning: Boolean)

  extension (dices: Dices)

    def rank: Dices =
      countSame
        .toVector
        .map(_.swap)
        .sorted.reverse
        .flatMap((count, dice) => Vector.fill(count)(dice))
        .mkString("")

    def countSame: Map[Char, Int] = dices.countElements

    def isOneOfAKind: Boolean   = dices.countSame.size == 5 && !isStreet
    def isPair: Boolean         = dices.countSame.size == 4
    def isTwoPair: Boolean      = dices.countSame.size == 3 && !isThreeOfAKind
    def isThreeOfAKind: Boolean = dices.countSame.values.max == 3 && !isFullHouse
    def isFullHouse: Boolean    = dices.countSame.size == 2 && !isFourOfAKind
    def isFourOfAKind: Boolean  = dices.countSame.values.max == 4
    def isStreet: Boolean       = dices.countSame.size == 5 && !(dices.exists(_ == '1') && dices.exists(_ == '6'))
    def isFiveOfAKind: Boolean  = dices.countSame.size == 1

    def value: Value =
      assert(dices.size == 5, s"cast $dices does not contain 5 dice")
      import Value.*
      if      dices.isOneOfAKind   then OneOfAKind(dices.rank)
      else if dices.isPair         then Pair(dices.rank)
      else if dices.isTwoPair      then TwoPair(dices.rank)
      else if dices.isThreeOfAKind then ThreeOfAKind(dices.rank)
      else if dices.isFullHouse    then FullHouse(dices.rank)
      else if dices.isFourOfAKind  then FourOfAKind(dices.rank)
      else if dices.isStreet       then Street(dices.rank)
      else if dices.isFiveOfAKind  then FiveOfAKind(dices.rank)
      else sys.error(s"unrecognized cast: $dices")

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
            val value   = (keep + cast).value
            (cast, chance, value, value > dices.value)
        .sortBy(_._1.size)


  type Choice = (keep: Dices, casts: Vector[Outcome])


  def main(args: Array[String]): Unit =
    cast(5).map(_.value).sorted.foreach(println)
    "11122".choices.foreach(println)


