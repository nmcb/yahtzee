package nmcb
package yahtzee

import scala.CanEqual.derived

opaque type Dice = Char

object Dice:

  given CanEqual[Dice, Dice] = derived

  def apply(char: Char): Dice = char


opaque type Dices = String

object Dices:

  given CanEqual[Dices, Dices] = derived

  /** Returns a normalized set of dices of given cast length */
  def apply(cast: String): Dices =
    assert(cast.nonEmpty && cast.length <= 5, s"cast length may be between 1 and 5 dices, was: ${cast.length}")

    cast
      .countSame
      .toVector
      .map(_.swap)
      .sorted.reverse
      .flatMap((count, dice) => Vector.fill(count)(dice))
      .mkString("")

  /** Returns a [[Vector]] of distinctly possible [[Dices]] casts after normalization for given [[nrOfDices]] */
  def cast(nrOfDices: Int): Vector[Dices] =
    assert(nrOfDices > 0 && nrOfDices <= 5, s"nr of dices cast may be between 1 and 5 dices, was: $nrOfDices")
    (0 until 6)
      .map(side => ('1'.toInt + side).toChar)
      .foldLeft("")((result, dice) => result + List.fill(nrOfDices)(dice).mkString(""))
      .combinations(nrOfDices)
      .distinct
      .toVector

  extension (dices: Dices)

    def countSame: Map[Dice, Int] =
      dices.groupMapReduce(identity)(_ => 1)(_ + _)
      
    def has(others: Dices): Boolean =
      val dicesCount  = dices.countSame
      val othersCount = others.countSame
      othersCount.forall((dice, count) => dicesCount.getOrElse(dice, 0) >= count)
    
    def isThreeOfAKind: Boolean = dices.countSame.values.max >= 3
    def isCarre: Boolean        = dices.countSame.values.max >= 4
    def isYahtzee: Boolean      = dices.countSame.size == 1
    def isFullHouse: Boolean    = dices.countSame.size == 2 && dices.countSame.values.max == 3 && dices.countSame.values.min == 2
    def isSmallStreet: Boolean  = dices.has("1234") || dices.has("2345") || dices.has("3456")
    def isLargeStreet: Boolean  = dices.has("12345") || dices.has("23456")
