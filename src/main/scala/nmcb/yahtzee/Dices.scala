package nmcb
package yahtzee

import scala.CanEqual.derived

opaque type Dice = Char

object Dice:

  given CanEqual[Dice, Dice] = derived

  def apply(char: Char): Dice =
    assert(faces.contains(char), s"dice faces may be between '1' and '6', was: $char")
    char

  val faces: Vector[Dice] =
    (0 until 6)
      .map(side => ('1'.toInt + side).toChar)
      .toVector


opaque type Dices = String

object Dices:
  
  val MaxNrOfDices = 5

  given CanEqual[Dices, Dices] = derived
  given Ordering[Dices] = Ordering.by(identity)

  /** Returns a normalized set of dices from given string of [[Dice]] faces */
  def apply(string: String): Dices =
    assert(string.forall(Dice.faces.contains), s"dice faces may be between '1' and '6', was: $string")
    assert(string.length <= MaxNrOfDices, s"length may be maximally $MaxNrOfDices dice faces, was: ${string.length}")

    string
      .countSame
      .toVector
      .map(_.swap)
      .sorted.reverse
      .flatMap((count, dice) => Vector.fill(count)(dice))
      .mkString("")

  /** Returns a [[Vector]] of distinctly possible [[Dices]] cast after normalization for given [[nrOfDices]] */
  def casts(nrOfDices: Int): Vector[Dices] =
    assert(nrOfDices >= 0 && nrOfDices <= MaxNrOfDices, s"nr of dices cast may be between 0 and $MaxNrOfDices dices, was: $nrOfDices")

    Dice
      .faces
      .foldLeft("")((result, dice) => result + List.fill(nrOfDices)(dice).mkString(""))
      .combinations(nrOfDices)
      .map(Dices.apply)
      .distinct
      .toVector

  extension (dices: Dices)

    def +(others: Dices): Dices =
      Dices(dices + others)

    def nrOfDices: Int =
      dices.length

    /** Returns a [[scala.Vector]] of distinctly possible [[nrOfDices]] kept from these [[dices]] */
    def keeps(nrOfDices: Int): Vector[Dices] =
      assert(nrOfDices >= 0 && nrOfDices <= MaxNrOfDices, s"nr of dices kept may be between 0 and $MaxNrOfDices dices, was: $nrOfDices")
      dices.combinations(nrOfDices).toVector

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
