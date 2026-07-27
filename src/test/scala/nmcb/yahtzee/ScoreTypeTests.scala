package nmcb.yahtzee

import nmcb.yahtzee.Score.ScoreType
import org.scalatest.funsuite.AnyFunSuite

class ScoreTypeTests extends AnyFunSuite:

  test("ScoreType.isValid"):

    // Upper ScoreTypes are always valid
    assertResult(true)(ScoreType.Ones.isValid(Dices("12345")))
    assertResult(true)(ScoreType.Ones.isValid(Dices("23456")))
    assertResult(true)(ScoreType.Twos.isValid(Dices("12345")))
    assertResult(true)(ScoreType.Twos.isValid(Dices("13456")))
    assertResult(true)(ScoreType.Threes.isValid(Dices("12345")))
    assertResult(true)(ScoreType.Threes.isValid(Dices("12456")))
    assertResult(true)(ScoreType.Fours.isValid(Dices("12345")))
    assertResult(true)(ScoreType.Fours.isValid(Dices("12356")))
    assertResult(true)(ScoreType.Fives.isValid(Dices("12345")))
    assertResult(true)(ScoreType.Fives.isValid(Dices("12346")))
    assertResult(true)(ScoreType.Sixes.isValid(Dices("23456")))
    assertResult(true)(ScoreType.Sixes.isValid(Dices("12345")))

    // Lower ScoreTypes
    assertResult(true)(ScoreType.FullHouse.isValid(Dices("11222")))
    assertResult(false)(ScoreType.FullHouse.isValid(Dices("11123")))
    assertResult(true)(ScoreType.Carre.isValid(Dices("11112")))
    assertResult(false)(ScoreType.Carre.isValid(Dices("11123")))
    assertResult(true)(ScoreType.FullHouse.isValid(Dices("11122")))
    assertResult(false)(ScoreType.FullHouse.isValid(Dices("11123")))
    assertResult(true)(ScoreType.SmallStreet.isValid(Dices("12346")))
    assertResult(false)(ScoreType.SmallStreet.isValid(Dices("12356")))
    assertResult(true)(ScoreType.LargeStreet.isValid(Dices("12345")))
    assertResult(false)(ScoreType.LargeStreet.isValid(Dices("12356")))
    assertResult(true)(ScoreType.Yahtzee.isValid(Dices("11111")))
    assertResult(false)(ScoreType.Yahtzee.isValid(Dices("11112")))
    assertResult(true)(ScoreType.Change.isValid(Dices("12345")))
    assertResult(true)(ScoreType.Change.isValid(Dices("23456")))

  test("ScoreType.score"):

    // Upper ScoreTypes count correct faces
    assertResult(2)(ScoreType.Ones.score(Dices("11222")))
    assertResult(6)(ScoreType.Twos.score(Dices("11222")))
    assertResult(6)(ScoreType.Threes.score(Dices("11233")))
    assertResult(8)(ScoreType.Fours.score(Dices("22344")))
    assertResult(15)(ScoreType.Fives.score(Dices("15556")))
    assertResult(12)(ScoreType.Sixes.score(Dices("12366")))

    // Lower ScoreTypes
    assertResult(7)(ScoreType.ThreeOfAKind.score(Dices("11122")))
    assertResult(8)(ScoreType.Carre.score(Dices("11222")))
    assertResult(25)(ScoreType.FullHouse.score(Dices("11122")))
    assertResult(30)(ScoreType.SmallStreet.score(Dices("12346")))
    assertResult(40)(ScoreType.LargeStreet.score(Dices("12345")))
    assertResult(50)(ScoreType.Yahtzee.score(Dices("11111")))
    assertResult(17)(ScoreType.Change.score(Dices("12356")))
