package nmcb.yahtzee

import org.scalatest.funsuite.AnyFunSuite

class ScoreTests extends AnyFunSuite:

  import Score.ScoreType.*

  test("Score.next"):

    // NOK nrOfDices
    assertResult("assertion failed: nr of dices must be 5 dices, was: 0")(intercept[AssertionError](
      Score.start.next(Dices(""))
    ).getMessage)
    assertResult("assertion failed: nr of dices must be 5 dices, was: 1")(intercept[AssertionError](
      Score.start.next(Dices("1"))
    ).getMessage)
    assertResult("assertion failed: nr of dices must be 5 dices, was: 2")(intercept[AssertionError](
      Score.start.next(Dices("12"))
    ).getMessage)
    assertResult("assertion failed: nr of dices must be 5 dices, was: 3")(intercept[AssertionError](
      Score.start.next(Dices("123"))
    ).getMessage)
    assertResult("assertion failed: nr of dices must be 5 dices, was: 4")(intercept[AssertionError](
      Score.start.next(Dices("1234"))
    ).getMessage)

    // OK
    assertResult(
      Vector(
        Score(Map(Ones        -> Dices("54321"))),
        Score(Map(Twos        -> Dices("54321"))),
        Score(Map(Threes      -> Dices("54321"))),
        Score(Map(Fours       -> Dices("54321"))),
        Score(Map(Fives       -> Dices("54321"))),
        Score(Map(Sixes       -> Dices("54321"))),
        Score(Map(SmallStreet -> Dices("54321"))),
        Score(Map(LargeStreet -> Dices("54321"))),
        Score(Map(Change      -> Dices("54321")))
      )
    )(Score.start.next(Dices("12345")))

  test("Score.totalUpper"):
    assertResult(
      Vector(
        1,
        2,
        3,
        4,
        5,
        0,
        0,
        0,
        0
      )
    )(Score.start.next(Dices("12345")).map(_.totalUpper))

  test("Score.totalBonus"):
    assertResult(
      Vector(
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0
      )
    )(Score.start.next(Dices("12345")).map(_.totalBonus))

    assertResult(35)(
      Score(
        Map(
          Fours -> Dices("44444"),
          Fives -> Dices("55555"),
          Sixes -> Dices("66666")
        )
      ).totalBonus
    )

  test("Score.totalLower"):
    assertResult(
      Vector(
        0,
        0,
        0,
        0,
        0,
        0,
        30,
        40,
        15
      )
    )(Score.start.next(Dices("12345")).map(_.totalLower))

  test("Score.totalGeneral"):
    assertResult(372)(
      Score(
        Map(
          Ones   -> Dices("11111"),         //  5
          Twos   -> Dices("22222"),         // 10
          Threes -> Dices("33333"),         // 15
          Fours  -> Dices("44444"),         // 20
          Fives  -> Dices("55555"),         // 25
          Sixes  -> Dices("66666"),         // 30
                                            // 35
          ThreeOfAKind -> Dices("55666"),   // 28
          Carre        -> Dices("56666"),   // 29
          FullHouse    -> Dices("55666"),   // 25
          SmallStreet  -> Dices("23456"),   // 30
          LargeStreet  -> Dices("23456"),   // 40
          Yahtzee      -> Dices("66666"),   // 50
          Change       -> Dices("66666")    // 30
        )
      ).totalGeneral
    )