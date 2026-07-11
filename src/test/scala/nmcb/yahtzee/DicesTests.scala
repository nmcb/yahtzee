package nmcb.yahtzee

import org.scalatest.funsuite.AnyFunSuite

class DicesTests extends AnyFunSuite:

  test("Dices.apply"):
    // NOK length
    assertResult("assertion failed: cast length may be between 1 and 5 dices, was: 0")(intercept[AssertionError](Dices("")).getMessage)
    assertResult("assertion failed: cast length may be between 1 and 5 dices, was: 6")(intercept[AssertionError](Dices("123456")).getMessage)

    // OK length
    assertResult("1")(Dices("1").toString)
    assertResult("21")(Dices("12").toString)
    assertResult("321")(Dices("123").toString)
    assertResult("4321")(Dices("1234").toString)
    assertResult("54321")(Dices("12345").toString)

    // Normalized
    assertResult("22233")(Dices("32322").toString)
    assertResult("33322")(Dices("23233").toString)

  test("Dices.cast"):
    // NOK length
    assertResult("assertion failed: nr of dices cast may be between 1 and 5 dices, was: 0")(intercept[AssertionError](Dices.cast(0)).getMessage)
    assertResult("assertion failed: nr of dices cast may be between 1 and 5 dices, was: 6")(intercept[AssertionError](Dices.cast(6)).getMessage)

    // OK length
    assertResult(6)(Dices.cast(1).length)
    assertResult(21)(Dices.cast(2).length)
    assertResult(56)(Dices.cast(3).length)
    assertResult(126)(Dices.cast(4).length)
    assertResult(252)(Dices.cast(5).length)

  test("Dices.countSame"):
    assertResult(Map(
      Dice('1') -> 1,
      Dice('2') -> 1,
      Dice('3') -> 1,
      Dice('4') -> 1,
      Dice('5') -> 1,
    ))(Dices("12345").countSame)

    assertResult(Map(
      Dice('2') -> 3,
      Dice('3') -> 2,
    ))(Dices("23232").countSame)

  test("Dices.has"):
    assertResult(false)(Dices("23456").has(Dices("1")))
    assertResult(false)(Dices("12345").has(Dices("6")))
    assertResult(false)(Dices("123").has(Dices("456")))
    assertResult(false)(Dices("456").has(Dices("123")))

    assertResult(true)(Dices("23456").has(Dices("2")))
    assertResult(true)(Dices("23456").has(Dices("3")))
    assertResult(true)(Dices("23456").has(Dices("4")))
    assertResult(true)(Dices("23456").has(Dices("5")))
    assertResult(true)(Dices("23456").has(Dices("6")))
    assertResult(true)(Dices("12345").has(Dices("1")))
    assertResult(true)(Dices("12345").has(Dices("2")))
    assertResult(true)(Dices("12345").has(Dices("3")))
    assertResult(true)(Dices("12345").has(Dices("4")))
    assertResult(true)(Dices("12345").has(Dices("5")))

    assertResult(false)(Dices("23232").has(Dices("1")))
    assertResult(false)(Dices("23232").has(Dices("4")))
    assertResult(false)(Dices("23232").has(Dices("5")))
    assertResult(false)(Dices("23232").has(Dices("6")))

    assertResult(false)(Dices("23232").has(Dices("333")))
    assertResult(false)(Dices("23232").has(Dices("2222")))

    assertResult(true)(Dices("23232").has(Dices("2")))
    assertResult(true)(Dices("23232").has(Dices("22")))
    assertResult(true)(Dices("23232").has(Dices("222")))
    assertResult(true)(Dices("23232").has(Dices("3")))
    assertResult(true)(Dices("23232").has(Dices("33")))

  test("Dices.isThreeOfAKind"):
    assertResult(false)(Dices("12345").isThreeOfAKind)
    assertResult(false)(Dices("23456").isThreeOfAKind)

    assertResult(false)(Dices("11234").isThreeOfAKind)
    assertResult(false)(Dices("12234").isThreeOfAKind)
    assertResult(false)(Dices("12334").isThreeOfAKind)
    assertResult(false)(Dices("12344").isThreeOfAKind)
    assertResult(false)(Dices("23455").isThreeOfAKind)
    assertResult(false)(Dices("34566").isThreeOfAKind)

    assertResult(true)(Dices("11123").isThreeOfAKind)
    assertResult(true)(Dices("12223").isThreeOfAKind)
    assertResult(true)(Dices("12333").isThreeOfAKind)
    assertResult(true)(Dices("23444").isThreeOfAKind)
    assertResult(true)(Dices("34555").isThreeOfAKind)
    assertResult(true)(Dices("45666").isThreeOfAKind)

    // Full House Is Also Three Of A Kind
    assertResult(true)(Dices("11122").isThreeOfAKind)
    assertResult(true)(Dices("11222").isThreeOfAKind)

    // Carre is also Three Of A Kind
    assertResult(true)(Dices("11112").isThreeOfAKind)
    assertResult(true)(Dices("12222").isThreeOfAKind)
    assertResult(true)(Dices("12333").isThreeOfAKind)
    assertResult(true)(Dices("23444").isThreeOfAKind)
    assertResult(true)(Dices("34555").isThreeOfAKind)
    assertResult(true)(Dices("56666").isThreeOfAKind)

    // Yahtzee is also Three Of A Kind
    assertResult(true)(Dices("11111").isThreeOfAKind)
    assertResult(true)(Dices("22222").isThreeOfAKind)
    assertResult(true)(Dices("33333").isThreeOfAKind)
    assertResult(true)(Dices("44444").isThreeOfAKind)
    assertResult(true)(Dices("55555").isThreeOfAKind)
    assertResult(true)(Dices("66666").isThreeOfAKind)

  test("Dices.isCarre"):
    assertResult(false)(Dices("12345").isCarre)
    assertResult(false)(Dices("23456").isCarre)

    assertResult(false)(Dices("11223").isCarre)
    assertResult(false)(Dices("11123").isCarre)

    assertResult(true)(Dices("11112").isCarre)
    assertResult(true)(Dices("12222").isCarre)

    // Yahtzee is also Carre
    assertResult(true)(Dices("11111").isCarre)
    assertResult(true)(Dices("22222").isCarre)
    assertResult(true)(Dices("33333").isCarre)
    assertResult(true)(Dices("44444").isCarre)
    assertResult(true)(Dices("55555").isCarre)
    assertResult(true)(Dices("66666").isCarre)

  test("Dices.isYahtzee"):
    assertResult(false)(Dices("12345").isYahtzee)
    assertResult(false)(Dices("23456").isYahtzee)

    assertResult(false)(Dices("11223").isYahtzee)
    assertResult(false)(Dices("11123").isYahtzee)

    assertResult(false)(Dices("11112").isYahtzee)
    assertResult(false)(Dices("12222").isYahtzee)

    assertResult(true)(Dices("11111").isYahtzee)
    assertResult(true)(Dices("22222").isYahtzee)
    assertResult(true)(Dices("33333").isYahtzee)
    assertResult(true)(Dices("44444").isYahtzee)
    assertResult(true)(Dices("55555").isYahtzee)
    assertResult(true)(Dices("66666").isYahtzee)

  test("Dices.isFullHouse"):
    assertResult(false)(Dices("12345").isFullHouse)
    assertResult(false)(Dices("23456").isFullHouse)

    assertResult(false)(Dices("11223").isFullHouse)
    assertResult(false)(Dices("11123").isFullHouse)

    assertResult(true)(Dices("11122").isFullHouse)
    assertResult(true)(Dices("11222").isFullHouse)

  test("Dices.isSmallStreet"):
    assertResult(false)(Dices("11123").isSmallStreet)
    assertResult(false)(Dices("12356").isSmallStreet)

    assertResult(true)(Dices("12346").isSmallStreet)
    assertResult(true)(Dices("23456").isSmallStreet)
    assertResult(true)(Dices("13456").isSmallStreet)

    // Large Street is also Small Street
    assertResult(true)(Dices("12345").isSmallStreet)
    assertResult(true)(Dices("23456").isSmallStreet)
