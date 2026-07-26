package nmcb.yahtzee

object Score:
  
  import Dices.*

  enum ScoreType( val score: Dices => Int, val isValid: Dices => Boolean = _ => true):
    case Ones   extends ScoreType(_.count.get(Dice('1')).map(_ * 1).getOrElse(0))
    case Twos   extends ScoreType(_.count.get(Dice('2')).map(_ * 2).getOrElse(0))
    case Threes extends ScoreType(_.count.get(Dice('3')).map(_ * 3).getOrElse(0))
    case Fours  extends ScoreType(_.count.get(Dice('4')).map(_ * 4).getOrElse(0))
    case Fives  extends ScoreType(_.count.get(Dice('5')).map(_ * 5).getOrElse(0))
    case Sixes  extends ScoreType(_.count.get(Dice('6')).map(_ * 6).getOrElse(0))
    
    case ThreeOfAKind extends ScoreType(_.total, _.isThreeOfAKind)
    case Carre        extends ScoreType(_.total, _.isCarre)
    case FullHouse    extends ScoreType(_ => 25, _.isFullHouse)
    case SmallStreet  extends ScoreType(_ => 30, _.isSmallStreet)
    case LargeStreet  extends ScoreType(_ => 40, _.isLargeStreet)
    case Yahtzee      extends ScoreType(_ => 50, _.isYahtzee)
    case Change       extends ScoreType(_.total)
    
  import ScoreType.*
  
  val UpperScoreTypes: Set[ScoreType] =
    Set(Ones, Twos, Threes, Fours, Fives, Sixes)
    
  val LowerScoreTypes: Set[ScoreType] =
    Set(ThreeOfAKind, Carre, FullHouse, SmallStreet, LargeStreet, Yahtzee, Change)
    
import Score.*

case class Score(filled: Map[ScoreType, Dices]):
  
  val totalUpper: Int =
    filled
      .filter((scoreType, dices) => UpperScoreTypes.contains(scoreType))
      .map((scoreType, dices) => scoreType.score(dices))
      .sum
    
  val totalBonus: Int =
    if totalUpper >= 63 then 35 else 0
    
  val totalLower: Int =
    filled
      .filter((scoreType, dices) => LowerScoreTypes.contains(scoreType))
      .map((scoreType, dices) => scoreType.score(dices))
      .sum
    
  val totalGeneral: Int =
    totalUpper + totalBonus + totalLower
  
  def free(dices: Dices): Set[Score] =
    ScoreType
      .values
      .filter(scoreType => !filled.contains(scoreType) && scoreType.isValid(dices))
      .map(scoreType => copy(filled = filled + (scoreType -> dices)))
      .toSet
