package com.evolutiongaming.bootcamp.adt

sealed trait PokerCombination
object PokerCombination {
  case object HighCard extends PokerCombination
  case object OnePair extends PokerCombination
  case object TwoPair extends PokerCombination
  case object ThreeOfKind extends PokerCombination
  case object Straight extends PokerCombination
  case object Flush extends PokerCombination
  case object FullHouse extends PokerCombination
  case object FourOfKind extends PokerCombination
  case object StraightFlush extends PokerCombination
}

