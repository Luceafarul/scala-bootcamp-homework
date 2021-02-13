package com.evolutiongaming.bootcamp.adt

sealed trait PokerCombination
final case class HighCard(first: Card, second: Card, third: Card, fourth: Card, fives: Card) extends PokerCombination
final case class OnePair(first: Card, second: Card, third: Card, fourth: Card, fives: Card) extends PokerCombination
final case class TwoPair(first: Card, second: Card, third: Card, fourth: Card, fives: Card) extends PokerCombination
final case class ThreeOfKind(first: Card, second: Card, third: Card, fourth: Card, fives: Card) extends PokerCombination
final case class Straight(first: Card, second: Card, third: Card, fourth: Card, fives: Card) extends PokerCombination
final case class Flush(first: Card, second: Card, third: Card, fourth: Card, fives: Card) extends PokerCombination
final case class FullHouse(first: Card, second: Card, third: Card, fourth: Card, fives: Card) extends PokerCombination
final case class FourOfKind(first: Card, second: Card, third: Card, fourth: Card, fives: Card) extends PokerCombination
final case class StraightFlush(first: Card, second: Card, third: Card, fourth: Card, fives: Card) extends PokerCombination
