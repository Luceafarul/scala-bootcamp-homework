package com.evolutiongaming.bootcamp.adt


sealed trait Rank
object Rank {
  case object Two extends Rank
  case object Three extends Rank
  case object Four extends Rank
  case object Five extends Rank
  case object Six extends Rank
  case object Seven extends Rank
  case object Eight extends Rank
  case object Nine extends Rank
  case object Ten extends Rank
  case object Jack extends Rank
  case object Queen extends Rank
  case object King extends Rank
  case object Ace extends Rank
}

sealed trait Suite
object Suite {
  case object Clubs extends Suite
  case object Hearts extends Suite
  case object Spades extends Suite
  case object Diamonds extends Suite
}

final case class Card(rank: Rank, suite: Suite)
