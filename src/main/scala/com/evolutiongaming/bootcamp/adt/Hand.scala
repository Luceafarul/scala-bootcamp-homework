package com.evolutiongaming.bootcamp.adt

sealed trait Hand
final case class OmahaHand(first: Card, second: Card, third: Card, fourth: Card) extends Hand
final case class TexasHand(first: Card, second: Card) extends Hand
