package com.evolutiongaming.bootcamp.adt

sealed trait Board
final case class OmahaBoard(first: Card, second: Card, third: Card, fourth: Card, fives: Card) extends Board
final case class TexasBoard(first: Card, second: Card, third: Card, fourth: Card, fives: Card) extends Board
