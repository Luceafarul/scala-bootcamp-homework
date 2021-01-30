package com.evolutiongaming.bootcamp.basics

import scala.annotation.tailrec

// Homework:
// Implement functions that calculate https://en.wikipedia.org/wiki/Lowest_common_denominator
// and https://en.wikipedia.org/wiki/Greatest_common_divisor for integers.
object Basic {
  def lcd(a: Int, b: Int): Int = (a * b) / gcd(a, b)

  @tailrec
  def gcd(a: Int, b: Int): Int = if (a == 0) b else gcd(b % a, a);
}
