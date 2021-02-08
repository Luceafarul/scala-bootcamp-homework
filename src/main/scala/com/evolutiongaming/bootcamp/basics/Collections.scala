package com.evolutiongaming.bootcamp.basics

object Collections {
  // https://leetcode.com/problems/running-sum-of-1d-array/
  def runningSum(nums: Array[Int]): Array[Int] = {
    if (nums.isEmpty) Array.empty
    else nums.tail.scanLeft(nums.head)((acc, elem) => acc + elem)
  }

  // https://leetcode.com/problems/shuffle-the-array
  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    if (nums.isEmpty) Array.empty
    else {
      val (first, second) = nums.splitAt(n)
      first.zip(second).flatMap { case (f, s) => List(f, s) }
    }
  }

  // https://leetcode.com/problems/richest-customer-wealth
  def maximumWealth(accounts: Array[Array[Int]]): Int = {
    accounts.map(_.sum).max
  }

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    candies.scanLeft(false) { (_, elem) =>
      if (candies.forall(elem + extraCandies >= _)) true else false
    }.drop(1)
  }

  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points
  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    val sorted = points
      .map(_.head)
      .sortWith(_ < _)

    sorted
      .tail
      .scanLeft((0, sorted.head)) { (acc, elem) =>
        val (_, y) = acc
        (elem - y) -> elem
      }
      .map { case (x, _) => x }
      .max
  }

  // optional hometask:
  //
  // https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/
  def maxDepth(s: String): Int = {
    ???
  }

  // https://leetcode.com/problems/split-a-string-in-balanced-strings
  def balancedStringSplit(s: String): Int = {
    ???
  }

  // https://leetcode.com/problems/matrix-block-sum/
  def matrixBlockSum(mat: Array[Array[Int]], K: Int): Array[Array[Int]] = {
    ???
  }
}
