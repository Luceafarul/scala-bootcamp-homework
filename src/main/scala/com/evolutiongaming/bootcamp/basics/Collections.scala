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
    ???
  }

  // optional hometask:
  //
  // https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/
  // https://leetcode.com/problems/split-a-string-in-balanced-strings
  // https://leetcode.com/problems/matrix-block-sum/
}
