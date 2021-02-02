package com.evolutiongaming.bootcamp.basics

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BasicSpec extends AnyWordSpec with Matchers {
  import Basic._

  "lcd" should {
    "be 6 for 2 and 3" in {
      lcd(2, 3) shouldBe 6
    }

    "be 216 for 12 and 18" in {
      lcd(12, 18) shouldBe 36
    }

    "be 0 for 0 and 0" in {
      lcd(0, 0) shouldBe 0
    }

    "be 0 for 0 and 7" in {
      lcd(0, 7) shouldBe 7
    }
  }

  "gcd" should {
    "be 4 for 8 and 12" in {
      gcd(8, 12) shouldBe 4
    }

    "be 6 for 48 and 18" in {
      gcd(48, 18) shouldBe 6
    }
  }
}
