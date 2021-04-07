package com.evolutiongaming.bootcamp.effects

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class EffectsHomework1Spec extends AnyWordSpec with Matchers {

  import EffectsHomework1._

  "IO" should {
    "create new IO" in {
      val io = IO { "This is string" }
      io shouldBe a [IO[String]]
    }

    "unsafeRunSync should return value" in {
      val io = IO { "This is string" }
      io.unsafeRunSync() shouldBe "This is string"
    }
  }
}
