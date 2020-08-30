package com.github.gpoirier.barrage

import cats.data.NonEmptyList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import actions._

class GameStateSpec extends AnyFlatSpec with Matchers {
  behavior of "resolve"

  it should "support taking a single excavator" in {
    val initial = GameState.initial(NonEmptyList.of(USA, Italy, Germany))
    val after = GameState.resolve(initial, Action.MachineShop(EngineerCount(1), Credit(2), Machinery(excavators = 1)))

    initial.currentPlayer shouldBe USA
    initial.nextPlayer shouldBe Italy
    initial.players(USA).resources shouldBe Resources(Credit(6), Machinery(excavators = 6, mixers = 4))

    after.currentPlayer shouldBe Italy
    after.players(USA).resources shouldBe Resources(Credit(4), Machinery(excavators = 7, mixers = 4))
  }
}
