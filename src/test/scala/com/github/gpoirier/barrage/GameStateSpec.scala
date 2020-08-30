package com.github.gpoirier.barrage

import cats.data.NonEmptyList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import actions._

class GameStateSpec extends AnyFlatSpec with Matchers {
  behavior of "resolve"

  it should "support taking a single excavator" in {
    val initial = GameState.initial(NonEmptyList.of(USA, Italy, Germany))
    val after = GameState.resolve(initial, Action.MachineShop.excavator.cheap)

    initial.currentPlayer shouldBe USA
    initial.nextPlayer shouldBe Italy
    initial.players(USA).resources shouldBe Resources(Credit(6), Machinery(excavators = 6, mixers = 4))

    after.currentPlayer shouldBe Italy
    after.players(USA).resources shouldBe Resources(Credit(4), Machinery(excavators = 7, mixers = 4))
  }

  it should "support taking a single mixer with the expensive choice spot" in {
    val initial = GameState.initial(NonEmptyList.of(USA, Italy, Germany, Netherlands))
    val after = GameState.resolve(initial, Action.MachineShop.choice(MachineryType.Mixer).expensive)

    initial.currentPlayer shouldBe USA
    initial.nextPlayer shouldBe Italy
    initial.players(USA).resources shouldBe Resources(Credit(6), Machinery(excavators = 6, mixers = 4))

    after.currentPlayer shouldBe Italy
    after.players(USA).resources shouldBe Resources(Credit(2), Machinery(excavators = 6, mixers = 5))
  }

  behavior of "ExternalWorks"

  it should "ensure C1 takes 5 excavators for 4 points and 3 mixers" in {
    val initial = GameState.initial(NonEmptyList.of(Italy, Germany, Netherlands))
    val after = GameState.resolve(initial, Action.ExternalWorks(C1))

    initial.players(Italy).engineers shouldBe EngineerCount(12)
    initial.players(Italy).resources shouldBe Resources(Credit(6), Machinery(excavators = 6, mixers = 4))
    initial.players(Italy).points shouldBe VictoryPoints(10)

    after.players(Italy).engineers shouldBe EngineerCount(10)
    after.players(Italy).resources shouldBe Resources(Credit(6), Machinery(excavators = 1, mixers = 7))
    after.players(Italy).points shouldBe VictoryPoints(14)
  }
}
