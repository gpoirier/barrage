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

  behavior of "ContractOffice"

  it should "ensure contract office is seeded with 2 contracts of each color" in {
    val state = GameState.initial(NonEmptyList.of(Italy, Germany, Netherlands))

    state.contractOffice.greenContracts.available.available.size shouldBe 2
    state.contractOffice.yellowContracts.available.available.size shouldBe 2
    state.contractOffice.redContracts.available.available.size shouldBe 2

    state.contractOffice.greenContracts.stack.stack.size shouldBe GreenContracts.all.stack.stack.size - 2
    state.contractOffice.yellowContracts.stack.stack.size shouldBe YellowContracts.all.stack.stack.size - 2
    state.contractOffice.redContracts.stack.stack.size shouldBe RedContracts.all.stack.stack.size - 2
  }

  it should "support replacing an available contract with a contract from the stack" in {
    val initial = ContractOffice(GreenContracts.all.seed, YellowContracts.all.seed, RedContracts.all.seed)
    val contract = initial.greenContracts.available.available.head
    val after = initial.replace(contract)

    initial.greenContracts.available.available.size shouldBe 2
    val stackSize = initial.greenContracts.stack.stack.size

    after.greenContracts.stack.stack.size shouldBe stackSize - 1
  }

  it should "support replacing 2 contracts of different types" in {
    val initial = ContractOffice(GreenContracts.all.seed, YellowContracts.all.seed, RedContracts.all.seed)
    val red = initial.redContracts.available.available.head
    val green = initial.greenContracts.available.available.head
    val after = initial.replace(Set(red, green))

    val redSize = initial.redContracts.stack.stack.size
    val greenSize = initial.greenContracts.stack.stack.size

    after.redContracts.stack.stack.size shouldBe redSize - 1
    after.greenContracts.stack.stack.size shouldBe greenSize - 1
  }
}
