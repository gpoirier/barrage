package com.github.gpoirier.barrage

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.Queue
import cats.implicits._
import cats.data._
import actions._
import commands._
import resources._
import literals._
import org.scalatest.Assertion

class GameStateSpec extends AnyFlatSpec with Matchers {
  behavior of "resolve"

  implicit class StateOps[S](self: StateM[S, Assertion]) {
    def runTest(initial: S): Assertion = self.runA(initial).fold(fail(_), identity)
  }

  it should "support taking spin actions" in {
    val empty = WheelSlot.empty
    val slot = WheelSlot(Some(TechnologyTile(1, Some(StructureType.Elevation))), 2.mixers)
    val initial = GameState.initial(NonEmptyList.of(USA, Italy, Germany))
    val addCredits = (lens.currentPlayerState composeLens lens.playerCredits).modify(_ ++ 5.credits)
    val pushWheel = lens.playerWheel.modify(_.push(slot)._2)
    val before = (pushWheel compose addCredits)(initial)

    val getWheel: StateM[GameState, Wheel] = StateT.inspect(lens.playerWheel.get)
    val getEng: StateM[GameState, EngineerCount] = StateT.inspect((lens.currentPlayerState composeLens lens.engineers).get)
    val getCredits: StateM[GameState, Credits] = StateT.inspect((lens.currentPlayerState composeLens lens.playerCredits).get)

    val op = for {
      _ <- getWheel.map(_.slots shouldBe Queue(empty, empty, empty, empty, slot))
      _ <- getEng.map(_ shouldBe 12.eng)
      _ <- getCredits.map(_ shouldBe 11.credits)

      _ <- GameState.resolveCommand(Command(Action.Workshop.Two, Nil))
      _ <- getWheel.map(_.slots shouldBe Queue(empty, empty, slot, empty, empty))
      _ <- getEng.map(_ shouldBe 10.eng)

      _ <- GameState.resolveCommand(Command(Action.Workshop.Two, Nil))
      _ <- getWheel.map(_.slots shouldBe Queue(slot, empty, empty, empty, empty))
      _ <- getEng.map(_ shouldBe 7.eng)

      result <- GameState.resolveCommand(Command(Action.Workshop.Two, Nil)).attempt
    } yield result shouldBe Left("No empty action spot left (Two)")

    op.runTest(before)
  }

  it should "ensure spin costs are correct" in {
    val initial = GameState.initial(NonEmptyList.of(USA, Italy, Germany))
    val addCredits = (lens.currentPlayerState composeLens lens.playerCredits).modify(_ ++ 14.credits)
    val addEng = (lens.currentPlayerState composeLens lens.engineers).modify(_ ++ EngineerCount(1))
    val before = (addCredits compose addEng)(initial)

    val getEng: StateM[GameState, EngineerCount] = StateT.inspect((lens.currentPlayerState composeLens lens.engineers).get)
    val getCredits: StateM[GameState, Credits] = StateT.inspect((lens.currentPlayerState composeLens lens.playerCredits).get)

    val op = for {
      _ <- getEng.map(_ shouldBe 13.eng)
      _ <- getCredits.map(_ shouldBe 20.credits)

      _ <- GameState.resolveCommand(Command(Action.Workshop.One, Nil))
      _ <- getEng.map(_ shouldBe 12.eng)
      _ <- getCredits.map(_ shouldBe 20.credits)

      _ <- GameState.resolveCommand(Command(Action.Workshop.One, Nil))
      _ <- getEng.map(_ shouldBe 10.eng)
      _ <- getCredits.map(_ shouldBe 20.credits)

      _ <- GameState.resolveCommand(Command(Action.Workshop.Two, Nil))
      _ <- getEng.map(_ shouldBe 8.eng)
      _ <- getCredits.map(_ shouldBe 18.credits)

      _ <- GameState.resolveCommand(Command(Action.Workshop.Two, Nil))
      _ <- getEng.map(_ shouldBe 5.eng)
      _ <- getCredits.map(_ shouldBe 13.credits)

      _ <- GameState.resolveCommand(Command(Action.Workshop.Three, Nil))
      _ <- getEng.map(_ shouldBe 3.eng)
      _ <- getCredits.map(_ shouldBe 8.credits)

      _ <- GameState.resolveCommand(Command(Action.Workshop.Three, Nil))
      _ <- getEng.map(_ shouldBe 0.eng)
      _ <- getCredits.map(_ shouldBe 0.credits)

    } yield succeed

    op.runTest(before)
  }

  it should "support taking a single excavator" in {
    val initial: GameState = GameState.initial(NonEmptyList.of(USA))
    val getCredits: StateM[GameState, Credits] = StateT.inspect((lens.currentPlayerState composeLens lens.playerCredits).get)
    val getMachinery: StateM[GameState, Machinery] = StateT.inspect((lens.currentPlayerState composeLens lens.playerMachinery).get)
    val getEng: StateM[GameState, EngineerCount] = StateT.inspect((lens.currentPlayerState composeLens lens.engineers).get)

    val op = for {
      // checking before state
      _ <- getCredits.map(_ shouldBe 6.credits)
      _ <- getMachinery.map(_ shouldBe Machinery(6.excavators, 4.mixers))
      _ <- getEng.map(_ shouldBe 12.eng)

      // taking an excavator
      _ <- GameState.resolveCommand(Command(Action.MachineShop.Excavator, Nil))
      _ <- getCredits.map(_ shouldBe 4.credits)
      _ <- getMachinery.map(_ shouldBe Machinery(7.excavators, 4.mixers))
      _ <- getEng.map(_ shouldBe 11.eng)

    } yield succeed

    op.runTest(initial)
  }

  it should "ensure MachineShop.Wild work and occupy the same spot" in {
    val initial: GameState = GameState.initial(NonEmptyList.of(USA))
    val getCredits: StateM[GameState, Credits] = StateT.inspect((lens.currentPlayerState composeLens lens.playerCredits).get)
    val getMachinery: StateM[GameState, Machinery] = StateT.inspect((lens.currentPlayerState composeLens lens.playerMachinery).get)
    val getEng: StateM[GameState, EngineerCount] = StateT.inspect((lens.currentPlayerState composeLens lens.engineers).get)
    val addCredits = (lens.currentPlayerState composeLens lens.playerCredits).modify(_ ++ 10.credits)
    val before = addCredits(initial)

    val op = for {
      // checking before state
      _ <- getCredits.map(_ shouldBe 16.credits)
      _ <- getMachinery.map(_ shouldBe Machinery(6.excavators, 4.mixers))
      _ <- getEng.map(_ shouldBe 12.eng)

      // taking an excavator
      _ <- GameState.resolveCommand(Command(Action.MachineShop.Wild, List(RewardSelector.WildMachinery(1.excavator))))
      _ <- getCredits.map(_ shouldBe 12.credits)
      _ <- getMachinery.map(_ shouldBe Machinery(7.excavators, 4.mixers))
      _ <- getEng.map(_ shouldBe 11.eng)

      // taking a second excavator
      _ <- GameState.resolveCommand(Command(Action.MachineShop.Wild, List(RewardSelector.WildMachinery(1.mixer))))
      _ <- getCredits.map(_ shouldBe 8.credits)
      _ <- getMachinery.map(_ shouldBe Machinery(7.excavators, 5.mixers))
      _ <- getEng.map(_ shouldBe 9.eng)

      // trying to use the excavator spot again
      result <- GameState.resolveCommand(Command(Action.MachineShop.Wild, Nil)).attempt
    } yield result shouldBe Left("No empty action spot left (Wild)")

    op.runTest(before)
  }

//  behavior of "ExternalWorks"
//
//  it should "ensure C1 takes 5 excavators for 6 points and 3 mixers" in {
//    val initial = GameState.initial(NonEmptyList.of(Italy, Germany, Netherlands))
//    val after = GameState.resolve(initial, Action.ExternalWorks(C1))
//
//    initial.players(Italy).engineers shouldBe EngineerCount(12)
//    initial.players(Italy).resources shouldBe Resources(Credit(6), Machinery(excavators = 6, mixers = 4))
//    initial.players(Italy).points shouldBe VictoryPoints(10)
//
//    after.players(Italy).engineers shouldBe EngineerCount(10)
//    after.players(Italy).resources shouldBe Resources(Credit(6), Machinery(excavators = 1, mixers = 7))
//    after.players(Italy).points shouldBe VictoryPoints(16)
//  }
}
