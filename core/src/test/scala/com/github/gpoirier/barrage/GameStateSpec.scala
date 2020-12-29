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

class GameStateSpec extends AnyFlatSpec with Matchers {
  behavior of "resolve"

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

      result <- StateT.inspect[Result, GameState, String] { gs =>
        GameState.resolveCommand(Command(Action.Workshop.Two, Nil)).run(gs)
          .fold(identity, _ => fail("The third spin should fail"))
      }
    } yield result

    op.run(before).fold(fail(_), _._2 shouldBe "No empty action spot left (Two)")
  }

  it should "support taking a single excavator" in {
    val initial: GameState = GameState.initial(NonEmptyList.of(USA))
    val getCredits: StateM[GameState, Credits] = StateT.inspect((lens.currentPlayerState composeLens lens.playerCredits).get)
    val getMachinery: StateM[GameState, Machinery] = StateT.inspect((lens.currentPlayerState composeLens lens.playerMachinery).get)

    val op = for {
      _ <- getCredits.map(_ shouldBe 6.credits)
      _ <- getMachinery.map(_ shouldBe Machinery(6.excavators, 4.mixers))
      _ <- GameState.resolveCommand(Command(Action.MachineShop.Excavator, Nil))
      _ <- getCredits.map(_ shouldBe 4.credits)
      _ <- getMachinery.map(_ shouldBe Machinery(7.excavators, 4.mixers))
    } yield ()

    op.run(initial)
  }
//
//  it should "support taking a single mixer with the expensive choice spot" in {
//    val initial = GameState.initial(NonEmptyList.of(USA, Italy, Germany, Netherlands))
//    val after = GameState.resolve(initial, Action.MachineShop.choice(MachineryType.Mixer).expensive)
//
//    initial.currentPlayer shouldBe USA
//    initial.nextPlayer shouldBe Italy
//    initial.players(USA).resources shouldBe Resources(Credit(6), Machinery(excavators = 6, mixers = 4))
//
//    after.currentPlayer shouldBe Italy
//    after.players(USA).resources shouldBe Resources(Credit(2), Machinery(excavators = 6, mixers = 5))
//  }
//
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
