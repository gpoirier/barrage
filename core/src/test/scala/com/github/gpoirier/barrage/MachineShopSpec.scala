package com.github.gpoirier.barrage

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.implicits._
import org.scalatest.Inside

class MachineShopSpec extends AnyFlatSpec with Matchers with Inside {
  behavior of "reserve"

  import MachineShopSection._
  import ActionColumn._

  it should "reserve free spots" in {
    val actions =
      for {
        _ <- MachineShopSection.excavator
        _ <- MachineShopSection.wild
        _ <- MachineShopSection.excavator
      } yield ()

    inside(actions.run(MachineShopSection.empty)) {
      case Right((rows, ())) =>
        rows.excavator shouldBe MachineShopSection.Row(List(Spot(Cheap, OccupiedSpot), Spot(Expensive, OccupiedSpot)))
        rows.wild shouldBe MachineShopSection.Row(List(Spot(Cheap, OccupiedSpot), Spot(Expensive, FreeSpot)))
        rows.both shouldBe MachineShopSection.Row(List(Spot(Cheap, FreeSpot), Spot(Expensive, FreeSpot)))
    }
  }

  it should "deny too many reservations" in {
    val actions1 =
      for {
        _ <- MachineShopSection.excavator
        _ <- MachineShopSection.wild
        _ <- MachineShopSection.excavator
        _ <- MachineShopSection.excavator
      } yield ()

    val actions2 =
      for {
        _ <- MachineShopSection.excavator
        _ <- MachineShopSection.wild
        _ <- MachineShopSection.excavator
        _ <- MachineShopSection.both
        _ <- MachineShopSection.both
        _ <- MachineShopSection.both
      } yield ()

    inside(actions1.run(MachineShopSection.empty)) {
      case Left(message) =>
        message shouldBe "No empty action spot left (Excavator)"
    }
    inside(actions2.run(MachineShopSection.empty)) {
      case Left(message) =>
        message shouldBe "No empty action spot left (Both)"
    }
  }
}
