package com.github.gpoirier.barrage

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.implicits._
import org.scalatest.Inside

class MachineShopSpec extends AnyFlatSpec with Matchers with Inside {
  behavior of "reserve"

  import MachineShop._
  import ActionColumn._

  it should "reserve free spots" in {
    val actions =
      for {
        _ <- MachineShop.excavator
        _ <- MachineShop.wild
        _ <- MachineShop.excavator
      } yield ()

    inside(actions.run(MachineShop.empty)) {
      case Right((rows, ())) =>
        rows.excavator shouldBe MachineShop.Row(List(Spot(Cheap, OccupiedSpot), Spot(Expensive, OccupiedSpot)))
        rows.wild shouldBe MachineShop.Row(List(Spot(Cheap, OccupiedSpot), Spot(Expensive, FreeSpot)))
        rows.both shouldBe MachineShop.Row(List(Spot(Cheap, FreeSpot), Spot(Expensive, FreeSpot)))
    }
  }

  it should "deny too many reservations" in {
    val actions =
      for {
        _ <- MachineShop.excavator
        _ <- MachineShop.wild
        _ <- MachineShop.excavator
        _ <- MachineShop.excavator
      } yield ()

    inside(actions.run(MachineShop.empty)) {
      case Left(message) =>
        message shouldBe "No empty action spot left"
    }
  }
}
