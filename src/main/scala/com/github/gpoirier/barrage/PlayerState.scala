package com.github.gpoirier.barrage

import cats.implicits._
import cats.data._
import actions.Cost
import resources._
import literals._

import Ordering.Implicits._

sealed trait StructureType
object StructureType {
  sealed trait Dam
  case object Base extends StructureType with Dam
  case object Elevation extends StructureType with Dam
  case object Conduit extends StructureType
  case object Powerhouse extends StructureType
  case object SpecialBuilding extends StructureType

  val all: Set[StructureType] = Set(Base, Elevation, Conduit, Powerhouse, SpecialBuilding)
}

case class TechnologyTile(level: Int, kind: Option[StructureType])

object TechnologyTile {
  def allForLevel(level: Int): Set[TechnologyTile] = StructureType.all.map(tpe => TechnologyTile(level, Some(tpe)))
}

sealed trait MachineryType
object MachineryType {
  case object Excavator extends MachineryType
  case object Mixer extends MachineryType
}

case class PlayerState(
  engineers: EngineerCount,
  resources: Resources,
  wheel: Wheel,
  points: VictoryPoints,
  energyProduction: Energy,
  tiles: Set[TechnologyTile]
) {
  def spin: PlayerState = {
    val (slot, newWheel) = wheel.push(WheelSlot.empty)
    copy(wheel = newWheel, resources = Resources(resources.credit, resources.machinery ++ slot.machinery), tiles = tiles ++ slot.tile)
  }
  def spin(count: Int): PlayerState = {
    (0 until count).foldLeft(this) { (acc, _) =>
      acc.spin
    }
  }
}

object PlayerState {
  val initial: PlayerState =
    PlayerState(
      EngineerCount(12),
      Resources(6.credits, 6.excavators & 4.mixers),
      Wheel.empty,
      VictoryPoints(10),
      Energy(0),
      TechnologyTile.allForLevel(0)
    )

  def spin(count: Int): PlayerState => PlayerState = _.spin(count)

  def payCost: Cost => StateM[PlayerState, Unit] = {
    case Cost(eng, resources) =>
      StateT.modifyF[Result, PlayerState] { ps =>
        ps.pure[Result]
          .ensure("Not enough engineers")(_.engineers < eng)
          .ensure("Not enough credit")(_.resources.credit < resources.credit)
          .ensure("Not enough excavator")(_.resources.machinery.excavators < resources.machinery.excavators)
          .ensure("Not enough mixer")(_.resources.machinery.mixers < resources.machinery.mixers)
          .map {
            lens.engineers.modify(_ -- eng) compose lens.resources.modify(_ -- resources)
          }
      }
  }

}