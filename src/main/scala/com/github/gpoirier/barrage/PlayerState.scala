package com.github.gpoirier.barrage

import resources._
import literals._

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
  energyProduction: RoundProduction,
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
      RoundProduction(0),
      TechnologyTile.allForLevel(0)
    )

  def spin(count: Int): PlayerState => PlayerState = _.spin(count)
}