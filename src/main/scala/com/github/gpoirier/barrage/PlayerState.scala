package com.github.gpoirier.barrage

sealed trait StructureType
object StructureType {
  sealed trait Dam
  case object Base extends StructureType with Dam
  case object Elevation extends StructureType with Dam
  case object Conduit extends StructureType
  case object Powerhouse extends StructureType
  case object SpecialBuilding extends StructureType
}

case class TechnologyTile(level: Int, kind: Option[StructureType])

case class Resources(money: Int, excavators: Int, mixers: Int)

case class WheelSlot(tile: Option[TechnologyTile], excavator: Int, mixer: Int)
case class Wheel(slots: List[WheelSlot])
case class PlayerState(resources: Resources, wheel: Wheel, points: Int, tiles: Set[TechnologyTile])
