package com.github.gpoirier.barrage
import scala.collection.immutable.Queue

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
case class Credit(value: Int) extends AnyVal {
  def -(other: Credit): Credit = Credit(value - other.value)
  def +(other: Credit): Credit = Credit(value + other.value)
}
case class Machinery(excavators: Int = 0, mixers: Int = 0) {
  def -(other: Machinery): Machinery = Machinery(excavators - other.excavators, mixers - other.mixers)
  def +(other: Machinery): Machinery = Machinery(excavators + other.excavators, mixers + other.mixers)
}
case class Resources(credit: Credit, machinery: Machinery) {
  def -(other: Resources): Resources = Resources(credit - other.credit, machinery - other.machinery)
  def +(other: Resources): Resources = Resources(credit + other.credit, machinery + other.machinery)
}

case class EngineerCount(value: Int) extends AnyVal {
  def -(other: EngineerCount): EngineerCount = EngineerCount(value - other.value)
  def +(other: EngineerCount): EngineerCount = EngineerCount(value + other.value)
}

case class VictoryPoints(count: Int) extends AnyVal {
  def -(other: VictoryPoints): VictoryPoints = VictoryPoints(count - other.count)
  def +(other: VictoryPoints): VictoryPoints = VictoryPoints(count + other.count)
}
case class RoundProduction(energyCount: Int)

case class WheelSlot(tile: Option[TechnologyTile], machinery: Machinery)
object WheelSlot {
  val empty = WheelSlot(None, Machinery(0, 0))
}
case class Wheel private (slots: Queue[WheelSlot]) {
  def push(slot: WheelSlot): (WheelSlot, Wheel) = {
    val (ws, queue) = slots.enqueue(slot).dequeue
    (ws, Wheel(queue))
  }
}

object Wheel {
  def empty: Wheel = Wheel(Queue.fill(5)(WheelSlot.empty))
}

case class PlayerState(
  engineers: EngineerCount,
  resources: Resources,
  wheel: Wheel,
  points: VictoryPoints,
  energyProduction: RoundProduction,
  tiles: Set[TechnologyTile],
  contracts: Set[Contract]
) {
  def spin: PlayerState = {
    val (slot, newWheel) = wheel.push(WheelSlot.empty)
    copy(wheel = newWheel, resources = Resources(resources.credit, resources.machinery + slot.machinery), tiles = tiles ++ slot.tile)
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
      Resources(Credit(6), Machinery(6, 4)),
      Wheel.empty,
      VictoryPoints(10),
      RoundProduction(0),
      TechnologyTile.allForLevel(0),
      Set() //Contracts
    )

  def spin(count: Int): PlayerState => PlayerState = _.spin(count)
}