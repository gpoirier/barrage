package com.github.gpoirier.barrage
import com.github.gpoirier.barrage.costs._

import scala.collection.immutable.Queue

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

case class Credit(value: Int) extends AnyVal {
  def -(other: Credit): Credit = Credit(value - other.value)
  def +(other: Credit): Credit = Credit(value + other.value)
}
case class Machinery(excavators: Int, mixers: Int) {
  def -(other: Machinery): Machinery = Machinery(excavators - other.excavators, mixers - other.mixers)
  def +(other: Machinery): Machinery = Machinery(excavators + other.excavators, mixers + other.mixers)
}
case class Resources(credit: Credit, machinery: Machinery) {
  def -(other: Resources): Resources = Resources(credit - other.credit, machinery - other.machinery)
  def +(other: Resources): Resources = Resources(credit + other.credit, machinery + other.machinery)
}

case class WheelSlot(tile: Option[TechnologyTile], machinery: Machinery)
object WheelSlot {
  val empty = WheelSlot(None, Machinery(0, 0))
}
case class Wheel(slots: Queue[WheelSlot] = Queue.fill(5)(WheelSlot.empty)) {
  def push(slot: WheelSlot): (WheelSlot, Wheel) = {
    val (ws, queue) = slots.enqueue(slot).dequeue
    (ws, Wheel(queue))
  }
}

case class PlayerState(engineers: Int, resources: Resources, wheel: Wheel, points: Int, tiles: Set[TechnologyTile]) {

  def resolveCost(cost: Cost): PlayerState = cost match {
    case Costs(costs) => costs match {
      case Nil => this
      case head :: tail => resolveCost(head).resolveCost(tail.asInstanceOf[Cost])
    }
    case EngineerCost(count) => copy(engineers = engineers - count)
    case CreditCost(credit: Credit) => copy(resources = Resources(this.resources.credit - credit, this.resources.machinery))
    case MachineryCost(machinery: Machinery) => copy(resources = Resources(this.resources.credit, this.resources.machinery - machinery))
  }

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

}