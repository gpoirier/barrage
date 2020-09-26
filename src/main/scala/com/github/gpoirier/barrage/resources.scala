package com.github.gpoirier.barrage

import io.estatico.newtype.Coercible
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._
import simulacrum._

import scala.collection.immutable.Queue

object resources {

  abstract class PlusMinusOps[A: Numeric] {
    type T

    implicit class PlusMinusOps(value: T) {
      def ++(other: T)(implicit ev1: Coercible[T, A], ev2: Coercible[A, T]): T =
        Numeric[A].plus(value.coerce, other.coerce[A]).coerce[T]
      def --(other: T)(implicit ev1: Coercible[T, A], ev2: Coercible[A, T]): T =
        Numeric[A].minus(value.coerce, other.coerce[A]).coerce[T]
    }
  }

  abstract class ToMachineryOps[A: Numeric] extends PlusMinusOps[A] {
    type T

    implicit class MachineryOps(value: T) {
      def &[B: ToMachinery](other: B)(implicit ev: ToMachinery[T]): Machinery =
        ToMachinery[T].to(value) ++ ToMachinery[B].to(other)

      def toMachinery(implicit ev: ToMachinery[T]): Machinery = ToMachinery[T].to(value)
    }
  }

  object literals {
    implicit class IntLiteralOps(value: Int) {
      def credit: Credits = Credits(value)
      def credits: Credits = Credits(value)

      def excavator: Excavators = Excavators(value)
      def excavators: Excavators = Excavators(value)

      def mixer: Mixers = Mixers(value)
      def mixers: Mixers = Mixers(value)

      def vp: VictoryPoints = VictoryPoints(value)

      def eng: EngineerCount = EngineerCount(value)
    }
  }
  import literals._

  @newtype case class Credits(count: Int)
  object Credits extends ToMachineryOps[Int] {
    type T = Credits
    implicit def creditsOrdering: Ordering[Credits] = deriving
  }

  @newtype case class Excavators(count: Int)
  object Excavators extends ToMachineryOps[Int] {
    type T = Excavators
    implicit def excavatorsOrdering: Ordering[Excavators] = deriving
    implicit def fromExcavator: ToMachinery[Excavators] = ToMachinery.instance(e => Machinery(e))
  }

  @newtype case class Mixers(count: Int)
  object Mixers extends ToMachineryOps[Int] {
    type T = Mixers
    implicit def mixerOrdering: Ordering[Mixers] = deriving
    implicit def fromMixer: ToMachinery[Mixers] = ToMachinery.instance(m => Machinery(mixers = m))
  }

  case class Machinery(excavators: Excavators = 0.excavator, mixers: Mixers = 0.mixer) {
    def --(other: Machinery): Machinery = Machinery(excavators -- other.excavators, mixers -- other.mixers)
    def ++(other: Machinery): Machinery = Machinery(excavators ++ other.excavators, mixers ++ other.mixers)
  }

  object Machinery {
    val empty: Machinery = Machinery()
    implicit def fromExcavator(excavators: Excavators): Machinery = Machinery(excavators)
    implicit def fromMixer(mixers: Mixers): Machinery = Machinery(mixers = mixers)
  }

  @typeclass trait ToMachinery[A] {
    def to(value: A): Machinery
    def add(x: A, y: A): Machinery = to(x) ++ to(y)
    def minus(x: A, y: A): Machinery = to(x) -- to(y)
  }
  object ToMachinery {
    def instance[A](fn: A => Machinery): ToMachinery[A] = value => fn(value)
  }

  case class Resources(credit: Credits, machinery: Machinery) {
    def --(other: Resources): Resources = Resources(credit -- other.credit, machinery -- other.machinery)
    def ++(other: Resources): Resources = Resources(credit ++ other.credit, machinery ++ other.machinery)
  }
  object Resources {
    val empty: Resources = Resources(0.credit, Machinery.empty)
    implicit def fromCredit(credit: Credits): Resources = Resources(credit, Machinery.empty)
    implicit def fromMachinery(machinery: Machinery): Resources = Resources(Credits(0), machinery)
  }

  @newtype case class EngineerCount(count: Int)
  object EngineerCount extends PlusMinusOps[Int] {
    type T = EngineerCount
    implicit def engineerCountOrdering: Ordering[EngineerCount] = deriving
  }

  @newtype case class VictoryPoints(count: Int)
  object VictoryPoints extends PlusMinusOps[Int] {
    type T = VictoryPoints
  }

  @newtype case class Energy(count: Int)
  object Energy extends PlusMinusOps[Int] {
    type T = Energy
  }

  case class WheelSlot(tile: Option[TechnologyTile], machinery: Machinery)
  object WheelSlot {
    val empty = WheelSlot(None, Machinery.empty)
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
}
