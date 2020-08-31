package com.github.gpoirier.barrage

object actions {

  sealed trait LocationCost
  object LocationCost {
    case object Free extends LocationCost
    case object Paying extends LocationCost
  }

  sealed trait ConduitSide
  object Conduit {
    case object Right extends ConduitSide
    case object Left extends ConduitSide
  }

  sealed trait BuildLocation
  object BuildLocation {
    case class Dam(basin: Int, tpe: StructureType.Dam, cost: LocationCost) extends BuildLocation
    case class Conduit(basin: Int, ab: ConduitSide) extends BuildLocation
    case class PowerHouse(basin: Int, cost: LocationCost) extends BuildLocation
    case class SpecialBuilding(row: Int, cost: LocationCost) extends BuildLocation
  }

  sealed trait Action {
    def engineers: EngineerCount
  }
  object Action {

    case class ActionSpot[A <: Action](cheap: A, expensive: A)
    case object Pass extends Action {
      def engineers: EngineerCount = EngineerCount(0)
    }
    case class Build(
      engineers: EngineerCount,
      tile: TechnologyTile,
      structureType: StructureType,
      location: BuildLocation,
      cost: Resources
    ) extends Action

    case class MachineShop(
      engineers: EngineerCount,
      cost: Credit,
      reward: Machinery
    ) extends Action

    object MachineShop {
      val excavator = ActionSpot(
        cheap = MachineShop(EngineerCount(1), Credit(2), Machinery(excavators = 1)),
        expensive = MachineShop(EngineerCount(1), Credit(5), Machinery(excavators = 1))
      )

      def choice(tpe: MachineryType): ActionSpot[MachineShop] = {
        val machinery = tpe match {
          case MachineryType.Excavator => Machinery(excavators = 1)
          case MachineryType.Mixer => Machinery(mixers = 1)
        }
        ActionSpot(MachineShop(EngineerCount(1), Credit(4), machinery), MachineShop(EngineerCount(2), Credit(4), machinery))
      }

      val both = ActionSpot(
        cheap = MachineShop(EngineerCount(2), Credit(5), Machinery(excavators = 1, mixers = 1)),
        expensive = MachineShop(EngineerCount(3), Credit(8), Machinery(excavators = 1, mixers = 1))
      )
    }

    case class WorkShop(
      engineers: EngineerCount,
      cost: Credit,
      spins: Int
    ) extends Action

    object WorkShop {
      val one = ActionSpot(
        cheap = WorkShop(EngineerCount(1), Credit(0), 1),
        expensive = WorkShop(EngineerCount(2), Credit(0), 1)
      )
      val two = ActionSpot(
        cheap = WorkShop(EngineerCount(2), Credit(2), 2),
        expensive = WorkShop(EngineerCount(3), Credit(5), 2)
      )
      val three = ActionSpot(
        cheap = WorkShop(EngineerCount(2), Credit(5), 3),
        expensive = WorkShop(EngineerCount(3), Credit(8), 3)
      )
    }

    case class ExternalWorks(
      externalWork: ExternalWork
    ) extends Action {
      def engineers: EngineerCount = EngineerCount(2)
    }

    case class PatentOffice(
      tile: TechnologyTile
    ) extends Action {
      def engineers: EngineerCount = EngineerCount(2)
    }

    case class ContractOffice(
      engineers: EngineerCount,
      cost: Credit,
      contracts: Set[Contract]
    ) extends Action

    sealed trait TurbineStation extends Action
    sealed trait WaterManagement extends Action
    sealed trait SpecialBuildings extends Action
  }

}