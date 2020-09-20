package com.github.gpoirier.barrage

object actions2 {
  sealed trait Action

  object Action {
    sealed trait Workshop extends Action
    object Workshop {
      case object One extends Workshop
      case object Two extends Workshop
      case object Three extends Workshop
    }

    sealed trait MachineShop extends Action
    object MachineShop {
      case object Excavator extends MachineShop
      case object Wild extends MachineShop
      case object Both extends MachineShop
    }
  }
}

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
  sealed trait ResourceAction extends Action {
    def cost: Credit
    def reward: Reward
  }
  object Action {

    case class ActionSpot[A <: Action](cheap: A, expensive: A)
    object ActionSpot {
      def apply[A <: Action](reward: Reward)(cheap: Reward => A, expensive: Reward => A): ActionSpot[A] =
          ActionSpot(cheap(reward), expensive(reward))
    }
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
      reward: Reward
    ) extends Action

    object MachineShop {
      val excavator = ActionSpot(Reward(Machinery(excavators = 1)))(
        MachineShop(EngineerCount(1), Credit(2), _),
        MachineShop(EngineerCount(1), Credit(5), _)
      )

      val wild = ActionSpot(Reward.wild(1))(
        MachineShop(EngineerCount(1), Credit(4), _),
        MachineShop(EngineerCount(2), Credit(4), _)
      )

      val both = ActionSpot(Reward(Machinery(excavators = 1, mixers = 1)))(
        MachineShop(EngineerCount(2), Credit(5), _),
        MachineShop(EngineerCount(3), Credit(8), _)
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

    sealed trait TurbineStation extends Action
    sealed trait WaterManagement extends Action
    sealed trait ContractOffice extends Action
    sealed trait SpecialBuildings extends Action
  }

  sealed trait Reward
  object Reward {
    case class WildMachinery(count: Int) extends Reward
    case class FixedResources(resources: Resources) extends Reward
    case class Wrench(count: Int) extends Reward

    def apply(resources: Resources): Reward = FixedResources(resources)

    def wild(count: Int): Reward = WildMachinery(count)
  }
}