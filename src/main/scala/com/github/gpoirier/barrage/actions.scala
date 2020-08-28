package com.github.gpoirier.barrage

import com.github.gpoirier.barrage.rewards.{ResourceReward, TechnologyTileReward, WrenchReward}

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

  sealed trait Action
  object Action {
    case class Build(
      engineers: Int,
      tile: TechnologyTile,
      structureType: StructureType,
      location: BuildLocation,
      cost: Resources
    ) extends Action

    case class MachineShop(
      engineers: Int,
      credit: Credit,
      reward: ResourceReward
    ) extends Action

    case class WorkShop(
      engineers: Int,
      credit: Credit,
      reward: WrenchReward
    ) extends Action

    case class PatentOffice(
      tile: TechnologyTileReward
    ) extends Action

    sealed trait TurbineStation extends Action
    sealed trait WaterManagement extends Action
    sealed trait ContractOffice extends Action
    sealed trait ExternalWorks extends Action
    sealed trait SpecialBuildings extends Action
  }

}