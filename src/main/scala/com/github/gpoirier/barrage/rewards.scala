package com.github.gpoirier.barrage

object rewards {

  sealed trait Reward
  case class Rewards(rewards: List[Reward]) extends Reward
  case class ResourceReward(resources: Resources) extends Reward
  case class TechnologyTileReward(technologyTile: TechnologyTile) extends Reward
  case class WrenchReward(count: Int = 1) extends Reward
//  case class BuildingReward(structureType: StructureType, location: Basin) extends Reward
//  case class EnergyReward(energy: Energy, canCompleteContract: Boolean) extends Reward
//  case class WaterReward(count: Int, flowing: Boolean) extends Reward
}