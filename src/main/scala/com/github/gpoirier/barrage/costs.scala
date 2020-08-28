package com.github.gpoirier.barrage

object costs {

  sealed trait Cost
  case class Costs(costs: List[Cost]) extends Cost
  case class EngineerCost(count: Int) extends Cost
  case class CreditCost(credit: Credit) extends Cost
  case class MachineryCost(machinery: Machinery) extends Cost
//  case class WheelCost(resources: Resources, technologyTile: TechnologyTile) extends Cost
}