package com.github.gpoirier.barrage

import resources._
import actions._

object commands {
  case class Command(action: Action, rewardsHandlers: List[RewardSelector])

  sealed trait RewardSelector
  object RewardSelector {
    case class WildMachinery(choices: Machinery) extends RewardSelector
    // case class ExternalWork(id: ExternalWork.Id) extends RewardSelector
    // case class Contract(id: Contract.Id) extends RewardSelector
  }
}
