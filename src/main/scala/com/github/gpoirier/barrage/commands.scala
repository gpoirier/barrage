package com.github.gpoirier.barrage

import actions._

object commands {
  case class Command(action: Action, rewardsHandlers: List[RewardHandler])

  sealed trait RewardHandler
  object RewardSelector {
    case class WildMachinery(choices: Machinery) extends RewardHandler
    // case class ExternalWork(id: ExternalWork.Id) extends RewardHandler
    // case class Contract(id: Contract.Id) extends RewardHandler
  }
}
