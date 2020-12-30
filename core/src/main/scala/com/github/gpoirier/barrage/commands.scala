package com.github.gpoirier.barrage

import cats.data._

import resources._
import actions._

object commands {
  case class Command(action: Action, rewardsSelectors: Chain[RewardSelector])
  object Command {
    def apply(action: Action, rewardsSelectors: List[RewardSelector]): Command =
      Command(action, Chain.fromSeq(rewardsSelectors))
  }

  sealed trait RewardSelector
  object RewardSelector {
    case class WildMachinery(choices: Machinery) extends RewardSelector
    // case class ExternalWork(id: ExternalWork.Id) extends RewardSelector
    // case class Contract(id: Contract.Id) extends RewardSelector
  }
}
