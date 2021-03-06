package com.github.gpoirier.barrage

import cats.implicits._
import cats.data._
import io.estatico.newtype.macros.newtype

import resources._
import literals._
import commands._

object actions {

  case class Cost(engineers: EngineerCount = 0.eng, resources: Resources = Resources.empty)

  @newtype
  case class Rewards(value: NonEmptyChain[Reward])
  object Rewards {
    def apply(first: Reward, tail: Reward*): Rewards = Rewards(NonEmptyChain(first, tail: _*))
  }

  sealed trait Reward {
    def isComplete: Boolean
  }
  sealed trait CompleteReward extends Reward {
    def isComplete: Boolean = true
    def update: GameState => GameState
  }
  sealed trait PlayerStateCompleteReward extends CompleteReward {
    final def update: GameState => GameState = lens.currentPlayerState.modify(updatePlayer)
    def updatePlayer: PlayerState => PlayerState
  }
  sealed trait IncompleteReward extends Reward {
    def isComplete: Boolean = false
    def update: PartialFunction[RewardSelector, StateM[GameState, Unit]]
  }

  object Reward {

    case class WildMachinery(count: Int) extends IncompleteReward {
      def update: PartialFunction[RewardSelector, StateM[GameState, Unit]] = {
        case RewardSelector.WildMachinery(choices) if choices.count == 1 =>
          StateT.modify((lens.currentPlayerState composeLens lens.resources).modify(_ ++ choices))
      }
    }
    case class FixedResources(resources: Resources) extends PlayerStateCompleteReward {
      def updatePlayer: PlayerState => PlayerState = lens.resources.modify(_ ++ resources)
    }
    case class Wrench(count: Int) extends PlayerStateCompleteReward {
      def updatePlayer: PlayerState => PlayerState = _.spin(count)
    }

    def apply(resources: Resources): Reward = FixedResources(resources)
    def wild(count: Int): Reward = WildMachinery(count)
  }


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
    def rewards: Rewards
    def take: StateM[GameState, Cost]
    def cost(state: GameState): Result[Cost] = take.runA(state)
  }

  object Action {
    sealed abstract class Workshop(val spin: Int) extends Action {
      def rewards: Rewards = Rewards(Reward.Wrench(spin))

      def take: StateM[GameState, Cost] =
        for {
          column <- lens.workshop composeWith WorkshopSection.forAction(this)
        } yield cost(column)

      def cost(column: ActionColumn): Cost = this -> column match {
        case (Workshop.One, ActionColumn.Cheap) => Cost(1.eng)
        case (Workshop.One, ActionColumn.Expensive) => Cost(2.eng)
        case (Workshop.Two, ActionColumn.Cheap) => Cost(2.eng, 2.credits)
        case (Workshop.Two, ActionColumn.Expensive) => Cost(3.eng, 5.credits)
        case (Workshop.Three, ActionColumn.Cheap) => Cost(2.eng, 5.credits)
        case (Workshop.Three, ActionColumn.Expensive) => Cost(3.eng, 8.credits)
      }
    }
    object Workshop {
      object One extends Workshop(1)
      object Two extends Workshop(2)
      object Three extends Workshop(3)
    }

    sealed abstract class MachineShop(reward: Reward) extends Action {
      def rewards: Rewards = Rewards(reward)
      def take: StateM[GameState, Cost] =
        for {
          column <- lens.machineShop composeWith MachineShopSection.forAction(this)
        } yield cost(column)

      def cost(column: ActionColumn): Cost = this -> column match {
        case (MachineShop.Excavator, ActionColumn.Cheap) => Cost(1.eng, 2.credit)
        case (MachineShop.Excavator, ActionColumn.Expensive) => Cost(1.eng, 5.credit)
        case (MachineShop.Wild, ActionColumn.Cheap) => Cost(1.eng, 4.credit)
        case (MachineShop.Wild, ActionColumn.Expensive) => Cost(2.eng, 4.credit)
        case (MachineShop.Both, ActionColumn.Cheap) => Cost(2.eng, 5.credit)
        case (MachineShop.Both, ActionColumn.Expensive) => Cost(3.eng, 8.credit)
      }
    }
    object MachineShop {
      object Excavator extends MachineShop(Reward.FixedResources(Machinery(1.excavators)))
      object Wild extends MachineShop(Reward.WildMachinery(1))
      object Both extends MachineShop(Reward.FixedResources(Machinery(1.excavators, 1.mixer)))
    }
  }
}

object actions0 {

//  sealed trait Action {
//    def engineers: EngineerCount
//  }
//  sealed trait ResourceAction extends Action {
//    def cost: Credit
//    def reward: Reward
//  }
//  object Action {
//
//    case class ActionSpot[A <: Action](cheap: A, expensive: A)
//    object ActionSpot {
//      def apply[A <: Action](reward: Reward)(cheap: Reward => A, expensive: Reward => A): ActionSpot[A] =
//          ActionSpot(cheap(reward), expensive(reward))
//    }
//    case object Pass extends Action {
//      def engineers: EngineerCount = EngineerCount(0)
//    }
//    case class Build(
//      engineers: EngineerCount,
//      tile: TechnologyTile,
//      structureType: StructureType,
//      location: BuildLocation,
//      cost: Resources
//    ) extends Action
//
//    case class MachineShop(
//      engineers: EngineerCount,
//      cost: Credit,
//      reward: Reward
//    ) extends Action
//
//    object MachineShop {
//      val excavator = ActionSpot(Reward(Machinery(excavators = 1)))(
//        MachineShop(EngineerCount(1), Credit(2), _),
//        MachineShop(EngineerCount(1), Credit(5), _)
//      )
//
//      val wild = ActionSpot(Reward.wild(1))(
//        MachineShop(EngineerCount(1), Credit(4), _),
//        MachineShop(EngineerCount(2), Credit(4), _)
//      )
//
//      val both = ActionSpot(Reward(Machinery(excavators = 1, mixers = 1)))(
//        MachineShop(EngineerCount(2), Credit(5), _),
//        MachineShop(EngineerCount(3), Credit(8), _)
//      )
//    }
//
//    case class WorkShop(
//      engineers: EngineerCount,
//      cost: Credit,
//      spins: Int
//    ) extends Action
//
//    object WorkShop {
//      val one = ActionSpot(
//        cheap = WorkShop(EngineerCount(1), Credit(0), 1),
//        expensive = WorkShop(EngineerCount(2), Credit(0), 1)
//      )
//      val two = ActionSpot(
//        cheap = WorkShop(EngineerCount(2), Credit(2), 2),
//        expensive = WorkShop(EngineerCount(3), Credit(5), 2)
//      )
//      val three = ActionSpot(
//        cheap = WorkShop(EngineerCount(2), Credit(5), 3),
//        expensive = WorkShop(EngineerCount(3), Credit(8), 3)
//      )
//    }
//
//    case class ExternalWorks(
//      externalWork: ExternalWork
//    ) extends Action {
//      def engineers: EngineerCount = EngineerCount(2)
//    }
//
//    case class PatentOffice(
//      tile: TechnologyTile
//    ) extends Action {
//      def engineers: EngineerCount = EngineerCount(2)
//    }
//
//    sealed trait TurbineStation extends Action
//    sealed trait WaterManagement extends Action
//    sealed trait ContractOffice extends Action
//    sealed trait SpecialBuildings extends Action
//  }

//  sealed trait Reward
//  object Reward {
//    case class WildMachinery(count: Int) extends Reward
//    case class FixedResources(resources: Resources) extends Reward
//    case class Wrench(count: Int) extends Reward
//
//    def apply(resources: Resources): Reward = FixedResources(resources)
//
//    def wild(count: Int): Reward = WildMachinery(count)
//  }
}