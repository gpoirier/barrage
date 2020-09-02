package com.github.gpoirier.barrage

import cats.data.NonEmptyList
import com.github.gpoirier.barrage.actions.Action
import scala.util.Random

case class GameState(
  currentPlayer: Company,
  turnOrder: List[Company],
  players: Map[Company, PlayerState],
  patentOffice: PatentOffice,
  externalWorks: ExternalWorks,
  contractOffice: ContractOffice
) {
  def currentPlayerState: PlayerState = players(currentPlayer)

  def nextPlayerState: PlayerState = players(nextPlayer)

  // If the last player pass, we keep him as the current player
  def nextPlayer: Company = {
    turnOrder.length match {
      case 0 | 1 => currentPlayer
      case n =>
        val index = turnOrder.indexOf(currentPlayer)
        turnOrder((index + 1) % n)
    }
  }
}
object GameState {

  def initial(turnOrder: NonEmptyList[Company]): GameState =
    GameState(
      currentPlayer = turnOrder.head,
      turnOrder = turnOrder.toList,
      players = turnOrder.map(_ -> PlayerState.initial).toList.toMap,
      // TODO Randomize first 3 tiles, and support for preselection with one tile per level removed
      patentOffice = PatentOffice(TechnologyTile.allForLevel(1).take(3)),
      externalWorks = ExternalWorks(Set(C1)),
      contractOffice = ContractOffice(GreenContracts.all.seed, YellowContracts.all.seed, RedContracts.all.seed)
    )

  private def forActivePlayer(f: PlayerState => PlayerState): GameState => GameState =
    lens.currentPlayerState.modify(f)

  private def resolveAction: Action => GameState => GameState = {
    case Action.Pass =>
      state => state.copy(turnOrder = state.turnOrder.filter(_ != state.currentPlayer))

    case Action.MachineShop(engineers, cost, reward) =>
      forActivePlayer {
        lens.playerCredits.modify(_ - cost) compose
          lens.playerMachinery.modify(_ + reward) compose
          lens.engineers.modify(_ - engineers)
      }

    case Action.WorkShop(engineers, cost, spins) =>
      forActivePlayer {
        lens.engineers.modify(_ - engineers) compose
          lens.playerCredits.modify(_ - cost) compose
          PlayerState.spin(spins)
      }

    case action @ Action.PatentOffice(tile) =>
      lens.patentOffice.modify(_ - tile) compose
        forActivePlayer {
          lens.engineers.modify(_ - action.engineers) compose
            lens.playerCredits.modify(_ - Credit(5)) compose
            lens.playerTiles.modify(_ + tile)
        }

    case Action.ExternalWorks(externalWork: ExternalWork) =>
      forActivePlayer(lens.engineers.modify(_ - EngineerCount(2)) andThen lens.playerMachinery.modify(_ - externalWork.cost)) andThen
        lens.externalWorks.modify(_ - externalWork) andThen
        externalWork.resolve

    case Action.ContractOffice(engineers, cost, contracts) =>
      forActivePlayer {
        lens.engineers.modify(_ - engineers) andThen
        lens.playerCredits.modify(_ - cost) andThen
        lens.contracts.modify(_ ++ contracts)
      } andThen lens.contractOffice.modify(_.replace(contracts))

    case _ => ???
  }

  def resolve(gameState: GameState, action: Action): GameState = {
    val nextPlayer = gameState.nextPlayer
    (resolveAction(action) andThen lens.currentPlayer.set(nextPlayer))(gameState)
  }

  /*
   * From the rulebook, End of Round is:
   *
   * Update the turn order. Put all Energy markers back to space "0". Take all of your Engineers back.
   */
  def endOfRound: GameState => GameState = { state =>
    val newTurnOrder = state.players.view.mapValues(_.energyProduction.energyCount).toList.sortBy(_._2).map(_._1)

    def updateTurnOrder = lens.turnOrder.set(newTurnOrder)
    def resetEnergyMarkers = lens.energyProduction.set(RoundProduction(0))
    def resetEngineers = lens.engineers.set(EngineerCount(12))

    val steps = updateTurnOrder andThen
        lens.eachPlayers.modify(resetEnergyMarkers andThen resetEngineers)

    steps(state)
  }
}

sealed trait Company
case object USA extends Company
case object France extends Company
case object Netherlands extends Company
case object Italy extends Company
case object Germany extends Company

case class PatentOffice(tiles: Set[TechnologyTile]) {
  def -(tile: TechnologyTile): PatentOffice = PatentOffice(tiles - tile)
}

case class ExternalWorks(externalWorks: Set[ExternalWork]) {
  def -(externalWork: ExternalWork): ExternalWorks = ExternalWorks(externalWorks - externalWork)
}

sealed trait ExternalWork {
  val cost: Machinery
  def resolve: GameState => GameState
}
object C1 extends ExternalWork {
  val cost = Machinery(excavators = 5)
  def resolve: GameState => GameState = {
    lens.currentPlayerState.modify(lens.points.modify(_ + VictoryPoints(6)) andThen lens.playerMachinery.modify(_ + Machinery(mixers = 3)))
  }
}

case class ContractOffice(greenContracts: Contracts, yellowContracts: Contracts, redContracts: Contracts) {

  def replace(contract: Contract): ContractOffice = contract match {
    case contract: GreenContract => copy(greenContracts = greenContracts.replace(contract))
    case contract: YellowContract => copy(yellowContracts = yellowContracts.replace(contract))
    case contract: RedContract => copy(redContracts = redContracts.replace(contract))
  }
  def replace(contracts: Set[Contract]): ContractOffice = {
    if (contracts.isEmpty) this
    else {
      val contract = contracts.head
      replace(contracts - contract).replace(contract)
    }
  }
}

abstract class Contract {
  val energy: Int
  def resolve: GameState => GameState
}
case class Contracts(stack: Stack, available: Available = Available(Set())) {

  def seed: Contracts = {
    val contract1 = stack.random
    val contract2 = (stack - contract1).random
    Contracts(stack = stack - contract1 - contract2, available = Available(Set(contract1, contract2)))
  }

  def replace(contract: Contract): Contracts = {
    val newContract = stack.random
    Contracts(stack = stack - newContract, available = available.replace(contract, newContract))
  }

}
case class Stack(stack: Set[Contract]) {
  val rnd = new Random
  def random: Contract = stack.toVector(rnd.nextInt(stack.size))
  def -(contract: Contract): Stack = Stack(stack - contract)
  def -(contracts: Set[Contract]): Stack = Stack(stack -- contracts)
}
case class Available(available: Set[Contract]) {
  def replace(oldContract: Contract, newContract: Contract): Available = Available(available - oldContract + newContract)
}

object StarterContracts {
  def all: Set[Contract] = Set(ContractS1, ContractS2, ContractS3, ContractS4)
  object ContractS1 extends Contract {
    val energy = 2
    def resolve: GameState => GameState = {
      lens.currentPlayerState.modify(lens.playerCredits.modify(_ + Credit(3)))
    }
  }
  object ContractS2 extends Contract {
    val energy = 3
    def resolve: GameState => GameState = ???
  }
  object ContractS3 extends Contract {
    val energy = 3
    def resolve: GameState => GameState = ???
  }
  object ContractS4 extends Contract {
    val energy = 4
    def resolve: GameState => GameState = ???
  }
}
sealed trait GreenContract extends Contract
object GreenContracts {
  def all: Contracts = Contracts(Stack(Set(ContractA1, ContractA2, ContractA3, ContractA4)))
  object ContractA1 extends GreenContract {
    val energy = 1
    def resolve: GameState => GameState = ???
  }
  object ContractA2 extends GreenContract {
    val energy = 1
    def resolve: GameState => GameState = ???
  }
  object ContractA3 extends GreenContract {
    val energy = 1
    def resolve: GameState => GameState = ???
  }
  object ContractA4 extends GreenContract {
    val energy = 1
    def resolve: GameState => GameState = ???
  }
}
sealed trait YellowContract extends Contract
object YellowContracts {
  def all: Contracts = Contracts(Stack(Set(ContractB1, ContractB2, ContractB3, ContractB4)))
  object ContractB1 extends YellowContract {
    val energy = 1
    def resolve: GameState => GameState = ???
  }
  object ContractB2 extends YellowContract {
    val energy = 1
    def resolve: GameState => GameState = ???
  }
  object ContractB3 extends YellowContract {
    val energy = 1
    def resolve: GameState => GameState = ???
  }
  object ContractB4 extends YellowContract {
    val energy = 1
    def resolve: GameState => GameState = ???
  }
}
sealed trait RedContract extends Contract
object RedContracts {
  def all: Contracts = Contracts(Stack(Set(ContractC1, ContractC2, ContractC3, ContractC4)))
  object ContractC1 extends RedContract {
    val energy = 1
    def resolve: GameState => GameState = ???
  }
  object ContractC2 extends RedContract {
    val energy = 1
    def resolve: GameState => GameState = ???
  }
  object ContractC3 extends RedContract {
    val energy = 1
    def resolve: GameState => GameState = ???
  }
  object ContractC4 extends RedContract {
    val energy = 1
    def resolve: GameState => GameState = ???
  }
}