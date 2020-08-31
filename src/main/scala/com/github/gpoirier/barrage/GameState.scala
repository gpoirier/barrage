package com.github.gpoirier.barrage

import cats.data.NonEmptyList
import com.github.gpoirier.barrage.actions.Action

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
      contractOffice = ContractOffice(Set())
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
    lens.currentPlayerState.modify(lens.points.modify(_ + VictoryPoints(4)) andThen lens.playerMachinery.modify(_ + Machinery(mixers = 3)))
  }
}

case class ContractOffice(contracts: Set[Contract]) {

  val availableGreenContracts = StarterContracts.all
  

  def replace(contracts: Set[Contract]): ContractOffice = ???

}
object ContractOffice {

}

sealed trait Contract {
  val energy: Int
  def resolve: GameState => GameState
}

object StarterContracts {
  def all: Set[Contract] = Set(S1, S2, S3, S4)
  object S1 extends Contract with {
    val energy = 2
    def resolve: GameState => GameState = {
      lens.currentPlayerState.modify(lens.playerCredits.modify(_ + Credit(3)))
    }
  }

  object S2 extends Contract with {
    val energy = 3
    def resolve: GameState => GameState = ???
  }

  object S3 extends Contract {
    val energy = 3
    def resolve: GameState => GameState = ???
  }

  object S4 extends Contract {
    val energy = 4]
    def resolve: GameState => GameState = ???
  }
}