package com.github.gpoirier.barrage

import com.github.gpoirier.barrage.actions.Action

case class GameState(
  currentPlayer: Company,
  turnOrder: List[Company],
  players: Map[Company, PlayerState],
  patentOffice: PatentOffice
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

  private def forActivePlayer(f: PlayerState => PlayerState): GameState => GameState =
    lens.activePlayer.modify(f)

  def resolveAction: Action => GameState => GameState = {
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

    case _ => ???
  }

  def resolve(gameState: GameState, action: Action): GameState = {
    val nextPlayer = gameState.nextPlayerState
    (resolveAction(action) compose lens.activePlayer.set(nextPlayer))(gameState)
  }

  /*
   * From the rulebook, End of Round is:
   *
   * Update the turn order. Put all Energy markers back to space "0". Take all of your Engineers back.
   */
  def endOfRound: GameState => GameState = { state =>
    val newTurnOrder = state.players.view.mapValues(_.energyProduction.energyCount).toList.sortBy(_._2).map(_._1)

    def resetTurnOrder = lens.turnOrder.set(newTurnOrder)
    def resetEnergyMarkers = lens.energyProduction.set(RoundProduction(0))
    def resetEngineers = lens.engineers.set(EngineerCount(12))

    val steps = resetTurnOrder compose
        lens.eachPlayers.modify(resetEnergyMarkers compose resetEngineers)

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