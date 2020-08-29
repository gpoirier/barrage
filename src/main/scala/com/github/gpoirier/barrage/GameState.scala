package com.github.gpoirier.barrage

import com.github.gpoirier.barrage.actions.Action

case class GameState(
  currentPlayer: Company,
  players: Map[Company, PlayerState],
  patentOffice: PatentOffice
) {
  def currentPlayerState: PlayerState = players(currentPlayer)
}
object GameState {

  def forActivePlayer(f: PlayerState => PlayerState): GameState => GameState =
    lens.activePlayer.modify(f)

  def resolveAction: Action => GameState => GameState = {
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

  def resolve(gameState: GameState, action: Action): GameState =
    resolveAction(action)(gameState)
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