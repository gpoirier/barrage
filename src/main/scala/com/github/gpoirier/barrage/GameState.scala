package com.github.gpoirier.barrage

import com.github.gpoirier.barrage.actions.Action

case class GameState(
                    players: Map[Player, PlayerState],
                    patentOffice: PatentOffice
                    )
object GameState {
  def resolve(gameState: GameState, player: Player, action: Action): GameState = {

    val playerState = gameState.players(player)

    action match {

      case Action.MachineShop(engineers, cost, reward) =>
        val update =
          lens.playerCredits.modify(_ - cost) compose
          lens.playerMachinery.modify(_ + reward) compose
          lens.engineers.modify(_ - engineers)

        gameState.copy(players = gameState.players.updated(player, update(playerState)))
      case Action.WorkShop(engineers, cost, spins) =>
        val newPlayerState = playerState.copy(
          engineers = playerState.engineers - engineers,
          resources = playerState.resources.copy(credit = playerState.resources.credit - cost)
        ).spin(spins)
        gameState.copy(players = gameState.players.updated(player, newPlayerState))
      case Action.PatentOffice(tile) =>
        val newPatentOffice = gameState.patentOffice.copy(tiles = gameState.patentOffice.tiles - tile)
        val newPlayerState = playerState.copy(
          engineers = playerState.engineers - action.engineers,
          resources = playerState.resources.copy(credit = playerState.resources.credit - Credit(5)),
          tiles = playerState.tiles + tile
        )
        gameState.copy(players = gameState.players.updated(player, newPlayerState), patentOffice = newPatentOffice)
      case _ => ???
    }
  }
}

case class Player(name: String)

case class PatentOffice(tiles: Set[TechnologyTile]) {
  def -(tile: TechnologyTile): PatentOffice = PatentOffice(tiles - tile)
}
object PatentOffice {
  def removeTile(patentOffice: PatentOffice, tile: TechnologyTile): PatentOffice = {
    patentOffice - tile
  }
}

