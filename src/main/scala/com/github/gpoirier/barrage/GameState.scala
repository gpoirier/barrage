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
        val newResources = Resources(playerState.resources.credit - cost, playerState.resources.machinery + reward)
        val newPlayerState = playerState.copy(
          engineers = playerState.engineers - engineers,
          resources = newResources
        )
        gameState.copy(players = gameState.players.updated(player, newPlayerState))
      case Action.WorkShop(engineers, cost, spins) =>
        val newPlayerState = playerState.copy(
          engineers = playerState.engineers - engineers,
          resources = playerState.resources.copy(credit = playerState.resources.credit - cost)
        ).spin(spins)
        gameState.copy(players = gameState.players.updated(player, newPlayerState))
      case Action.PatentOffice(tile) =>
        val newPatentOffice = gameState.patentOffice.copy(tiles = gameState.patentOffice.tiles - tile)
        val newPlayerState = playerState.copy(
          engineers = playerState.engineers - 2,
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

