package com.github.gpoirier.barrage

import com.github.gpoirier.barrage.actions.Action
import com.github.gpoirier.barrage.costs._
import com.github.gpoirier.barrage.rewards._

case class GameState(
                    players: Map[Player, PlayerState],
                    patentOffice: PatentOffice
                    )
object GameState {
  def resolveAction(gameState: GameState, player: Player, action: Action): GameState = {

    val playerState = gameState.players(player)

    action match {

      case Action.MachineShop(engineers, credit, reward) =>
        val costPaid = playerState.resolveCost(Costs(List(EngineerCost(engineers), CreditCost(credit))))
        resolveReward(gameState.copy(players = gameState.players.updated(player, costPaid)), player, reward)

      case Action.WorkShop(engineers, credit, reward) =>
        val costPaid = playerState.resolveCost(Costs(List(EngineerCost(engineers), CreditCost(credit))))
        resolveReward(gameState.copy(players = gameState.players.updated(player, costPaid)), player, reward)

      case Action.PatentOffice(reward) =>
        val costPaid = playerState.resolveCost(Costs(List(EngineerCost(2), CreditCost(Credit(5)))))
        resolveReward(gameState.copy(players = gameState.players.updated(player, costPaid)), player, reward)
      case _ => ???
    }
  }

   def resolveReward(gameState: GameState, player: Player, reward: Reward): GameState = {

     val playerState = gameState.players(player)

     reward match {
       case Rewards(rewards) => rewards match {
         case Nil => gameState
         case head :: tail => resolveReward(resolveReward(gameState, player, tail.asInstanceOf[Reward]), player, head)
       }
       case ResourceReward(resources: Resources) =>
         val newResources = playerState.resources + resources
         val newPlayerState = playerState.copy(resources = newResources)
         gameState.copy(players = gameState.players.updated(player, newPlayerState))

       case TechnologyTileReward(technologyTile: TechnologyTile) =>
         val newPlayerState = playerState.copy(tiles = playerState.tiles + technologyTile)
         gameState.copy(players = gameState.players.updated(player, newPlayerState))

       case WrenchReward(count: Int) =>
         val newPlayerState = playerState.spin(count)
         gameState.copy(players = gameState.players.updated(player, newPlayerState))
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

