package com.github.gpoirier.barrage

import monocle.Lens
import monocle.macros.GenLens

object lens {
  val wheel: Lens[PlayerState, Wheel] = GenLens[PlayerState](_.wheel)
  val resources: Lens[PlayerState, Resources] = GenLens[PlayerState](_.resources)
  val engineers: Lens[PlayerState, EngineerCount] = GenLens[PlayerState](_.engineers)
  val playerTiles: Lens[PlayerState, Set[TechnologyTile]] = GenLens[PlayerState](_.tiles)
  val energyProduction: Lens[PlayerState, RoundProduction] = GenLens[PlayerState](_.energyProduction)

  val credits: Lens[Resources, Credit] = GenLens[Resources](_.credit)
  val machinery: Lens[Resources, Machinery] = GenLens[Resources](_.machinery)
  val playerCredits: Lens[PlayerState, Credit] = resources composeLens credits
  val playerMachinery: Lens[PlayerState, Machinery] = resources composeLens machinery

  val players: Lens[GameState, Map[Company, PlayerState]] = GenLens[GameState](_.players)
  val activePlayer: Lens[GameState, PlayerState] =
    Lens[GameState, PlayerState](_.currentPlayerState)(player => game => players.modify(_ + (game.currentPlayer -> player))(game))

  val patentOffice: Lens[GameState, PatentOffice] = GenLens[GameState](_.patentOffice)
  val turnOrder: Lens[GameState, List[Company]] = GenLens[GameState](_.turnOrder)

}
