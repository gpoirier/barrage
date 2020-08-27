package com.github.gpoirier.barrage

case class GameState(
                    players: Map[String, PlayerState],
                    patentOffice: PatentOffice
                    )
object GameState {}

case class PatentOffice(tiles: Set[TechnologyTile])
object PatentOffice {
  def removeTile(patentOffice: PatentOffice, tile: TechnologyTile): PatentOffice = {
    patentOffice.copy(tiles = patentOffice.tiles.filterNot(_ == tile))
  }
}

