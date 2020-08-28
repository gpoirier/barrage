package com.github.gpoirier.barrage

case class GameState(
                    players: Map[String, PlayerState],
                    patentOffice: PatentOffice
                    )
object GameState {}

case class PatentOffice(tiles: Set[TechnologyTile]) {
  def -(tile: TechnologyTile): PatentOffice = PatentOffice(tiles - tile)
}
object PatentOffice {
  def removeTile(patentOffice: PatentOffice, tile: TechnologyTile): PatentOffice = {
    patentOffice - tile
  }
}

