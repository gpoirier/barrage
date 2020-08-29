package com.github.gpoirier.barrage

import monocle.Lens
import monocle.macros.GenLens

object lens {
  val wheel: Lens[PlayerState, Wheel] = GenLens[PlayerState](_.wheel)
  val resources: Lens[PlayerState, Resources] = GenLens[PlayerState](_.resources)
  val engineers: Lens[PlayerState, EngineerCount] = GenLens[PlayerState](_.engineers)

  val credits: Lens[Resources, Credit] = GenLens[Resources](_.credit)
  val machinery: Lens[Resources, Machinery] = GenLens[Resources](_.machinery)
  val playerCredits: Lens[PlayerState, Credit] = resources composeLens credits
  val playerMachinery: Lens[PlayerState, Machinery] = resources composeLens machinery
}
