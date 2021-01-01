package com.github.gpoirier.barrage

import com.github.gpoirier.barrage.BoardState.Basin.{ConduitSpot, DamBasin, DamSpot, PowerHouseBasin}
import com.github.gpoirier.barrage.BoardState.{Basin, Water}


case class BoardState(
                     basins: Set[Basin]
                     ) {

  def addWater(basin: Basin, count: Water): BoardState = {
    val (newBasin, overflow) = basin.addWater(count)
    val newBasins = basins - basin + newBasin
    if (overflow == 0 || basin.overflowsTo == None) copy(basins = newBasins)
    else copy(basins = newBasins).addWater(basin.overflowsTo.get, overflow)
  }

}

object BoardState {

  type Water = Int

  def initial: BoardState = BoardState(
    basins = Set[Basin]()
  )

  sealed trait Basin {
    def addWater(count: Water): (Basin, Water)
    val overflowsTo: Option[Basin]
  }

  object Basin {

    case class DamSpot(
                        isEmpty: Boolean = true,
                        height: Int = 0, //should probably be dam.height
                        water: Water = 0,
                        //dam: Option[Dam] = None,
                      ) {

      def addWater(count: Water): (DamSpot, Water) = {
        val total = water + count
        if (total > height) (copy(water = height), total - height)
        else (copy(water = total), 0)
      }
    }

    case class ConduitSpot(
                            isEmpty: Boolean = true,
                            flowsTo: Basin,
                            level: Int
                          )

    case class DamBasin(
                         upperDam: DamSpot,
                         lowerDam: DamSpot,
                         overflowsTo: Option[Basin],
                         conduitA: ConduitSpot,
                         conduitB: ConduitSpot,
                       ) extends Basin {

      def addWater(count: Water): (DamBasin, Water) = {
        val (newUpper, overflow) = upperDam.addWater(count)
        if (overflow == 0) (copy(upperDam = newUpper), 0)
        else {
          val (newLower, overflow2) = lowerDam.addWater(overflow)
          (copy(upperDam = newUpper, lowerDam = newLower), overflow2)
        }
      }
    }

    case class PowerHouseSpot()

    case class PowerHouseBasin() extends Basin {
      def addWater(count: Water): (Basin, Water) = (this, 0)
      val overflowsTo: Option[Basin] = None
    }


  }



}


case class Basins(basins: Set[Basin])

object Basins {
  object Basin1 extends DamBasin(
    upperDam = DamSpot(),
    lowerDam = DamSpot(),
    overflowsTo = Option(Basin8),
    conduitA = ConduitSpot(flowsTo = Basin8, level = 5),
    conduitB = ConduitSpot(flowsTo = Basin5, level = 4)
  )
  object Basin2 extends DamBasin(
    upperDam = DamSpot(),
    lowerDam = DamSpot(),
    overflowsTo = Option(Basin5),
    conduitA = ConduitSpot(flowsTo = Basin6, level = 3),
    conduitB = ConduitSpot(flowsTo = Basin10, level = 5)
  )
  object Basin3 extends DamBasin(
    upperDam = DamSpot(),
    lowerDam = DamSpot(),
    overflowsTo = Option(Basin6),
    conduitA = ConduitSpot(flowsTo = Basin5, level = 4),
    conduitB = ConduitSpot(flowsTo = Basin6, level = 3)
  )
  object Basin4 extends DamBasin(
    upperDam = DamSpot(),
    lowerDam = DamSpot(),
    overflowsTo = Option(Basin7),
    conduitA = ConduitSpot(flowsTo = Basin7, level = 3),
    conduitB = ConduitSpot(flowsTo = Basin12, level = 5)
  )
  object Basin5 extends DamBasin(
    upperDam = DamSpot(),
    lowerDam = DamSpot(),
    overflowsTo = Option(Basin9),
    conduitA = ConduitSpot(flowsTo = Basin8, level = 3),
    conduitB = ConduitSpot(flowsTo = Basin10, level = 4)
  )
  object Basin6 extends DamBasin(
    upperDam = DamSpot(),
    lowerDam = DamSpot(),
    overflowsTo = Option(Basin10),
    conduitA = ConduitSpot(flowsTo = Basin9, level = 4),
    conduitB = ConduitSpot(flowsTo = Basin7, level = 2)
  )
  object Basin7 extends DamBasin(
    upperDam = DamSpot(),
    lowerDam = DamSpot(),
    overflowsTo = Option(Basin10),
    conduitA = ConduitSpot(flowsTo = Basin10, level = 2),
    conduitB = ConduitSpot(flowsTo = Basin12, level = 3)
  )
  object Basin8 extends DamBasin(
    upperDam = DamSpot(),
    lowerDam = DamSpot(),
    overflowsTo = Option(Basin11),
    conduitA = ConduitSpot(flowsTo = Basin11, level = 3),
    conduitB = ConduitSpot(flowsTo = Basin9, level = 2)
  )
  object Basin9 extends DamBasin(
    upperDam = DamSpot(),
    lowerDam = DamSpot(),
    overflowsTo = Option(Basin11),
    conduitA = ConduitSpot(flowsTo = Basin11, level = 1),
    conduitB = ConduitSpot(flowsTo = Basin12, level = 3)
  )
  object Basin10 extends DamBasin(
    upperDam = DamSpot(),
    lowerDam = DamSpot(),
    overflowsTo = Option(Basin12),
    conduitA = ConduitSpot(flowsTo = Basin11, level = 2),
    conduitB = ConduitSpot(flowsTo = Basin12, level = 1)
  )
  object Basin11 extends PowerHouseBasin()
  object Basin12 extends PowerHouseBasin()
}





