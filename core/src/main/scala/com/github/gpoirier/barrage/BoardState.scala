package com.github.gpoirier.barrage

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
                            flowsTo: DamSpot,
                            level: Int
                          )

    case class DamBasin(
                         upper: DamSpot,
                         lower: DamSpot,
                         overflowsTo: Option[Basin]
                         //conduitA: ConduitSpot,
                         //conduitB: ConduitSpot,
                       ) extends Basin {

      def addWater(count: Water): (DamBasin, Water) = {
        val (newUpper, overflow) = upper.addWater(count)
        if (overflow == 0) (copy(upper = newUpper), 0)
        else {
          val (newLower, overflow2) = lower.addWater(overflow)
          (copy(upper = newUpper, lower = newLower), overflow2)
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

object Basins





