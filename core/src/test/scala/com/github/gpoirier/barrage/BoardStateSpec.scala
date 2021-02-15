package com.github.gpoirier.barrage

import com.github.gpoirier.barrage.BoardState.Basin.{DamBasin, DamSpot}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BoardStateSpec extends AnyFlatSpec with Matchers {

  behavior of "basins"

//  it should "overflow dams correctly" in {
//
//    val basin2 = DamBasin(
//      upperDam = DamSpot(height = 0, water = 0),
//      lowerDam = DamSpot(height = 2, water = 1),
//      overflowsTo = None
//    )
//    //this is the upper basin, but I get a "forward reference" compile error if I declare it above
//    val basin1 = DamBasin(
//      upperDam = DamSpot(height = 1, water = 1),
//      lowerDam = DamSpot(height = 3, water = 0),
//      overflowsTo = Some(basin2)
//    )
//
//    val before = BoardState(basins = Set(basin1, basin2))
//
//    val after = before.addWater(basin1, 5)
//
//    after.basins shouldBe Set(
//      DamBasin(
//        upperDam = DamSpot(height = 1, water = 1),
//        lowerDam = DamSpot(height = 3, water = 3),
//        overflowsTo = Some(basin2)
//      ),
//      DamBasin(
//        upperDam = DamSpot(height = 0, water = 0),
//        lowerDam = DamSpot(height = 2, water = 2),
//        overflowsTo = None
//      )
//    )
//
//    val after2 = before.addWater(basin1, 1)
//
//    after2.basins shouldBe Set(
//      DamBasin(
//        upperDam = DamSpot(height = 0, water = 0),
//        lowerDam = DamSpot(height = 2, water = 1),
//        overflowsTo = None
//      ),
//      DamBasin(
//        upperDam = DamSpot(height = 1, water = 1),
//        lowerDam = DamSpot(height = 3, water = 1),
//        overflowsTo = Some(basin2)
//      )
//    )
//
//
//
//
//  }
}
