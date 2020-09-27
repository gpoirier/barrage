package com.github.gpoirier.barrage

import cats.data._
import cats.implicits._
import monocle.Lens
import monocle.macros.GenLens

import scala.annotation.tailrec

sealed abstract class Availability(val isEmpty: Boolean)
case object OccupiedSpot extends Availability(isEmpty = false)
case object FreeSpot extends Availability(isEmpty = true)

sealed trait ActionColumn
object ActionColumn {
  case object Cheap extends ActionColumn
  case object Expensive extends ActionColumn
}

trait Section {
  type F[A] = Either[String, A]
  type Rows

  protected def reserve(name: String, f: GenLens[Rows] => Lens[Rows, Row]): StateT[F, Rows, ActionColumn] = {
    (f(GenLens[Rows]) composeWith Row.reserve).transformF(_.leftMap(_ ++ " (" ++ name ++ ")"))
  }

  case class Spot(column: ActionColumn, availability: Availability = FreeSpot) {
    def reserve: Spot = copy(availability = OccupiedSpot)
  }
  case class Row(spots: List[Spot])

  object Row {

    val emptyCommonRow: Row = Row(List(Spot(ActionColumn.Cheap), Spot(ActionColumn.Expensive)))

    def reserve: StateT[F, Row, ActionColumn] = StateT[F, Row, ActionColumn] {
      case Row(spots) =>
        @tailrec
        def go(acc: List[Spot], list: List[Spot]): (Option[ActionColumn], List[Spot]) = list match {
          case head :: tail =>
            if (head.availability.isEmpty) {
              val spot = head.reserve
              Some(spot.column) -> (acc ::: spot :: tail)
            } else {
              go(acc ::: head :: Nil, tail)
            }
          case Nil =>
            None -> acc
        }

        go(Nil, spots) match {
          case (Some(column), list) =>
            (Row(list) -> column).asRight
          case (None, _) =>
            "No empty action spot left".asLeft
        }
    }
  }
}
