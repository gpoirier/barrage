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

  import actions._
  import cats._
  import cats.implicits._

  type Result[A] = Either[String, A]
  def test: StateT[Either[String, *], GameState, actions.Cost] = ???
  def test1: StateM[GameState, actions.Cost] = StateT[Result, GameState, Cost] { state =>
    Right(state -> actions.Cost())
  }

  def test2[F[_]: Monad](implicit S: cats.mtl.Stateful[F, GameState]): F[Cost] = {
    for {
      state <- S.get
      _ <- S.set(GameState.initial(???))
    } yield Cost(state.currentPlayerState.engineers)
  }

  trait Console[F[_]] {
    def print(text: String): F[Unit]
  }

  object Console {
    implicit def forSync[F[_]: Sync]: Console[F] = new Console[F] {
      def print(text: String): F[Unit] = Sync[F].delay(println(text))
    }

    import cats.Id
    def unsafeForTest: Console[Id] = new Console[Id] {
      def print(text: String): Id[Unit] = println(text)
    }
  }

  trait Console0 {
    def print(hello: String): IO[Unit]
  }

  test2[State[GameState, *]]
  test2[StateM[GameState, *]]
  test2[StateT[Either[String, *], GameState, *]]
  test2[StateT[IO, GameState, *]]





}
