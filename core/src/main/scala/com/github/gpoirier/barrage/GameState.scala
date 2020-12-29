package com.github.gpoirier.barrage

import cats.implicits._
import cats.data.{NonEmptyList, StateT}
import com.github.gpoirier.barrage.actions.Action
import com.github.gpoirier.barrage.commands.Command
import resources._
import literals._

case class GameState(
  currentPlayer: Company,
  turnOrder: List[Company],
  players: Map[Company, PlayerState],
  patentOffice: PatentOffice,
  externalWorks: ExternalWorks,
  machineShop: MachineShop.Rows,
  workshop: Workshop.Rows
) {
  def currentPlayerState: PlayerState = players(currentPlayer)

  def nextPlayerState: PlayerState = players(nextPlayer)

  // If the last player pass, we keep him as the current player
  def nextPlayer: Company = {
    turnOrder.length match {
      case 0 | 1 => currentPlayer
      case n =>
        val index = turnOrder.indexOf(currentPlayer)
        turnOrder((index + 1) % n)
    }
  }
}
object GameState {

  type F[A] = Either[String, A]

  def initial(turnOrder: NonEmptyList[Company]): GameState =
    GameState(
      currentPlayer = turnOrder.head,
      turnOrder = turnOrder.toList,
      players = turnOrder.map(_ -> PlayerState.initial).toList.toMap,
      // TODO Randomize first 3 tiles, and support for preselection with one tile per level removed
      patentOffice = PatentOffice(TechnologyTile.allForLevel(1).take(3)),
      externalWorks = ExternalWorks(Set()),
      MachineShop.empty,
      Workshop.empty
    )

  def simple: GameState = initial(NonEmptyList.of(USA, Germany, Italy, France))

  private def forActivePlayer(f: PlayerState => PlayerState): GameState => GameState =
    lens.currentPlayerState.modify(f)

  private def forActivePlayerF(f: PlayerState => PlayerState): StateT[F, GameState, Unit] =
    StateT.modify[F, GameState](forActivePlayer(f))

  private[barrage] def resolveCommand: Command => StateT[F, GameState, Unit] = {
    case Command(action: Action.Workshop, Nil) =>
      lens.workshop composeWith Workshop.forAction(action) flatMap { column =>
        lens.currentPlayerState composeWith {
          for {
            _ <- PlayerState.payCost(action.cost(column))
            _ <- PlayerState.spin(action.spin)
          } yield ()
        }
      }
    case Command(action: Action.MachineShop, Nil) =>
      lens.machineShop composeWith MachineShop.forAction(action) flatMap { column =>
        lens.currentPlayerState composeWith {
          for {
            _ <- PlayerState.payCost(action.cost(column))
            _ <- PlayerState.addResources(Resources(credit = 0.credit, machinery = action.machinery))
          } yield ()
        }
      }
  }

//  private def resolveAction: Action => GameState => GameState = {
//    case Action.Pass =>
//      state => state.copy(turnOrder = state.turnOrder.filter(_ != state.currentPlayer))
//
////    case Action.MachineShop(engineers, cost, reward) =>
////      forActivePlayer {
////        lens.playerCredits.modify(_ - cost) compose
////          lens.playerMachinery.modify(_ + reward) compose
////          lens.engineers.modify(_ - engineers)
////      }
//
//    case Action.WorkShop(engineers, cost, spins) =>
//      forActivePlayer {
//        lens.engineers.modify(_ - engineers) compose
//          lens.playerCredits.modify(_ - cost) compose
//          PlayerState.spin(spins)
//      }
//
//    case action @ Action.PatentOffice(tile) =>
//      lens.patentOffice.modify(_ - tile) compose
//        forActivePlayer {
//          lens.engineers.modify(_ - action.engineers) compose
//            lens.playerCredits.modify(_ - Credit(5)) compose
//            lens.playerTiles.modify(_ + tile)
//        }
//
//    case Action.ExternalWorks(externalWork: ExternalWork) =>
//      forActivePlayer(lens.engineers.modify(_ - EngineerCount(2)) andThen lens.playerMachinery.modify(_ - externalWork.cost)) andThen
//        lens.externalWorks.modify(_ - externalWork) andThen
//        externalWork.resolve
//
//    case _ => ???
//  }

//  def resolve(gameState: GameState, action: Action): GameState = {
//    val nextPlayer = gameState.nextPlayer
//    (resolveAction(action) andThen lens.currentPlayer.set(nextPlayer))(gameState)
//  }

  def resolve(command: Command): StateM[GameState, Unit] = {
    for {
      nextPlayer <- StateT.inspect[Result, GameState, Company](_.nextPlayer)
      _ <- resolveCommand(command)
      _ <- StateT.modify[Result, GameState](lens.currentPlayer.set(nextPlayer))
    } yield ()
  }

  /*
   * From the rulebook, End of Round is:
   *
   * Update the turn order. Put all Energy markers back to space "0". Take all of your Engineers back.
   */
  def endOfRound: GameState => GameState = { state =>
    val newTurnOrder = state.players.view.mapValues(_.energyProduction.count).toList.sortBy(_._2).map(_._1)

    def updateTurnOrder = lens.turnOrder.set(newTurnOrder)
    def resetEnergyMarkers = lens.energyProduction.set(Energy(0))
    def resetEngineers = lens.engineers.set(EngineerCount(12))

    val steps = updateTurnOrder andThen
        lens.eachPlayers.modify(resetEnergyMarkers andThen resetEngineers)

    steps(state)
  }
}

sealed trait Company
case object USA extends Company
case object France extends Company
case object Netherlands extends Company
case object Italy extends Company
case object Germany extends Company

case class PatentOffice(tiles: Set[TechnologyTile]) {
  def -(tile: TechnologyTile): PatentOffice = PatentOffice(tiles - tile)
}

case class ExternalWorks(externalWorks: Set[ExternalWork]) {
  def -(externalWork: ExternalWork): ExternalWorks = ExternalWorks(externalWorks - externalWork)
}

sealed trait ExternalWork {
  val cost: Machinery
  def resolve: GameState => GameState
}
object C1 extends ExternalWork {
  val cost: Machinery = 5.excavators
  def resolve: GameState => GameState = {
    lens.currentPlayerState.modify(lens.points.modify(_ ++ 6.vp) andThen lens.playerMachinery.modify(_ ++ 3.mixers))
  }
}

object MachineShop extends Section {
  case class Rows(excavator: Row, wild: Row, both: Row)

  def forAction: Action.MachineShop => StateT[F, Rows, ActionColumn] = {
    case Action.MachineShop.Excavator => excavator
    //case Action.MachineShop.Wild => wild
    case Action.MachineShop.Both => both
  }

  def excavator: StateT[F, Rows, ActionColumn] = reserve("Excavator", _(_.excavator))
  def wild: StateT[F, Rows, ActionColumn] = reserve("Wild", _(_.wild))
  def both: StateT[F, Rows, ActionColumn] = reserve("Both", _(_.both))

  def empty: Rows = Rows(
    excavator = Row.emptyCommonRow,
    wild = Row.emptyCommonRow,
    both = Row.emptyCommonRow,
  )
}

object Workshop extends Section {
  case class Rows(one: Row, two: Row, three: Row)

  def forAction: Action.Workshop => StateT[F, Rows, ActionColumn] = {
    case Action.Workshop.One => one
    case Action.Workshop.Two => two
    case Action.Workshop.Three => three
  }

  def one: StateT[F, Rows, ActionColumn] = reserve("One", _(_.one))
  def two: StateT[F, Rows, ActionColumn] = reserve("Two", _(_.two))
  def three: StateT[F, Rows, ActionColumn] = reserve("Three", _(_.three))

  def empty: Rows = Rows(
    one = Row.emptyCommonRow,
    two = Row.emptyCommonRow,
    three = Row.emptyCommonRow,
  )
}
