package com.github.gpoirier.barrage.cli

import cats.implicits._
import cats.data.StateT
import cats.effect._
import org.jline.reader._
import org.jline.reader.LineReader.Option
import org.jline.terminal.TerminalBuilder
import cats.effect.IOApp
import com.github.gpoirier.barrage.GameState
import com.github.gpoirier.barrage.commands.Command
import com.github.gpoirier.barrage.parsers.CommandParser

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    App.make.flatMap(_.loop.run(GameState.simple)).as(ExitCode.Success)
  }
}

case class App(reader: LineReader) {

  object parse extends CommandParser {
    def apply(text: String): Either[String, Command] =
      parseAll(command, text) match {
        case Success(cmd, _) => Right(cmd)
        case other => Left(other.toString)
      }
  }

  type F[A] = StateT[IO, GameState, A]
  val F: Sync[F] = Sync[F]

  def info(text: String): IO[Unit] = IO(println(s"[info] $text"))
  def execute: String => StateT[IO, GameState, Boolean] = {
    case "exit" => StateT.pure(false)
    case line =>
      parse(line).fold(
        failure => StateT.liftF[IO, GameState, Unit](info(s"Invalid command: $failure")).as(true),
        cmd =>
          for {
            state <- StateT.get[IO, GameState]
            _ <- StateT.setF[IO, GameState] {
              GameState.resolve(cmd).run(state).fold(
                error => info(s"Illegal State: $error").as(state),
                pair => IO.pure(pair._1)
              )
            }
          } yield true
      )
  }

  def toIO[A](result: Either[String, A]): IO[A] =
    result.leftMap(new Exception(_)).liftTo[IO]

  def readLine: IO[String] = IO(reader.readLine("barrage> "))

  def loop: StateT[IO, GameState, Unit] =
    for {
      line <- StateT.liftF[IO, GameState, String](readLine)
      continue <- execute(line)
      _ <- {
        if (continue) loop
        else F.unit
      }
    } yield ()
}

object App {
  def make: IO[App] = IO {
    val terminal = TerminalBuilder.builder().build()
    // val executeThread = Thread.currentThread()
    // terminal.handle(Signal.INT,  _ => executeThread.interrupt())

    val reader = LineReaderBuilder.builder()
      .terminal(terminal)
      // .parser(parser)
      .variable(LineReader.SECONDARY_PROMPT_PATTERN, "%M%P > ")
      .variable(LineReader.INDENTATION, 2)
      .variable(LineReader.LIST_MAX, 100)
      // .variable(LineReader.HISTORY_FILE, Paths.get(".history"))
      .option(Option.INSERT_BRACKET, true)
      .option(Option.EMPTY_WORD_OPTIONS, false)
      .option(Option.USE_FORWARD_SLASH, true)             // use forward slash in directory separator
      .option(Option.DISABLE_EVENT_EXPANSION, true)
      .build()

    App(reader)
  }
}