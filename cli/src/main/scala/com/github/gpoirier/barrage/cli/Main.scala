package com.github.gpoirier.barrage.cli

import cats.implicits._
import cats.data._
import cats.effect._
import cats.mtl._
import org.jline.reader._
import org.jline.reader.LineReader.Option
import org.jline.terminal.TerminalBuilder
import com.github.gpoirier.barrage.GameState
import com.github.gpoirier.barrage.commands._
import com.github.gpoirier.barrage.parsers.CommandParser

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    type F[A] = StateT[IO, GameState, A]
    val resolver = CommandResolver.default(Console.forSync[F])
    App.make[F](resolver).loop.runA(GameState.simple).as(ExitCode.Success)
  }
}

trait Console[F[_]] {
  def print(text: String): F[Unit]
}

object Console {
  implicit def forSync[F[_]: Sync]: Console[F] = text => Sync[F].delay(println(text))
}

trait CommandResolver[F[_]] {
  def resolve(command: Command): F[Unit]
}

object CommandResolver {
  type F[A] = StateT[IO, GameState, A]
  def default(console: Console[F]): CommandResolver[F] = { cmd =>
    for {
      state <- StateT.get[IO, GameState]
      _ <- GameState.resolve(cmd).transformF {
        case Left(error) => console.print(error).run(state)
        case Right((newState, ())) => IO(newState -> {})
      }
    } yield ()
  }
}

case class App[F[_]](reader: LineReader, resolver: CommandResolver[F])
                    (implicit F: Sync[F], S: Stateful[F, GameState], console: Console[F]) {

  object parse extends CommandParser {
    def apply(text: String): Either[String, Command] =
      parseAll(command, text) match {
        case Success(cmd, _) => Right(cmd)
        case other => Left(other.toString)
      }
  }

  def info(text: String): F[Unit] = console.print(s"[info] $text")

  def execute: String => F[Boolean] = {
    case "exit" => F.pure(false)
    case line =>
      parse(line).fold(
        failure => info(s"Invalid command: $failure").as(true),
        cmd => resolver.resolve(cmd).as(true)
      )
  }

  def readPrompt(prompt: String): F[String] =
    F.delay(reader.readLine(prompt))

  def readLine: F[String] =
    for {
      state <- S.get
      line <- readPrompt(s"${state.currentPlayerState.summary}\n${state.currentPlayer}> ")
    } yield line

  def loop: F[Unit] =
    for {
      line <- readLine
      continue <- execute(line)
      _ <- {
        if (continue) loop
        else F.unit
      }
    } yield ()
}

object App {
  def make[F[_]: Sync: Stateful[*[_], GameState]](resolver: CommandResolver[F]): App[F] = {
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

    App(reader, resolver)
  }
}