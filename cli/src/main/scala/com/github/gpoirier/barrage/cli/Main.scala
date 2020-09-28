package com.github.gpoirier.barrage.cli

import cats.effect._

import org.jline.reader._
import org.jline.reader.LineReader.Option

import org.jline.terminal.TerminalBuilder

import cats.effect.IOApp

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    App.make.flatMap(_.loop).as(ExitCode.Success)
  }
}

case class App(reader: LineReader) {

  def info(text: String): IO[Unit] = IO(println(s"[info] $text"))
  def execute: String => IO[Boolean] = {
    case "exit" => IO(false)
    case line => info(s"Executing $line...").as(true)
  }

  def readLine: IO[String] = IO(reader.readLine("barrage> "))

  def loop: IO[Unit] =
    for {
      line <- readLine
      continue <- execute(line)
      _ <- {
        if (continue) loop
        else IO.unit
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