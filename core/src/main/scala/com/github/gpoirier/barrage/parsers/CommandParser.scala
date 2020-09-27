package com.github.gpoirier.barrage
package parsers

import scala.util.parsing.combinator.RegexParsers

import commands._

trait CommandParser extends RegexParsers with ActionParser with ResourceParser {
  def rewards: Parser[RewardSelector] = resource.machinery ^^ { RewardSelector.WildMachinery }

  def command: Parser[Command] = action() ~ ("->\\s*".r ~> rewards.+).? ^^ {
    case action ~ handlers => Command(action, handlers.getOrElse(Nil))
  }
}