package com.github.gpoirier.barrage
package parsers

import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import cats.data._
import actions._
import commands._
import resources.literals._

class CommandParserSpec extends AnyFlatSpec with Matchers with Inside {
  behavior of "parser"

  object TestParser extends CommandParser
  import TestParser._

  def parse(text: String): ParseResult[Command] = parseAll(command, text)

  it should "parse command with reward" in {
    inside(parse("wild->1g")) {
      case Success(Command(action, selectors), _) =>
        action shouldBe Action.MachineShop.Wild
        selectors shouldBe Chain(RewardSelector.WildMachinery(1.mixer))
    }
    inside(parse("wild -> 1b")) {
      case Success(Command(action, selectors), _) =>
        action shouldBe Action.MachineShop.Wild
        selectors shouldBe Chain(RewardSelector.WildMachinery(1.excavator))
    }
  }

  it should "parse command without reward" in {
    inside(parse("both")) {
      case Success(Command(action, Chain.nil), _) =>
        action shouldBe Action.MachineShop.Both
    }
  }
}