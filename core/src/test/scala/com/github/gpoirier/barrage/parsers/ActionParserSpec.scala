package com.github.gpoirier.barrage
package parsers

import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import actions._

class ActionParserSpec extends AnyFlatSpec with Matchers with Inside {
  behavior of "parser"

  object TestParser extends ActionParser
  import TestParser._

  def parse(text: String): ParseResult[Action] = parseAll(action(), text)

  it should "parse a workshop action" in {
    inside(parse("workshop 1 spin")) {
      case Success(action, _) => action shouldBe Action.Workshop.One
    }
    inside(parse("workshop 2 spin")) {
      case Success(action, _) => action shouldBe Action.Workshop.Two
    }
    inside(parse("2 spin")) {
      case Success(action, _) => action shouldBe Action.Workshop.Two
    }
    inside(parse("3 spin")) {
      case Success(action, _) => action shouldBe Action.Workshop.Three
    }
  }

  it should "parse a machine shop action" in {
    inside(parse("machineShop excavator")) {
      case Success(action, _) => action shouldBe Action.MachineShop.Excavator
    }
    inside(parse("wild")) {
      case Success(action, _) => action shouldBe Action.MachineShop.Wild
    }
    inside(parse("machineShop 1b1g")) {
      case Success(action, _) => action shouldBe Action.MachineShop.Both
    }
  }
}