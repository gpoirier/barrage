package com.github.gpoirier.barrage.parsers

import com.github.gpoirier.barrage.actions._
import com.github.gpoirier.barrage._

import scala.util.parsing.combinator.RegexParsers

trait ActionParser extends RegexParsers with ResourceParser {

  import Action._

  object action {
    def us: Parser[USA.type] = "us" ^^ { _ => USA }
    def nl: Parser[Netherlands.type] = "nl" ^^ { _ => Netherlands }
    def fr: Parser[France.type] = "fr" ^^ { _ => France }
    def de: Parser[Germany.type] = "de" ^^ { _ => Germany }
    def it: Parser[Italy.type] = "it" ^^ { _ => Italy }

    def company: Parser[Company] = us | nl | fr | de | it

    def workshop: Parser[Workshop] = "workshop".? ~> ("1" | "2" | "3") <~ "spin" ^^ {
      case "1" => Workshop.One
      case "2" => Workshop.Two
      case "3" => Workshop.Three
    }

    object machineShop {
      import MachineShop._
      def excavator: Parser[Excavator.type] = ("excavator" | "1b") ^^^ Excavator
      def wild: Parser[Wild.type] = ("wild" | "1g") ^^^ Wild
      def both: Parser[Both.type] = ("both" | "1b1g") ^^^ Both

      def apply(): Parser[MachineShop] = "machineShop".? ~> (excavator ||| wild ||| both)
    }

    def apply(): Parser[Action] = workshop | machineShop()
  }
}