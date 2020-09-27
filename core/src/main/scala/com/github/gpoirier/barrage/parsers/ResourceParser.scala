package com.github.gpoirier.barrage
package parsers

import scala.util.parsing.combinator.RegexParsers

import resources._
import literals._

trait ResourceParser extends RegexParsers {

  private def digit: Parser[Char] = elem("digit", Character.isDigit)
  private def num: Parser[Int] = digit.+ ^^ { _.mkString.toInt}

  object resource {
    def excavators: Parser[Excavators] = num <~ "b" ^^ (_.excavators)
    def mixers: Parser[Mixers] = num <~ "g" ^^ (_.mixers)
    def both: Parser[Machinery] = excavators ~ mixers ^^ {
      case e ~ m => e & m
    }
    def machinery: Parser[Machinery] = both ||| excavators.map(_.toMachinery) ||| mixers.map(_.toMachinery)
  }
}