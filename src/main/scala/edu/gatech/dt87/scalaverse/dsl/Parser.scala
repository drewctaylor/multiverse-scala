package edu.gatech.dt87.scalaverse.dsl
import scala.util.parsing.combinator.RegexParsers


class SParser extends RegexParsers {

    def event="event"~identifier
    def action="action"~identifier~rep(event)
    def goal= "goal"~identifier~description~rep(action)

    def description = """"[^"]*"""".r
    def identifier  = """[_\p{L}][_\p{L}\p{Nd}]*""".r
    def integer     = """(0|[1-9]\d*)""".r ^^ { _.toInt }
    def loop =
        "for"~identifier~"in"~integer~"to"~integer~statement ^^
            { case f~variable~i~lBound~t~uBound~statement => ForLoop(variable, lBound, uBound,statement) }
    def statements = statement*
    def block = "{"~>statements<~"}"  ^^ { l => Block(l) }
    def statement : Parser[Statement] = loop | block
}

abstract trait Statement
case class Block(statements : List[Statement]) extends Statement
case class ForLoop(variable: String, lowerBound:Int, upperBound: Int, statement:Statement) extends Statement

object Parser extends SParser with App {
    parseAll(goal, "goal goalname     \"This is a test\" action action1 action action2 event event1 event event2") match {
        case Success(lup, _) => println(lup)
        case x => println(x)
    }
}