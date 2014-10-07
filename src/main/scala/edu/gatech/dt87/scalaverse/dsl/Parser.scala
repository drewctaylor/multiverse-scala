package edu.gatech.dt87.scalaverse.dsl

import scala.util.parsing.combinator.RegexParsers

sealed trait GoalSpecification

case class GoalIdentifiedSpecification(identifier: String, name: Option[String], strategySpecificationList: List[StrategySpecification]) extends GoalSpecification

case class GoalUnidentifiedSpecification(name: Option[String], strategySpecificationList: List[StrategySpecification]) extends GoalSpecification

sealed trait StrategySpecification

case class StrategyIdentifiedSpecification(identifier: String, name: Option[String], eventSpecificationList: List[EventSpecification]) extends StrategySpecification

case class StrategyUnidentifiedSpecification(name: Option[String], eventSpecificationList: List[EventSpecification]) extends StrategySpecification

sealed trait EventSpecification

case class EventIdentifiedSpecification(identifier: String, name: Option[String]) extends EventSpecification

case class EventUnidentifiedSpecification(name: Option[String]) extends EventSpecification

class DslParser extends RegexParsers {
    def identifier = """\p{Alnum}+""".r
    def nameDoubleQuote = """"[\p{Alnum}\p{Punct} &&[^"]]*"""".r
    def end = "."

    def goalKeyword = "goal" | "g"
    def strategyKeyword = "strategy" | "s"
    def eventKeyword = "event" | "e"

    def goalSpecification = goalUnidentifiedSpecification | goalIdentifiedSpecification

    def goalIdentifiedSpecification = goalKeyword ~ identifier ~ opt(nameDoubleQuote) ~ end ~ rep(strategySpecification) ^^ {
        case keyword ~ identifier ~ name ~ end ~ strategySpecificationList => GoalIdentifiedSpecification(identifier, name, strategySpecificationList)
    }

    def goalUnidentifiedSpecification = goalKeyword ~ opt(nameDoubleQuote) ~ end ~ rep1(strategySpecification) ^^ {
        case keyword ~ name ~ end ~ strategySpecificationList => GoalUnidentifiedSpecification(name, strategySpecificationList)
    }

    def strategySpecification = strategyUnidentifiedSpecification | strategyIdentifiedSpecification

    def strategyUnidentifiedSpecification = strategyKeyword ~ opt(nameDoubleQuote) ~ end ~ rep1(eventSpecification) ^^ {
        case keyword ~ name ~ end ~ eventSpecificationList => StrategyUnidentifiedSpecification(name, eventSpecificationList)
    }

    def strategyIdentifiedSpecification = strategyKeyword ~ identifier ~ opt(nameDoubleQuote) ~ end ~ rep(eventSpecification) ^^ {
        case keyword ~ identifier ~ name ~ end ~ eventSpecificationList => StrategyIdentifiedSpecification(identifier, name, eventSpecificationList)
    }

    def eventSpecification = eventUnidentifiedSpecification | eventIdentifiedSpecification

    def eventUnidentifiedSpecification = eventKeyword ~ opt(nameDoubleQuote) ~ end ^^ {
        case keyword ~ name ~ end => EventUnidentifiedSpecification(name)
    }

    def eventIdentifiedSpecification = eventKeyword ~ identifier ~ opt(nameDoubleQuote) ~ end ^^ {
        case keyword ~ identifier ~ name ~ end => EventIdentifiedSpecification(identifier, name)
    }

    def statement = goalSpecification | strategyIdentifiedSpecification | eventIdentifiedSpecification

    def statementSet = rep(statement)
}

object Parser extends DslParser with App {
    parseAll(statementSet, """goal goalname "This, thi is a test". s test "this is a strategy". e test.""") match {
        case Success(lup, _) => println(lup)
        case x => println(x)
    }
}

