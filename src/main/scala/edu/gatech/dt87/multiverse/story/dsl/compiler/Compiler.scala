package edu.gatech.dt87.multiverse.story.dsl.compiler

import edu.gatech.dt87.multiverse.planner.{Strategy, StrategyStep, Event, Goal}
import edu.gatech.dt87.multiverse.story.StateStrategyStep._
import edu.gatech.dt87.multiverse.story.dsl.parser._
import edu.gatech.dt87.multiverse.story.{StateStrategyStep, State}

import scala.collection.mutable

object Compiler {
    def compile(source: String): Option[(State, Set[Goal[State, StateStrategyStep.SymbolMap, StateStrategyStep.SymbolMap]])] = {
        val parser = new Parser()

        parser.phrase(parser.declarationStory)(new parser.lexical.Scanner(source)) match {

            case parser.Success(DeclarationStory(title, seed, s, gl), next) =>

                val eventList = s.block.declarationList.foldLeft(List[Event[State, StateStrategyStep.SymbolMap, StateStrategyStep.SymbolMap]]())((el, d) => d match {
                    case DeclarationEntity(k, o, ba) =>
                        el ++ List(StateStrategyStep.eventDeclare(DeclarationEntity(k, o, ba))) ++ ba.assignmentList.map({
                            case StatementAssignment(ExpressionIdentifierSequence(None, i), right, op) =>
                                StateStrategyStep.eventAssignment(StatementAssignment(ExpressionIdentifierSequence(Some(o), i), right, op))
                            case sa =>
                                StateStrategyStep.eventAssignment(sa)
                        })
                    case DeclarationRelationship(o, ba) =>
                        el ++ ba.assignmentList.map({
                            case StatementAssignment(ExpressionIdentifierSequence(None, i), right, op) =>
                                StateStrategyStep.eventAssignment(StatementAssignment(ExpressionIdentifierSequence(Some(o), i), right, op))
                            case sa =>
                                StateStrategyStep.eventAssignment(sa)
                        })
                })

                val step = eventList.reduce((a: StrategyStep[State, StateStrategyStep.SymbolMap, StateStrategyStep.SymbolMap], b: StrategyStep[State, StateStrategyStep.SymbolMap, StateStrategyStep.SymbolMap]) => a merge b)
                val strategy: Strategy[State, SymbolMap, SymbolMap] = Strategy(step)
                val goalExecution = Goal(strategy).satisfy(State(title.map(_.value), seed.map(_.value.rounded.toInt)), Map())
                val storyState = goalExecution.successor().get._1

                var map = mutable.Map[Symbol, Goal[State, StateStrategyStep.SymbolMap, StateStrategyStep.SymbolMap]]()
                val goalForGoalSymbol = (gs: Symbol) => map(gs)
                val declarationFor = (gs: Symbol) => gl.filter(_.identifier.identifier == gs).head
                val goalForGoalName = (gn: String) => gl.filter((g) => (g.label.map(_.value) getOrElse g.identifier.identifier.name) == gn).head
                val goalList = gl.map(g => Goal(g.label.map(_.value) getOrElse g.identifier.identifier.name, g.block.strategyList.map(s => {
                    val eventList = s.block.statementList.map({
                        case s: StatementQuery => StateStrategyStep.eventQuery(s)
                        case s: StatementSubgoal => StateStrategyStep.eventSubgoal(s, goalForGoalSymbol, declarationFor)
                        case s: StatementNarration => StateStrategyStep.eventNarration(s)
                        case s: StatementAssignment => StateStrategyStep.eventAssignment(s)
                        case StatementDeclarationData(DeclarationEntity(k, o, ba)) =>
                            (List(StateStrategyStep.eventDeclare(DeclarationEntity(k, o, ba))) ++ ba.assignmentList.map({
                                case StatementAssignment(ExpressionIdentifierSequence(None, i), right, op) =>
                                    StateStrategyStep.eventAssignment(StatementAssignment(ExpressionIdentifierSequence(Some(o), i), right, op))
                                case sa =>
                                    StateStrategyStep.eventAssignment(sa)
                            })).reduce((a: StrategyStep[State, StateStrategyStep.SymbolMap, StateStrategyStep.SymbolMap], b: StrategyStep[State, StateStrategyStep.SymbolMap, StateStrategyStep.SymbolMap]) => {
                                a merge b
                            })
                        case StatementDeclarationData(DeclarationRelationship(o, ba)) => ba.assignmentList.map({
                            case StatementAssignment(ExpressionIdentifierSequence(None, i), right, op) =>
                                StateStrategyStep.eventAssignment(StatementAssignment(ExpressionIdentifierSequence(Some(o), i), right, op))
                            case sa =>
                                StateStrategyStep.eventAssignment(sa)
                        }).reduce((a: StrategyStep[State, StateStrategyStep.SymbolMap, StateStrategyStep.SymbolMap], b: StrategyStep[State, StateStrategyStep.SymbolMap, StateStrategyStep.SymbolMap]) => {
                            a merge b
                        })
                    }).reduce((a: StrategyStep[State, StateStrategyStep.SymbolMap, StateStrategyStep.SymbolMap], b: StrategyStep[State, StateStrategyStep.SymbolMap, StateStrategyStep.SymbolMap]) => {
                        a merge b
                    })

                    if (s.label.isDefined)
                        Strategy(s.label.get.value, eventList)
                    else
                        Strategy(eventList)
                }).toSeq: _*))

                goalList.map((g) => goalForGoalName(g.label).identifier.identifier -> g).map(map += _)

                val top = goalList.filter((g) => declarationFor(goalForGoalName(g.label).identifier.identifier).parameterList.isEmpty)

                Some(storyState, top.toSet)

            case o =>
                println(o)
                None

        }
    }
}