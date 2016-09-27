package edu.gatech.dt87.multiverse.language.compiler

import edu.gatech.dt87.multiverse.planner.{Goal, Strategy}
import edu.gatech.dt87.multiverse.story.StateStrategyStep
import edu.gatech.dt87.multiverse.story.StateStrategyStep._
import edu.gatech.dt87.multiverse.language.parser._
import edu.gatech.dt87.multiverse.story.state.State
import monocle.function._
import monocle.std._
import monocle.syntax._

import scala.collection.immutable.Iterable
import scalaz.{Failure, NonEmptyList, Success, Validation}

object Compiler {

    def validateGoalList(declarationStory: DeclarationStory): List[String] = {

        if (declarationStory.goalList.isEmpty) {

            List("The story must include at least one goal.")

        } else {

            List()
        }
    }

    def validateGoalSymbolUnique(declarationStory: DeclarationStory): List[String] = {

        declarationStory.goalList.map(goal => goal.identifier)
            .groupBy(symbol => symbol)
            .filter(tuple => tuple._2.size > 1)
            .flatMap(tuple => List(s"The goal ${tuple._1} may appear at most once."))
            .toList
    }

    def validateGoalParameterListUnique(declarationStory: DeclarationStory): List[String] = {

        declarationStory.goalList.flatMap(goal =>
            goal.parameterList.map(parameter => parameter._2)
                .groupBy(symbol => symbol)
                .filter(tuple => tuple._2.size > 1)
                .flatMap(tuple => List(s"The goal ${goal.identifier} may include at most one parameter ${tuple._1}")))
    }

    def validateSubgoalName(declarationStory: DeclarationStory): List[String] = {

        declarationStory.goalList.flatMap(goal =>
            goal.strategySet.flatMap(strategy => strategy.statementList)
                .filter(statement => {
                    statement.isInstanceOf[StatementSubgoal]
                }).map(statement => {
                statement.asInstanceOf[StatementSubgoal]
            }))
            .map(subgoal => subgoal.goal)
            .filter(identifier => !declarationStory.goalList.map(goal => goal.identifier).contains(identifier))
            .flatMap(identifier => List(s"The subgoal $identifier does not reference a goal."))
    }

    def validateSubgoalParameterList(declarationStory: DeclarationStory): List[String] = {

        declarationStory.goalList.flatMap(goal =>
            goal.strategySet.flatMap(strategy => strategy.statementList)
                .filter(statement => {
                    statement.isInstanceOf[StatementSubgoal]
                }).map(statement => {
                statement.asInstanceOf[StatementSubgoal]
            }))
            .filter(subgoal => declarationStory.goalList.map(goal => goal.identifier).contains(subgoal.goal))
            .map(subgoal => (subgoal, subgoal.parameterList.size - declarationStory.goalList.filter(goal => goal.identifier == subgoal.goal).head.parameterList.size))
            .filter(tuple => tuple._2 != 0)
            .flatMap(tuple => List(s"The subogal ${tuple._1.goal} must have ${tuple._1.parameterList.size - tuple._2} parameters, not ${tuple._1.parameterList.size}."))
    }

    def validate(declarationStory: DeclarationStory): Validation[NonEmptyList[String], DeclarationStory] = {

        val list = validateGoalList(declarationStory) :::
            validateGoalSymbolUnique(declarationStory) :::
            validateGoalParameterListUnique(declarationStory) :::
            validateSubgoalName(declarationStory) :::
            validateSubgoalParameterList(declarationStory)

        if (list.isEmpty) {

            Success(declarationStory)

        } else {

            Failure(NonEmptyList.nel(list.head, list.tail))
        }
    }

    def sortGenerator(seedOption: Option[LiteralNumber]): Set[StrategyT] => Seq[StrategyT] = {
        val random = seedOption.map(seed => {
            seed.value.rounded.toInt
        }).map(seedInt => {
            new scala.util.Random(seedInt)
        }) getOrElse new scala.util.Random()

        (set) => {
            random.shuffle(set.toSeq)
        }
    }

    def compileOther(source: String): Validation[NonEmptyList[String], (State, Set[GoalT])] = {

        Success((State(), Set[GoalT]()))
    }

    def compile(source: String): Validation[NonEmptyList[String], (State, Set[GoalT])] =

        Parser.phrase(Parser.declarationStory)(new Parser.lexical.Scanner(source)) match {

            case Parser.Success(DeclarationStory(titleOption, seedOption, stateDeclarationOption, goalDeclarationList), next) =>

                validate(DeclarationStory(titleOption, seedOption, stateDeclarationOption, goalDeclarationList)).map[(State, Set[GoalT])](declarationStory => {

                    val sort = sortGenerator(seedOption)

                    val strategyStepListOption = stateDeclarationOption.map(stateDeclaration => {
                        stateDeclaration.assignmentList.map(assignment => {
                            StateStrategyStep.bind(assignment)
                        }) ++ stateDeclaration.assignmentList.map(assignment => {
                            StateStrategyStep.assign(assignment)
                        })
                    })

                    val stateStrategyStepOption = strategyStepListOption.map(stateEventList => {
                        stateEventList.reduce((a: StrategyStepT, b: StrategyStepT) => {
                            a.merge(b)
                        })
                    })

                    val stateStrategyOption = stateStrategyStepOption.map(stateStrategyStep => {
                        Strategy(stateStrategyStep)
                    })

                    val stateGoalOption = stateStrategyOption.map(stateStrategy => {
                        Goal(sort, Seq(stateStrategy))
                    })

                    val state = if (titleOption.isDefined) {
                        State(Some(titleOption.get.value))
                    } else {
                        State()
                    }

                    val stateGoalExecutionOption = stateGoalOption.map(stateGoal => {
                        stateGoal.satisfy(state, Map[Symbol, (Symbol, Int)]())
                    })

                    val stateOption = stateGoalExecutionOption.flatMap(stateGoalExecution => {
                        stateGoalExecution.successor()
                    })

                    lazy val goalFor: Symbol => (Goal[State, SymbolMap, SymbolMap], DeclarationGoal) = (symbol) => {
                        goalList.filter(tuple => tuple._2.identifier == symbol).head
                    }

                    lazy val goalList = goalDeclarationList.map(goalDeclaration => {
                        if (goalDeclaration.label.isDefined) {
                            (Goal(goalDeclaration.label.get.value, sort, goalDeclaration.strategySet.map(strategyDeclaration => {
                                val strategyStepList = strategyDeclaration.statementList.filter(statement => {
                                    statement.isInstanceOf[StatementAssignmentQualified]
                                }).map(assignment => {
                                    StateStrategyStep.bind(assignment.asInstanceOf[StatementAssignmentQualified])
                                }) ++ strategyDeclaration.statementList.map({
                                    case s: StatementAssignmentQualified =>
                                        StateStrategyStep.assign(s)

                                    case s: StatementNarration =>
                                        StateStrategyStep.narrate(s)

                                    case s: StatementSubgoal =>
                                        StateStrategyStep.subgoal(s, goalFor)

                                    case s: StatementQuery =>
                                        StateStrategyStep.query(s)
                                    //
                                    //                                    case s: StatementAssignmentUnqualified =>
                                    //                                        println("An unqualified assignment statement may not appear here.")
                                    //                                        None
                                })

                                val strategyStep = strategyStepList.reduce((a: StrategyStepT, b: StrategyStepT) => {
                                    a.merge(b)
                                })

                                strategyDeclaration.label match {
                                    case Some(LiteralString(label)) => Strategy(label, strategyStep)
                                    case _ => Strategy(strategyStep)
                                }
                            }).toSeq), goalDeclaration)
                        } else {
                            (Goal(sort, goalDeclaration.strategySet.map(strategyDeclaration => {
                                val strategyStepList = strategyDeclaration.statementList.filter(statement => {
                                    statement.isInstanceOf[StatementAssignmentQualified]
                                }).map(assignment => {
                                    StateStrategyStep.bind(assignment.asInstanceOf[StatementAssignmentQualified])
                                }) ++ strategyDeclaration.statementList.map({
                                    case s: StatementAssignmentQualified => StateStrategyStep.assign(s)
                                    case s: StatementNarration => StateStrategyStep.narrate(s)
                                    case s: StatementSubgoal => StateStrategyStep.subgoal(s, goalFor)
                                    case s: StatementQuery => StateStrategyStep.query(s)
                                })

                                val strategyStep = strategyStepList.reduce((a: StrategyStepT, b: StrategyStepT) => {
                                    a.merge(b)
                                })

                                strategyDeclaration.label match {
                                    case Some(LiteralString(label)) => Strategy(label, strategyStep)
                                    case _ => Strategy(strategyStep)
                                }
                            }).toSeq), goalDeclaration)

                        }
                    })

                    if (stateOption.isDefined) {
                        (stateOption.get._1, goalList.filter(_._2.parameterList.isEmpty).map(_._1).toSet)
                    } else {
                        (State(), goalList.filter(_._2.parameterList.isEmpty).map(_._1).toSet)
                    }

                })

            case Parser.NoSuccess(message, next) =>

                Failure(NonEmptyList.nels(message))

        }

}