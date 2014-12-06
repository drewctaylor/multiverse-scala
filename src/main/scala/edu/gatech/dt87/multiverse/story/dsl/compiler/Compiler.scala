package edu.gatech.dt87.multiverse.story.dsl.compiler

import edu.gatech.dt87.multiverse.planner.{Goal, Strategy}
import edu.gatech.dt87.multiverse.story.StateStrategyStep
import edu.gatech.dt87.multiverse.story.StateStrategyStep._
import edu.gatech.dt87.multiverse.story.dsl.parser._
import edu.gatech.dt87.multiverse.story.state.State
import monocle.function._
import monocle.std._
import monocle.syntax._

object Compiler {

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

    def static(declarationStory: DeclarationStory): Option[DeclarationStory] = {
        val goalList = declarationStory.goalList
        val goalIdentifierList = goalList.map(goal => {
            goal.identifier
        })
        val goalParameterListSizeMap = goalList.map(goal => {
            goal.identifier -> goal.parameterList.size
        }).toMap

        val subgoalList = goalList.map(goal => {
            goal.strategySet.map(strategy => {
                strategy.statementList.filter(statement => {
                    statement.isInstanceOf[StatementSubgoal]
                }).map(statement => {
                    statement.asInstanceOf[StatementSubgoal]
                })
            }).flatten
        }).flatten

        val subgoalArgumentListSizeMap = subgoalList.map(subgoal => {
            subgoal.goal -> subgoal.parameterList.size
        }).toMap

        val subgoalMustReferenceGoal = subgoalList.foldLeft(List[String]())((messageList, subgoal) => {
            if (goalIdentifierList.contains(subgoal.goal)) {
                messageList
            } else {
                messageList :+ s"""The identifier "${subgoal.goal.name}" does not reference a goal."""
            }
        })

        val subgoalArgumentListSizeMustMatchGoalParameterListSize = subgoalList.foldLeft(List[String]())((messageList, subgoal) => {
            goalParameterListSizeMap.get(subgoal.goal) match {
                case Some(i) => {
                    if (i == subgoalArgumentListSizeMap(subgoal.goal)) {
                        messageList
                    } else {
                        messageList :+ s"""The length of the argument list does not match the length of the parameter list of the goal "${subgoal.goal.name}"; it is ${subgoalArgumentListSizeMap(subgoal.goal)}, but it should be $i."""
                    }
                }
                case None => messageList
            }
        })

        val messageList = subgoalMustReferenceGoal ++ subgoalArgumentListSizeMustMatchGoalParameterListSize

        if (messageList.size == 0) {

            Some(declarationStory)

        } else {

            messageList.map(message => {
                println(message)
            })

            None
        }
    }

    def compile(source: String): Option[(State, Set[GoalT])] = {
        Parser.phrase(Parser.unit)(new Parser.lexical.Scanner(source)) match {

            case Parser.Success(DeclarationStory(titleOption, seedOption, stateDeclarationOption, goalDeclarationList), next) =>

                if (static(DeclarationStory(titleOption, seedOption, stateDeclarationOption, goalDeclarationList)).isDefined) {

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

                    val state = if(titleOption.isDefined) {
                        State(Some(titleOption.get.value))
                    } else {
                        State()
                    }

                    val stateGoalExecutionOption = stateGoalOption.map(stateGoal => {
                        stateGoal.satisfy(state, Map[Symbol, (Symbol, Int)]())
                    })

                    val stateOption = stateGoalExecutionOption.map(stateGoalExecution => {
                        stateGoalExecution.successor()
                    }).flatten

                    lazy val goalFor: Symbol => (Goal[State, SymbolMap, SymbolMap], DeclarationGoal) = (symbol) => {
                        goalList.filter(tuple => tuple._2.identifier == symbol).head
                    }

                    lazy val goalList = goalDeclarationList.map(goalDeclaration => {
                        if(goalDeclaration.label.isDefined) {
                            (Goal(goalDeclaration.label.get.value, sort, goalDeclaration.strategySet.map(strategyDeclaration => {
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

                    if(stateOption.isDefined) {
                        Some(stateOption.get._1, goalList.filter(_._2.parameterList.size == 0).map(_._1).toSet)
                    } else {
                        Some(State(), goalList.filter(_._2.parameterList.size == 0).map(_._1).toSet)
                    }

                } else {
                    None
                }

            case Parser.NoSuccess(message, next) =>
                println(message, next.pos)
                None

        }
    }

}