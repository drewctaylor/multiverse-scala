package edu.gatech.dt87.multiverse.ui

import edu.gatech.dt87.multiverse.planner._
import edu.gatech.dt87.multiverse.prettyPrinter.PrettyPrinter
import edu.gatech.dt87.multiverse.story.StateStrategyStep.SymbolMap
import edu.gatech.dt87.multiverse.story.state.State
import monocle.syntax._

case class Server(state: State, goalSet: Set[Goal[State, SymbolMap, SymbolMap]]) {
    var goalForId = Map[String, Goal[State, SymbolMap, SymbolMap]]()
    var stateForId = Map[String, State]()
    var idForState = Map[State, String]()
    var idForGoal = Map[Goal[State, SymbolMap, SymbolMap], String]()
    var iterator = Iterator.from(0)

    def idForGoalHelper(goal: Goal[State, SymbolMap, SymbolMap]): String = {
        if (!idForGoal.contains(goal)) {
            val id = iterator.next()
            idForGoal += goal -> id.toString
            goalForId += id.toString -> goal
        }

        idForGoal(goal)
    }

    def idForStateHelper(state: State): String = {
        if (!idForState.contains(state)) {
            val id = iterator.next()
            idForState += state -> id.toString
            stateForId += id.toString -> state
        }

        idForState(state)
    }

    def initial(): String = {
        val ret = s"""{ "title" : "${state.title.getOrElse("Untitled")}", "stateId" : "${idForStateHelper(state)}", "state" : ${PrettyPrinter.json(state)} }"""
        ret
    }

    def satisfiableGoalSet(stateId: String): String = {
        println(PrettyPrinter.print(stateForId(stateId), 0))

        goalSet.map(goal => {
            println(PrettyPrinter.print(goal.satisfy(stateForId(stateId), Map[Symbol, (Symbol, Int)]())))
        })

        val ret = "[" + Planner.satisfiableGoalSet(stateForId(stateId), Map[Symbol, (Symbol, Int)](), goalSet).map((goal: Goal[State, SymbolMap, SymbolMap]) => { s"""{"goalId":"${idForGoalHelper(goal)}","goalName":"${goal.label}"}"""}).mkString(",") + "]"
        ret
    }

    def satisfyGoal(stateId: String, goalId: String): String = {
        val state = stateForId(stateId).applyLens(State.focusNarration).set(None)

// temporary fix until we can find a better way to ensure that the goal we know can be satisfied is satisfied.
//        val goalExecution = goalForId(goalId).satisfy(state, Map[Symbol, (Symbol, Int)]())
//        val eventExecutionSequence = Fabula.fabula(goalExecution)

        var stateNext: Option[(State, Any)] = None
        var goalExecution: GoalExecution[State, SymbolMap, SymbolMap] = null
        var eventExecutionSequence: Seq[EventExecution[State, _, _]] = null

        while (!stateNext.isDefined) {
            goalExecution = goalForId(goalId).satisfy(state, Map[Symbol, (Symbol, Int)]())
            eventExecutionSequence = Fabula.fabula(goalExecution)
            stateNext = eventExecutionSequence.headOption.map(_.successor()).flatten
        }

        val fabula = "[" + eventExecutionSequence.map(eventContext => {
            eventContext.successor().map(successor => {
                s"""{ "eventName" : "${eventContext.event.label}" , "stateId" : "${idForStateHelper(successor._1)}" , "narration" : "${
                    successor._1.narration.map(text => {
                        text.replace('\n', ' ')
                    }).getOrElse("")
                }"}"""
            }).getOrElse("")
        }).mkString(",") + "]"

        val ret = s"""{ "goalId" : "$goalId", "goalExecution" : ${PrettyPrinter.json(goalExecution)}, "fabula" : $fabula }"""

        println(PrettyPrinter.print(goalExecution))

        ret
    }
}
