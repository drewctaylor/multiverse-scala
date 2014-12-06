package edu.gatech.dt87.multiverse.ui

import edu.gatech.dt87.multiverse.planner.{Fabula, EventExecution, Goal, Planner}
import edu.gatech.dt87.multiverse.prettyPrinter.PrettyPrinter
import edu.gatech.dt87.multiverse.story.StateStrategyStep.SymbolMap
import edu.gatech.dt87.multiverse.story.state.State

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
        println(PrettyPrinter.print(state, 0))

        goalSet.map(goal => {
            println(PrettyPrinter.print(goal.satisfy(state, Map[Symbol, (Symbol, Int)]())))
        })

        val ret = "[" + Planner.satisfiableGoalSet(stateForId(stateId), Map[Symbol, (Symbol, Int)](), goalSet).map((goal: Goal[State, SymbolMap, SymbolMap]) => { s"""{"goalId":"${idForGoalHelper(goal)}","goalName":"${goal.label}"}"""}).mkString(",") + "]"
        ret
    }

    def satisfyGoal(stateId: String, goalId: String): String = {
        val goalExecution = goalForId(goalId).satisfy(stateForId(stateId), Map[Symbol, (Symbol, Int)]())
        val fabula = "[" + (Fabula.fabula(goalExecution): Seq[EventExecution[State, _, _]]).map((eventContext: EventExecution[State, _, _]) => { s"""{"eventName":"${eventContext.event.label}","stateId":"${idForStateHelper(eventContext.successor().get._1)}","narration":"${eventContext.successor().get._1.narration.map(_.replace('\n', ' ')).getOrElse("")}"}"""}).mkString(",") + "]"
        val ret = s"""{ "goalId" : "$goalId", "goalExecution" : ${PrettyPrinter.json(goalExecution)}, "fabula" : $fabula }"""

        println(PrettyPrinter.print(goalExecution))

        ret
    }
}
