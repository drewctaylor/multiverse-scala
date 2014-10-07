package edu.gatech.dt87.scalaverse.ui

import edu.gatech.dt87.scalaverse.planner._
import edu.gatech.dt87.scalaverse.prettyPrinter._
import edu.gatech.dt87.scalaverse.story._
import edu.gatech.dt87.scalaverse.planner._

case class Server(state : StateNarration, goalSet : Set[Goal[StateNarration, Unit, Unit]]) {
    var goalForId = Map[String, Goal[StateNarration, Unit, Unit]]()
    var stateForId = Map[String, StateNarration]()
    var idForState = Map[StateNarration, String]()
    var idForGoal = Map[Goal[StateNarration, Unit, Unit], String]()
    var id = 0

    def idForGoalHelper(goal : Goal[StateNarration, Unit, Unit]) : String = {
        if(!idForGoal.contains(goal)) {
            id = id + 1
            idForGoal += goal -> id.toString
            goalForId += id.toString -> goal
        }

        idForGoal(goal)
    }

    def idForStateHelper(state : StateNarration) : String = {
        if(!idForState.contains(state)) {
            id = id + 1
            idForState += state -> id.toString
            stateForId += id.toString -> state
        }

        idForState(state)
    }

    def initial() : String = {
        val ret =s"""{ "stateId" : "${idForStateHelper(state)}", "narration": "${state.narrationMap("").text}"}"""
        ret
    }

    def satisfiableGoalSet(stateId : String) : String = {
        val ret = "[" + Planner.satisfiableGoalSet(state, (), goalSet).map((goal : Goal[StateNarration, Unit, Unit]) => { s"""{"goalId":"${idForGoalHelper(goal)}","goalName":"${goal.name}"}""" }).mkString(",") + "]"
        ret
    }

    def satisfyGoal(stateId: String, goalId: String): String = {
        val goalExecution = goalForId(goalId).satisfy(stateForId(stateId), ())
//        System.out.println(PrettyPrinter.print(goalExecution));
        val ret = "[" + (Fabula.fabula(goalExecution) : Seq[EventExecution[StateNarration, _, _]]).map((eventContext : EventExecution[StateNarration, _, _]) => { s"""{"eventName":"${eventContext.event.name}","stateId":"${idForStateHelper(eventContext.successor.get._1)}","narration":"${eventContext.successor.get._1.narrationMap("").text}"}""" }).mkString(",") + "]"
        ret
    }
}
