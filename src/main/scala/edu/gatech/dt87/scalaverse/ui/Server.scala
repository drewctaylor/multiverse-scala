package edu.gatech.dt87.scalaverse.ui

import edu.gatech.dt87.scalaverse.planner.context.EventContext
import edu.gatech.dt87.scalaverse.planner.{Goal, Planner}
import edu.gatech.dt87.scalaverse.prettyPrinter.PrettyPrinter
import edu.gatech.dt87.scalaverse.story.{Fabula, StateNarration}
import edu.gatech.dt87.scalaverse.planner._

case class Server(state : StateNarration, goalSet : Set[Goal[StateNarration, Unit]]) {
    var goalForId = Map[String, Goal[StateNarration, Unit]]()
    var stateForId = Map[String, StateNarration]()
    var idForState = Map[StateNarration, String]()
    var idForGoal = Map[Goal[StateNarration, Unit], String]()
    var id = 0

    def idForGoalHelper(goal : Goal[StateNarration, Unit]) : String = {
        if(!idForGoal.contains(goal)) {
            id = id + 1;
            idForGoal += goal -> id.toString
            goalForId += id.toString -> goal
        }

        idForGoal(goal)
    }

    def idForStateHelper(state : StateNarration) : String = {
        if(!idForState.contains(state)) {
            id = id + 1;
            idForState += state -> id.toString
            stateForId += id.toString -> state
        }

        idForState(state)
    }

    def initial() : String = {
        val ret =s"""{ "stateId" : "${idForStateHelper(state)}", "narration": "${state.narrationMap("").text}"}"""
        ret;
    }

    def satisfiableGoalSet(stateId : String) : String = {
        val ret = "[" + Planner.satisfiableGoalSet(state, (), goalSet).map((goal : Goal[StateNarration, Unit]) => { s"""{"goalId":"${idForGoalHelper(goal)}","goalName":"${goal.name}"}""" }).mkString(",") + "]";
        ret
    }

    def satisfyGoal(stateId: String, goalId: String): String = {
        val goalContext = Planner.satisfyGoal[StateNarration, Unit](stateForId(stateId), (), goalForId(goalId))
        System.out.println(PrettyPrinter.print(goalContext));
        val ret = "[" + (Fabula.fabula(goalContext) : Seq[EventContext[StateNarration, _]]).map((eventContext : EventContext[StateNarration, _]) => { s"""{"eventName":"${eventContext.event.name}","stateId":"${idForStateHelper(eventContext.succeeding.get)}","narration":"${eventContext.succeeding.get.narrationMap("").text}"}""" }).mkString(",") + "]"
        ret;
    }
}
