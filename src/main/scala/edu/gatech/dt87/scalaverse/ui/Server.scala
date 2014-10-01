package edu.gatech.dt87.scalaverse.ui

import edu.gatech.dt87.scalaverse.planner.{EventContext, Goal, Planner}
import edu.gatech.dt87.scalaverse.story.StateNarration
import edu.gatech.dt87.scalaverse.planner._

case class Server(state : StateNarration, planner : Planner[StateNarration]) {
    var goalForId = Map[String, Goal[StateNarration]]()
    var stateForId = Map[String, StateNarration]()
    var idForState = Map[StateNarration, String]()
    var idForGoal = Map[Goal[StateNarration], String]()
    var id = 0

    def idForGoalHelper(goal : Goal[StateNarration]) : String = {
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
        val ret = "[" + planner.satisfiableGoalSet(stateForId(stateId)).map((goal : Goal[StateNarration]) => { s"""{"goalId":"${idForGoalHelper(goal)}","goalName":"${goal.name}"}""" }).mkString(",") + "]";
        ret
    }

    def satisfyGoal(stateId: String, goalId: String): String = {
        val goalContext = planner.satisfyGoal(stateForId(stateId), goalForId(goalId))
        System.out.println(goalContext : String)
        val ret = "[" + (goalContext : Seq[EventContext[StateNarration]]).map((eventContext : EventContext[StateNarration]) => { s"""{"eventName":"${eventContext.event.name}","stateId":"${idForStateHelper(eventContext.succeeding.get)}","narration":"${eventContext.succeeding.get.narrationMap("").text}"}""" }).mkString(",") + "]"
        ret;
    }
}
