package edu.gatech.dt87.multiverse.ui

import edu.gatech.dt87.multiverse.planner.{EventExecution, Goal, Planner}
import edu.gatech.dt87.multiverse.prettyPrinter.PrettyPrinter
import edu.gatech.dt87.multiverse.story.StoryStrategyStep.StorySymbolMap
import edu.gatech.dt87.multiverse.story.{Fabula, StoryState}

case class Server(state: StoryState, goalSet: Set[Goal[StoryState, StorySymbolMap, StorySymbolMap]]) {
    var goalForId = Map[String, Goal[StoryState, StorySymbolMap, StorySymbolMap]]()
    var stateForId = Map[String, StoryState]()
    var idForState = Map[StoryState, String]()
    var idForGoal = Map[Goal[StoryState, StorySymbolMap, StorySymbolMap], String]()
    var iterator = Iterator.from(0)

    def idForGoalHelper(goal: Goal[StoryState, StorySymbolMap, StorySymbolMap]): String = {
        if (!idForGoal.contains(goal)) {
            val id = iterator.next()
            idForGoal += goal -> id.toString
            goalForId += id.toString -> goal
        }

        idForGoal(goal)
    }

    def idForStateHelper(state: StoryState): String = {
        if (!idForState.contains(state)) {
            val id = iterator.next()
            idForState += state -> id.toString
            stateForId += id.toString -> state
        }

        idForState(state)
    }

    def initial(): String = {
        val ret = s"""{ "stateId" : "${idForStateHelper(state)}"}"""
        ret
    }

    def satisfiableGoalSet(stateId: String): String = {
        println(PrettyPrinter.print(stateForId(stateId), 0))

        val ret = "[" + Planner.satisfiableGoalSet(stateForId(stateId), Map[Symbol, (Symbol, Int)](), goalSet).map((goal: Goal[StoryState, StorySymbolMap, StorySymbolMap]) => { s"""{"goalId":"${idForGoalHelper(goal)}","goalName":"${goal.name}"}"""}).mkString(",") + "]"
        ret
    }

    def satisfyGoal(stateId: String, goalId: String): String = {
        val goalExecution = goalForId(goalId).satisfy(stateForId(stateId), Map[Symbol, (Symbol, Int)]())
        println(PrettyPrinter.print(goalExecution));

        val ret = "[" + (Fabula.fabula(goalExecution): Seq[EventExecution[StoryState, _, _]]).map((eventContext: EventExecution[StoryState, _, _]) => { s"""{"eventName":"${eventContext.event.name}","stateId":"${idForStateHelper(eventContext.successor().get._1)}","narration":"${eventContext.successor().get._1.narration getOrElse ""}"}"""}).mkString(",") + "]"
        ret.replace('\n',' ')
    }
}
