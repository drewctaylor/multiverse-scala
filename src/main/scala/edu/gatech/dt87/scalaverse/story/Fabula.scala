package edu.gatech.dt87.scalaverse.story

import edu.gatech.dt87.scalaverse.planner._
import edu.gatech.dt87.scalaverse.planner.context.{SubgoalContext, EventContext, StrategyContext, GoalContext}

/**
 * Created by drewtaylor on 10/1/14.
 */
object Fabula {

    def fabula[S, T](goalContext : GoalContext[S, T]) : Seq[EventContext[S, _]] = {
        goalContext.strategyContextSequence.headOption match {
            case Some(actionContext) => fabula(actionContext)
            case None => Seq()
        }
    }

    def fabula[S, T1, T2](actionContext : StrategyContext[S, T1]) : Seq[EventContext[S, _]] = {
        actionContext.strategyStepContexts.flatMap {
            case eventContext: EventContext[S, T1] => Seq(eventContext)
            case subgoalContext: SubgoalContext[S, T1, T2] => fabula(subgoalContext.goalContext)
        }
    }
}
