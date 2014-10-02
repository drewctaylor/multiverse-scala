package edu.gatech.dt87.scalaverse.prettyPrinter

import edu.gatech.dt87.scalaverse.planner.context._

object PrettyPrinter {
    def print[S, T](goalContext : GoalContext[S, T], depth : Int = 0) : String = {
        s"""${" " * depth}Goal "${goalContext.goal.name}" ${goalContext.parameter} (${if(goalContext.succeeding.isDefined) "Success" else "Failure"})\n""" + goalContext.strategyContextSequence.foldLeft("")((string, actionContext) => {
            string + print(actionContext, depth + 4)
        })
    }

    def print[S, T1, T2](strategyContext : StrategyContext[S, T1], depth : Int) : String = {
        s"""${" " * depth}Strategy "${strategyContext.strategy.name}" ${strategyContext.parameter} (${if(strategyContext.succeeding.isDefined) "Success" else "Failure"})\n""" + strategyContext.strategyStepContextSequence.foldLeft("")((string, strategyStepContext) => {
            string + print(strategyStepContext, depth + 4)
        })
    }

    def print[S, T1, T2](actionElementContext : StrategyStepContext[S], depth : Int) : String = {
        actionElementContext match {
            case eventContext : EventContext[S, T1] => print(eventContext, depth)
            case subgoalContext : SubgoalContext[S, T1, T2] => print(subgoalContext, depth)
        }
    }

    def print[S, T](eventContext : EventContext[S, T], depth : Int) : String = {
        s"""${" " * depth}Event "${eventContext.event.name}" (${if(eventContext.succeeding.isDefined) "Success" else "Failure"})\n"""
    }

    def print[S, T1, T2](subgoalContext : SubgoalContext[S, T1, T2], depth : Int) : String = {
        print(subgoalContext.goalContext, depth)
    }
}
