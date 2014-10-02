package edu.gatech.dt87.scalaverse.prettyPrinter

import edu.gatech.dt87.scalaverse.planner._

object PrettyPrinter {
    def print[S, T](goalContext : GoalContext[S, T], depth : Int = 0) : String = {
        s"""${" " * depth}Goal "${goalContext.goal.name}"\n""" + goalContext.actionContextSequence.foldLeft("")((string, actionContext) => {
            string + print(actionContext, depth + 4)
        })
    }

    def print[S, T1, T2](strategyContext : StrategyContext[S, T1], depth : Int) : String = {
        s"""${" " * depth}Strategy "${strategyContext.strategy.name}"\n""" + strategyContext.stepContexts.foldLeft("")((string, actionElementContext) => {
            string + print(actionElementContext, depth + 4)
        })
    }

    def print[S, T1, T2](actionElementContext : StepContext[S], depth : Int) : String = {
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
