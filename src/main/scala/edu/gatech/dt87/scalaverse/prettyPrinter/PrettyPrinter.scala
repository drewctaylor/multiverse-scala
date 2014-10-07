package edu.gatech.dt87.scalaverse.prettyPrinter

import edu.gatech.dt87.scalaverse.planner._

object PrettyPrinter {
    def print[S, X, Y](goalExecution : GoalExecution[S, X, Y], depth : Int = 0) : String = {
        s"""${" " * depth}Goal "${goalExecution.goal.name}" (${if(goalExecution.successor.isDefined) "Success" else "Failure"})\n""" + goalExecution.strategyExecutionSequence.foldLeft("")((string, strategyExecution) => {
            string + print(strategyExecution, depth + 4)
        })
    }

    def print[S, X, Y](strategyExecution : StrategyExecution[S, X, Y], depth : Int) : String = {
        s"""${" " * depth}Strategy "${strategyExecution.strategy.name}" (${if(strategyExecution.successor.isDefined) "Success" else "Failure"})\n""" + strategyExecution.strategyStepExecutionSequence.strategyStepExecutionSequence().foldLeft("")((string, strategyStepExecution) => {
            string + print(strategyStepExecution, depth + 4)
        })
    }

    def print[S, X, X1, Y1, Y](strategyStepExecution : StrategyStepExecution[S, Y], depth : Int) : String = {
        strategyStepExecution match {
            case noExecution : NoExecution[S, Y] => print(noExecution, depth)
            case eventContext : EventExecution[S, X, Y] => print(eventContext, depth)
            case subgoalContext : SubgoalExecution[S, X, X1, Y1, Y] => print(subgoalContext, depth)
        }
    }

    def print[S, Y](noExecution : NoExecution[S, Y], depth : Int) : String = {
        s"""${" " * depth}Noop (${if(noExecution.successor.isDefined) "Success" else "Failure"})\n"""
    }

    def print[S, X, Y](eventExecution : EventExecution[S, X, Y], depth : Int) : String = {
        s"""${" " * depth}Event "${eventExecution.event.name}" (${if(eventExecution.successor.isDefined) "Success" else "Failure"})\n"""
    }

    def print[S, X, X1, Y1, Y](subgoalExecution : SubgoalExecution[S, X, X1, Y1, Y], depth : Int) : String = {
        print(subgoalExecution.goalExecution, depth)
    }
}
