package edu.gatech.dt87.multiverse.planner

object Fabula {

  def fabula[S, X, Y](goalExecution: GoalExecution[S, X, Y]): Seq[EventExecution[S, _, _]] = {
    goalExecution.strategyExecutionSequence.headOption match {
      case Some(strategyExecution) => fabula(strategyExecution)
      case None => Seq()
    }
  }

  def fabula[S, X, X1, Y1, Y](strategyExecution: StrategyExecution[S, X, Y]): Seq[EventExecution[S, _, _]] = {
    strategyExecution.strategyStepExecutionSequence.strategyStepExecutionSequence().flatMap {
      case eventContext: EventExecution[S, X, Y] => Seq(eventContext)
      case subgoalContext: SubgoalExecution[S, X, X1, Y1, Y] => fabula(subgoalContext.goalExecution)
    }
  }
}
