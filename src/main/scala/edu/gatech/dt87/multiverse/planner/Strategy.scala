package edu.gatech.dt87.multiverse.planner

/**
  * A strategy is a composition of strategy steps: subgoals and events.
  *
  * @tparam S the state type
  * @tparam X the strategy input type
  * @tparam Y the strategy output type
  */
trait Strategy[S, X, Y] {

  /**
    * @return the label for the strategy
    */
  def label: String

  /**
    * Given a state and a strategy input, return a description of the effort of the planner to implement that strategy.
    *
    * @param state a state
    * @param input a strategy input
    * @return a description of the effort of the planner to implement the strategy
    */
  def satisfy(state: S, input: X): StrategyExecution[S, X, Y]
}

/**
  * A strategy is a composition of strategy steps: subgoals and events.
  *
  * @param label                the label for the strategy
  * @param strategyStepSequence the sequence of strategy steps: subgoals and events.
  * @tparam S the state type
  * @tparam X the strategy input type
  * @tparam Y the strategy output type
  */
case class StrategyImplementation[S, X, Y](label: String, strategyStepSequence: StrategyStep[S, X, Y]) extends Strategy[S, X, Y] {

  /**
    * Given a state and a strategy input, return a description of the effort of the planner to implement that strategy.
    *
    * @param state a state
    * @param input a strategy input
    * @return a description of the effort of the planner to implement the strategy
    */
  def satisfy(state: S, input: X): StrategyExecution[S, X, Y] = {
    StrategyExecution(this, input, strategyStepSequence.satisfy(state, input))
  }
}

/**
  * A strategy factory.
  */
object Strategy {

  val index: Iterator[Int] = Iterator.from(0)

  /**
    * A strategy factory.
    *
    * @param label                the label for the strategy
    * @param strategyStepSequence the sequence of strategy steps: subgoals and events.
    * @tparam S the state type
    * @tparam X the strategy input type
    * @tparam Y the strategy output type
    */
  def apply[S, X, Y](label: String, strategyStepSequence: StrategyStep[S, X, Y]): Strategy[S, X, Y] = {
    StrategyImplementation(label, strategyStepSequence)
  }

  /**
    * A strategy factory; the system chooses the label for the strategy.
    *
    * @param strategyStepSequence the sequence of strategy steps: subgoals and events.
    * @tparam S the state type
    * @tparam X the strategy input type
    * @tparam Y the strategy output type
    */
  def apply[S, X, Y](strategyStepSequence: StrategyStep[S, X, Y]): Strategy[S, X, Y] = {
    StrategyImplementation(s"Unlabeled Strategy ${index.next()}", strategyStepSequence)
  }
}

/**
  * A strategy execution is strategy and a sequence of step executions: subgoal and goal executions.
  *
  * @param strategy                      the strategy
  * @param input                         the strategy input
  * @param strategyStepExecutionSequence the sequence of strategy step executions: subgoal and goal executions
  * @tparam S the state type
  * @tparam X the strategy input type
  * @tparam Y the strategy output type
  */
case class StrategyExecution[S, X, Y](strategy: Strategy[S, X, Y], input: X, strategyStepExecutionSequence: StrategyStepExecution[S, Y]) {

  /**
    * @return successor state and strategy output.
    */
  def successor(): Option[(S, Y)] = {
    strategyStepExecutionSequence.successor()
  }
}
