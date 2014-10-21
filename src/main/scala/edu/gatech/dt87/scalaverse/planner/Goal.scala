package edu.gatech.dt87.scalaverse.planner

import edu.gatech.dt87.scalaverse.random.Random

trait Goal[S, X, Y] {
    def name : String
    def strategySet : Set[Strategy[S, X, Y]]
    def satisfy(state : S, input : X) : GoalExecution[S, X, Y]
}

/**
 * A goal is a set of strategies, any one of which may achieve the goal.
 *
 * @param name the name of the goal
 * @param strategySet the set of strategies, any one of which may achieve the goal
 * @tparam S the state type
 * @tparam X the goal input type
 * @tparam Y the goal output type
 */
case class GoalImplementation[S, X, Y](name: String, strategySet: Set[Strategy[S, X, Y]]) extends Goal[S, X, Y] {
    /**
     * Given a state and a goal input, return a description of the effort of the planner to achieve that goal.
     *
     * @param state a state
     * @param input a goal input
     * @return the description of the effort of the planner to achieve the goal
     */
    def satisfy(state: S, input: X): GoalExecution[S, X, Y] = {
        Random.shuffle(strategySet.toSeq).foldLeft[GoalExecution[S, X, Y]](GoalExecution[S, X, Y](this, input))((goalExecution, strategy) => {
            goalExecution.successor() match {
                case Some(_) => goalExecution
                case None => goalExecution.prepend(strategy.satisfy(state, input))
            }
        })
    }
}

/**
 * A goal factory.
 */
object Goal {
    val iterator = Iterator.from(0)

    /**
     * A goal factory.
     *
     * @param name the name of the goal
     * @param strategySequence the set of strategies, any one of which may satisfy the goal
     * @tparam S the state type
     * @tparam X the goal input type
     * @tparam Y the goal output type
     * @return the goal
     */
    def apply[S, X, Y](name: String, strategySequence: Strategy[S, X, Y]*): Goal[S, X, Y] = {
        GoalImplementation[S, X, Y](name, strategySequence.toSet)
    }

    /**
     * A goal factory; the system chooses the name of the goal.
     *
     * @param strategySequence the set of strategies, any one of which may satisfy the goal
     * @tparam S the state type
     * @tparam X the goal input type
     * @tparam Y the goal output type
     * @return the goal
     */
    def apply[S, X, Y](strategySequence: Strategy[S, X, Y]*): Goal[S, X, Y] = {
        GoalImplementation[S, X, Y](s"Unnamed Goal ${iterator.next()}", strategySequence.toSet)
    }
}

/**
 * A goal execution is a description of the effort by the planner to achieve a goal: it is a goal, a goal input, and a
 * sequence of strategy executions.
 *
 * @param goal the goal
 * @param input the goal input
 * @param strategyExecutionSequence the sequence of strategy executions
 * @tparam S the state type
 * @tparam X the goal input type
 * @tparam Y the goal output type
 */
case class GoalExecution[S, X, Y](goal: Goal[S, X, Y], input : X, strategyExecutionSequence: StrategyExecution[S, X, Y]*) {

    /**
     * Construct a goal execution from this goal execution and the given strategy execution.
     *
     * @param strategyExecution the given strategy execution
     * @return the constructed goal execution
     */
    def prepend(strategyExecution: StrategyExecution[S, X, Y]): GoalExecution[S, X, Y] = {
        new GoalExecution[S, X, Y](goal, input, strategyExecution +: strategyExecutionSequence: _*)
    }

    /**
     * The successor state and goal output.
     *
     * @return successor state and goal output.
     */
    def successor(): Option[(S, Y)] = {
        strategyExecutionSequence.headOption match {
            case Some(strategyExecution) => strategyExecution.successor()
            case None => None
        }
    }
}
