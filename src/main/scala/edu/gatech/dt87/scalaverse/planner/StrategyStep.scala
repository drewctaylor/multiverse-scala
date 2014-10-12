package edu.gatech.dt87.scalaverse.planner

/**
 * An strategy step is an event or subgoal.
 *
 * @tparam S the state type
 * @tparam X the step input type
 * @tparam Y the step output type
 */
sealed trait StrategyStep[S, X, Y] {
    def name() : String

    def satisfy(state: S, input: X): StrategyStepExecution[S, Y]

    /**
     * Merge two strategy steps into a single strategy step.
     *
     * @param second the second strategy step
     * @tparam Z the second strategy step output type
     * @return
     */
    def merge[Z](second: StrategyStep[S, Y, Z]): StrategyStep[S, X, Z] = new StrategyStep[S, X, Z] {
        def name() : String = {
            s"${StrategyStep.this.name()} merge ${second.name()}"
        }

        def satisfy(state: S, input: X): StrategyStepExecution[S, Z] = {
            val strategyStepExecution = StrategyStep.this.satisfy(state, input)
            strategyStepExecution.successor() match {
                case None => strategyStepExecution merge new NoExecution()
                case Some((successorState, output)) => strategyStepExecution merge second.satisfy(successorState, output)
            }
        }
    }
}

/**
 * A Subgoal is a function that transforms the subgoal input into goal input, a goal, and a function that transforms
 * goal output into subgoal output.
 *
 * @param name the name of the subgoal
 * @param transformInput the function that transforms the subgoal input into the goal input
 * @param goal the goal.
 * @param transformOutput the function that transforms the goal output into the subgoal output
 * @tparam S the state type.
 * @tparam X the subogal input type
 * @tparam X1 the goal input type
 * @tparam Y1 the goal output type
 * @tparam Y the subgoal output type
 */
case class Subgoal[S, X, X1, Y1, Y](name: String, transformInput: (S, X) => X1, goal: Goal[S, X1, Y1], transformOutput: (S, X, Y1) => Y) extends StrategyStep[S, X, Y] {

    /**
     * Given a state and a subgoal input, return a description of the effort of the planner to satisfy that subgoal.
     *
     * @param state a state
     * @param input a subgoal input
     * @return a description of the effort of the planner to satisfy the subgoal
     */
    def satisfy(state: S, input: X): SubgoalExecution[S, X, X1, Y1, Y] = {
        SubgoalExecution[S, X, X1, Y1, Y](this, input, goal.satisfy(state, transformInput(state, input)))
    }
}

object Subgoal {
    val iterator = Iterator.from(0)

    /**
     * A Subgoal factory; the system chooses the name of the subgoal.
     *
     * @param transformInput the function that transforms the subgoal input into the goal input
     * @param goal the goal.
     * @param transformOutput the function that transforms the goal output into the subgoal output
     * @tparam S the state type.
     * @tparam X the subogal input type
     * @tparam X1 the goal input type
     * @tparam Y1 the goal output type
     * @tparam Y the subgoal output type
     */
    def apply[S, X, X1, Y1, Y](transformInput: (S, X) => X1, goal: Goal[S, X1, Y1], transformOutput: (S, X, Y1) => Y): Subgoal[S, X, X1, Y1, Y] = {
        Subgoal[S, X, X1, Y1, Y](s"Unnamed Subgoal ${iterator.next()}", transformInput, goal, transformOutput)
    }
}

/**
 * An Event is a function that transforms a state into a succeeding state
 *
 * @param name the name of the Event.
 * @param transform the function that transforms the state into the succeeding state.
 * @tparam S the state type.
 * @tparam X the event input type
 * @tparam Y the event output type
 */
case class Event[S, X, Y](name: String, transform: (S, X) => Option[(S, Y)]) extends StrategyStep[S, X, Y] {

    /**
     * Given a state and an event input, return a description of the effort of the planner to satisfy that event.
     *
     * @param state a state
     * @param input an event input
     * @return a description of the effort of the planner to satisfy the event
     */
    def satisfy(state: S, input: X): EventExecution[S, X, Y] = {
        EventExecution[S, X, Y](this, transform(state, input))
    }

}

object Event {
    val iterator = Iterator.from(0)

    /**
     * An Event factory; the system chooses the name of the event.
     *
     * @param transform the function that transforms the state into the succeeding state.
     * @tparam S the state type.
     * @tparam X the event input type
     * @tparam Y the event output type
     * @return the Event.
     */
    def apply[S, X, Y](transform: (S, X) => Option[(S, Y)]): Event[S, X, Y] = {
        Event[S, X, Y](s"Unnamed Event ${iterator.next()}", transform)
    }
}

/**
 * A strategy step execution is a sequence of event and subgoal executions.
 *
 * @tparam S the state type
 * @tparam Y the strategy step output type
 */
trait StrategyStepExecution[S, Y] {

    /**
     * @return the sequence of strategy step executions that constitute this strategy step execution
     */
    def strategyStepExecutionSequence(): Seq[StrategyStepExecution[S, _]]

    /**
     * The successor state and strategy step output
     */
    def successor(): Option[(S, Y)]

    /**
     * Merge two strategy step executions into a single strategy step execution.
     * 
     * @param second the second strategy step execution
     * @tparam Z the second strategy step output type
     * @return
     */
    def merge[Z](second: StrategyStepExecution[S, Z]): StrategyStepExecution[S, Z] = new StrategyStepExecution[S, Z] {

        def strategyStepExecutionSequence(): Seq[StrategyStepExecution[S, _]] = {
            StrategyStepExecution.this.strategyStepExecutionSequence() ++ second.strategyStepExecutionSequence()
        }

        def successor(): Option[(S, Z)] = {
            StrategyStepExecution.this.successor() match {
                case None => None
                case Some(_) => second.successor()
            }
        }
    }
}

/**
 * A subgoal execution is a subgoal and its goal execution.
 *
 * @param subgoal the subgoal
 * @param goalExecution the goal execution
 * @tparam S the state type.
 * @tparam X the subogal input type
 * @tparam X1 the goal input type
 * @tparam Y1 the goal output type
 * @tparam Y the subgoal output type
 */
case class SubgoalExecution[S, X, X1, Y1, Y](subgoal: Subgoal[S, X, X1, Y1, Y], input : X, goalExecution: GoalExecution[S, X1, Y1]) extends StrategyStepExecution[S, Y] {

    /**
     * a sequence consisting of this subgoal execution
     */
    val strategyStepExecutionSequence = Seq(this)

    /**
     * The successor state and subgoal output.
     */
    def successor(): Option[(S, Y)] = {
        goalExecution.successor() match {
            case None => None
            case Some((state, output)) => Some((state, subgoal.transformOutput(state, input, output)))
        }
    }
}

/**
 * An event execution is an event and its successor state.
 *
 * @param event the event
 * @param successor the successor state and event output
 * @tparam S the state type.
 * @tparam X the event input type
 * @tparam Y the event output type
 */
case class EventExecution[S, X, Y](event: Event[S, X, Y], successor: Option[(S, Y)]) extends StrategyStepExecution[S, Y] {

    /**
     * a sequence consisting of this event execution
     */
    val strategyStepExecutionSequence = Seq(this)
}

/**
 * An no-op execution is a strategy step execution for which the successor state is none.
 *
 * @tparam S the state type.
 * @tparam Y the strategy step output type
 */
case class NoExecution[S, Y]() extends StrategyStepExecution[S, Y] {

    /**
     * an empty sequence
     */
    val strategyStepExecutionSequence = Seq()

    /**
     * the successor state
     */
    val successor: Option[(S, Y)] = None
}