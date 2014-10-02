package edu.gatech.dt87.scalaverse.planner.context

import edu.gatech.dt87.scalaverse.planner.Goal

/**
 * A GoalContext is a Goal and a sequence of StrategyContexts.
 *
 * @param goal the Goal
 * @param strategyContextSequence the sequence of StrategyContexts
 * @tparam S the state type
 * @tparam T the parameter type
 */
case class GoalContext[S, T](goal: Goal[S, T], strategyContextSequence: StrategyContext[S, T]*) {
    /**
     * Construct a GoalContext from this GoalContext and the given StrategyContext.
     *
     * @param strategyContext the given StrategyContext.
     * @return the constructed GoalContext.
     */
    def append(strategyContext: StrategyContext[S, T]): GoalContext[S, T] = {
        new GoalContext[S, T](goal, strategyContext +: strategyContextSequence: _*)
    }

    /**
     * The succeeding state.
     *
     * @return succeeding state.
     */
    def succeeding(): Option[S] = {
        strategyContextSequence.headOption match {
            case Some(strategyContext) => strategyContext.succeeding()
            case None => None
        }
    }
}

