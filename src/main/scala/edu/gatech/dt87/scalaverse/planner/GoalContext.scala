package edu.gatech.dt87.scalaverse.planner

/**
 * A GoalContext is a Goal and a sequence of ActionContexts.
 *
 * @param goal the Goal
 * @param actionContextSequence the sequence of ActionContexts
 * @tparam S the state type
 * @tparam T the parameter type
 */
case class GoalContext[S, T](goal: Goal[S, T], actionContextSequence: StrategyContext[S, T]*) {
    /**
     * Construct a GoalContext from this GoalContext and the given StrategyContext.
     *
     * @param actionContext the given StrategyContext.
     * @return the constructed GoalContext.
     */
    def append(actionContext: StrategyContext[S, T]): GoalContext[S, T] = {
        new GoalContext[S, T](goal, actionContext +: actionContextSequence: _*)
    }

    /**
     * The succeeding state.
     *
     * @return succeeding state.
     */
    def succeeding(): Option[S] = {
        actionContextSequence.headOption match {
            case Some(actionContext) => actionContext.succeeding()
            case None => None
        }
    }
}

