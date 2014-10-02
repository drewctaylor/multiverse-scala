package edu.gatech.dt87.scalaverse.planner.context

import edu.gatech.dt87.scalaverse.planner.Strategy

/**
 * A StrategyContext is an Strategy and a sequence of StrategyStepContexts: GoalContexts and EventContexts.
 *
 * @param strategy the Strategy.
 * @param strategyStepContextSequence the sequence of StrategyStepContexts: SubgoalContexts and EventContexts.
 * @tparam S the state type.
 * @tparam T the parameter type.
 */
case class StrategyContext[S, T](strategy: Strategy[S, T], parameter: T, strategyStepContextSequence: StrategyStepContext[S]*) {
    /**
     * Construct a StrategyContext from this StrategyContext and the given StrategyStepContext.
     *
     * @param strategyStepContext the given StrategyStepContext.
     * @return the constructed StrategyContext.
     */
    def append(strategyStepContext: StrategyStepContext[S]): StrategyContext[S, T] = {
        new StrategyContext[S, T](strategy, parameter, strategyStepContextSequence :+ strategyStepContext: _*)
    }

    /**
     * The succeeding state.
     *
     * @return succeeding state.
     */
    def succeeding(): Option[S] = {
        strategyStepContextSequence.lastOption match {
            case Some(strategyStepContext) => strategyStepContext.succeeding()
            case None => None
        }
    }
}
