package edu.gatech.dt87.scalaverse.planner

/**
 * A StrategyContext is an Strategy and a sequence of ActionElementContexts: GoalContexts and EventContexts.
 *
 * @param strategy the Strategy.
 * @param stepContexts the sequence of StepContexts: SubgoalContexts and EventContexts.
 * @tparam S the state type.
 * @tparam T the parameter type.
 */
case class StrategyContext[S, T](strategy: Strategy[S, T], stepContexts: StepContext[S]*) {
    /**
     * Construct a StrategyContext from this StrategyContext and the given StepContext.
     *
     * @param stepContext the given StepContext.
     * @return the constructed StrategyContext.
     */
    def append(stepContext: StepContext[S]): StrategyContext[S, T] = {
        new StrategyContext[S, T](strategy, stepContexts :+ stepContext: _*)
    }

    /**
     * The succeeding state.
     *
     * @return succeeding state.
     */
    def succeeding(): Option[S] = {
        stepContexts.lastOption match {
            case Some(actionElementContext) => actionElementContext.succeeding()
            case None => None
        }
    }
}
