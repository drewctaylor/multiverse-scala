package edu.gatech.dt87.scalaverse.planner

/**
 * An ActionContext is an Action and a sequence of ActionElementContexts: GoalContexts and EventContexts.
 *
 * @param action the Action.
 * @param actionElementContextSequence the sequence of ActionElementContexts: GoalContexts and EventContexts.
 * @tparam S the state type.
 */
case class ActionContext[S](action: Action[S], actionElementContextSequence: ActionElementContext[S]*) {
    /**
     * Construct an ActionContext from this ActionContext and the given ActionElementContext.
     *
     * @param actionElementContext the given ActionElementContext.
     * @return the constructed ActionContext.
     */
    def append(actionElementContext: ActionElementContext[S]): ActionContext[S] = {
        new ActionContext[S](action, actionElementContextSequence :+ actionElementContext:_*)
    }

    /**
     * The succeeding state.
     *
     * @return succeeding state.
     */
    def succeeding(): Option[S] = {
        actionElementContextSequence.lastOption match {
            case Some(actionElementContext) => actionElementContext.succeeding()
            case None => None
        }
    }
}
