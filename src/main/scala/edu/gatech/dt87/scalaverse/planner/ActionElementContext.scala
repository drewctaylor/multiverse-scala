package edu.gatech.dt87.scalaverse.planner

/**
 * An ActionElementContext is either an EventContext or a GoalContext.
 *
 * @tparam S the state type
 */
sealed trait ActionElementContext[S] {
    /**
     * The state succeeding the Event of the EventContext or the Goal of the GoalContext.
     */
    def succeeding(): Option[S]
}

/**
 * An EventContext is an Event and the succeeding state.
 *
 * @param event the event
 * @param succeeding the succeeding state
 * @tparam S the state type
 */
case class EventContext[S](event: Event[S], succeeding: Option[S]) extends ActionElementContext[S]

/**
 * A GoalContext is a Goal and a sequence of ActionContexts.
 *
 * @param goal the Goal
 * @param actionContextSequence the sequence of ActionContexts
 * @tparam S the state type
 */
case class GoalContext[S](goal: Goal[S], actionContextSequence: ActionContext[S]*) extends ActionElementContext[S] {
    /**
     * Construct a GoalContext from this GoalContext and the given ActionContext.
     *
     * @param actionContext the given ActionContext.
     * @return the constructed GoalContext.
     */
    def append(actionContext: ActionContext[S]): GoalContext[S] = {
        new GoalContext[S](goal, actionContext +: actionContextSequence:_*)
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

