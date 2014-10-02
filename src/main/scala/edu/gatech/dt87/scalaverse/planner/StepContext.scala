package edu.gatech.dt87.scalaverse.planner

/**
 * A StepContext is either an EventContext or a SubgoalContext.
 *
 * @tparam S the state type
 */
sealed trait StepContext[S] {
    /**
     * The state succeeding the Event of the EventContext or the Subgoal of the SubgoalContext.
     */
    def succeeding(): Option[S]
}

/**
 * An EventContext is an Event and the succeeding state.
 *
 * @param event the event
 * @param succeeding the succeeding state
 * @tparam S the state type
 * @tparam T the parameter type
 */
case class EventContext[S, T](event: Event[S, T], succeeding: Option[S]) extends StepContext[S]

/**
 * An SubgoalContext is a Sugboal and a GoalContext.
 *
 * @param subgoal the subgoal
 * @param goalContext the goal context
 * @tparam S the state type
 * @tparam T1 the external parameter type
 * @tparam T2 the internal parameter type
 */
case class SubgoalContext[S, T1, T2](subgoal: Subgoal[S, T1, T2], goalContext: GoalContext[S, T2]) extends StepContext[S] {
    def succeeding() : Option[S] = {
        goalContext.succeeding()
    }
}