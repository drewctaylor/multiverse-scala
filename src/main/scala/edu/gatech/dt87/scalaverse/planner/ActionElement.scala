package edu.gatech.dt87.scalaverse.planner

/**
 * An ActionElement is either an Event or a Subgoal.
 *
 * @tparam S the state type
 * @tparam T the parameter tuple type
 */
sealed trait ActionElement[S, T]

/**
 * A Subgoal is a goal and a transition from the a parameter tuple to a succeeding parameter tuple.
 *
 * @param transition the transition
 * @param goal the goal
 * @tparam S the state type
 * @tparam T1 the parameter tuple type
 * @tparam T2 the succeeding parameter tuple type
 */
case class Subgoal[S, T1, T2](transition: (S, T1) => T2, goal: Goal[S, T2]) extends ActionElement[S, T1]

/**
 * An Event is a transition from a state to either Some state or no state.
 *
 * @param name the name of the Event.
 * @param transition the transition of the Event.
 * @tparam S the state type.
 * @tparam T the parameter tuple type
 */
case class Event[S, T](name: String, transition: (S, T) => Option[S]) extends ActionElement[S, T]

object Event {
    val iterator = Iterator.from(0)

    /**
     * An Event factory; the system chooses the name of the event.
     *
     * @param transition the transition of the Event.
     * @tparam S the state type.
     * @tparam T the parameter tuple type
     * @return the Event.
     */
    def apply[S, T](transition: (S, T) => Option[S]): Event[S, T] = {
        Event[S, T](s"Unnamed Event ${iterator.next()}", transition)
    }
}