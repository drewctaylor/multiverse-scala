package edu.gatech.dt87.scalaverse.planner

/**
 * An StrategyStep is either an Event or a Subgoal.
 *
 * @tparam S the state type
 * @tparam T the parameter type
 */
sealed trait StrategyStep[S, T]

/**
 * A Subgoal is a goal and a transformation between parameter types
 *
 * @param transition transformation between parameter types.
 * @param goal the goal.
 * @tparam S the state type.
 * @tparam T1 the external parameter type.
 * @tparam T2 the internal parameter type.
 */
case class Subgoal[S, T1, T2](transition: (S, T1) => T2, goal: Goal[S, T2]) extends StrategyStep[S, T1]

/**
 * An Event is an attempted state change.
 *
 * @param name the name of the Event.
 * @param transition the state change; returns None on failure
 * @tparam S the state type.
 * @tparam T the parameter type.
 */
case class Event[S, T](name: String, transition: (S, T) => Option[S]) extends StrategyStep[S, T]

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