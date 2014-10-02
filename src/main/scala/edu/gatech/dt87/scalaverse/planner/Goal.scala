package edu.gatech.dt87.scalaverse.planner

/**
 * A Goal is a set of Actions, any one of which may satisfy the Goal.
 *
 * @param name the name of the Goal
 * @param actionSet the set of Actions, any one of which may satisfy the Goal
 * @tparam S the state type
 * @tparam T the tuple type
 */
case class Goal[S, T](name: String, actionSet: Set[Action[S, T]])

object Goal {
    val iterator = Iterator.from(0)

    /**
     * A Goal factory.
     *
     * @param name the name of the Goal.
     * @param actionSequence the set of Actions, any one of which may satisfy the Goal.
     * @tparam S the state type.
     * @return the Goal.
     */
    def apply[S, T](name: String, actionSequence: Action[S, T]*): Goal[S, T] = {
        Goal[S, T](name, actionSequence.toSet)
    }

    /**
     * A Goal factory; the system chooses the name of the Goal.
     *
     * @param actionSequence the set of Actions, any one of which may satisfy the Goal
     * @tparam S the state type
     * @return the Goal
     */
    def apply[S, T](actionSequence: Action[S, T]*): Goal[S, T] = {
        Goal[S, T](s"Unnamed Goal ${iterator.next()}", actionSequence.toSet)
    }
}
