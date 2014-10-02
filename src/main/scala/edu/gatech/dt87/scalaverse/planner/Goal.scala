package edu.gatech.dt87.scalaverse.planner

/**
 * A Goal is a set of Strategies, any one of which may satisfy the Goal.
 *
 * @param name the name of the Goal
 * @param strategies the set of Strategies, any one of which may satisfy the Goal
 * @tparam S the state type
 * @tparam T the parameter type
 */
case class Goal[S, T](name: String, strategies: Set[Strategy[S, T]])

object Goal {
    val iterator = Iterator.from(0)

    /**
     * A Goal factory.
     *
     * @param name the name of the Goal.
     * @param strategies the set of Strategies, any one of which may satisfy the Goal.
     * @tparam S the state type.
     * @return the Goal.
     */
    def apply[S, T](name: String, strategies: Strategy[S, T]*): Goal[S, T] = {
        Goal[S, T](name, strategies.toSet)
    }

    /**
     * A Goal factory; the system chooses the name of the Goal.
     *
     * @param strategies the set of Strategies, any one of which may satisfy the Goal
     * @tparam S the state type
     * @return the Goal
     */
    def apply[S, T](strategies: Strategy[S, T]*): Goal[S, T] = {
        Goal[S, T](s"Unnamed Goal ${iterator.next()}", strategies.toSet)
    }
}
