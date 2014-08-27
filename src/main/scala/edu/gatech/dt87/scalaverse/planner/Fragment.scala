package edu.gatech.dt87.scalaverse.planner

/**
 * A Fragment is a story fragment.
 *
 * A fragment consists of a when function - a precondition - and a then function.
 *
 * @param when a function that, when given the current state, returns Some value on success and None on failure.
 * @param then a function that, when given the current state, returns Some story state on success and None on failure.
 * @tparam S the type of the state
 */
case class Fragment[S](when: (S) => Option[Any],
                       then: (S) => Option[S]) {
    /**
     * If the given when function returns None, return None; otherwise, return Some story state or None.
     *
     * @param state the current state
     * @return either the next state or a String indicating the action that failed
     */
    def apply(state: S): Option[S] = {
        when(state) match {
            case Some(value) => then(state)
            case None => None
        }
    }
}
