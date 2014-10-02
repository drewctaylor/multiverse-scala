package edu.gatech.dt87.scalaverse.planner

/**
 * An Action is a sequence of ActionElements: Goals and Events.
 *
 * @param name the name of the Action
 * @param actionElementSequence the sequence of ActionElements: Goals and Events.
 * @tparam S the state type
 * @tparam T the parameter tuple type
 */
case class Action[S, T](name: String, actionElementSequence: ActionElement[S, T]*)

object Action {
    val index = Iterator.from(0)

    /**
     * An Action factory; the system chooses the name of the Action.
     *
     * @param actionElementSequence the sequence of ActionElements: Goals and Events.
     * @tparam S the state type
     * @tparam T the parameter tuple type
     * @return the Action
     */
    def apply[S, T](actionElementSequence: ActionElement[S, T]*): Action[S, T] = {
        Action(s"Unnamed Action ${index.next()}", actionElementSequence: _*)
    }
}