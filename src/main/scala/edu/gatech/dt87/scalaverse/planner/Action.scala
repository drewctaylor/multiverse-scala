package edu.gatech.dt87.scalaverse.planner

/**
 * An Action is a sequence of ActionElements: Goals and Events.
 *
 * @param name the name of the Action
 * @param actionElementSequence the sequence of ActionElements: Goals and Events.
 * @tparam S the state type
 */
case class Action[S](name: String, actionElementSequence: ActionElement[S]*)

object Action {
    val index = Iterator.from(0)

    /**
     * An Action factory; the system chooses the name of the Action.
     *
     * @param actionElementSequence the sequence of ActionElements: Goals and Events.
     * @tparam S the state type
     * @return the Action
     */
    def apply[S](actionElementSequence: ActionElement[S]*): Action[S] = {
        Action(s"Unnamed Action ${index.next()}", actionElementSequence: _*)
    }
}