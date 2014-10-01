package edu.gatech.dt87.scalaverse.planner

/**
 * An ActionElement is either an Event or a Goal.
 *
 * @tparam S the state type
 */
sealed trait ActionElement[S] {
    /**
     * The name of the Event or Goal.
     */
    val name: String
}

/**
 * A Goal is a set of Actions, any one of which may satisfy the Goal.
 *
 * @param name the name of the Goal
 * @param actionSet the set of Actions, any one of which may satisfy the Goal
 * @tparam S the state type
 */
case class Goal[S](name: String, actionSet: Set[Action[S]]) extends ActionElement[S]

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
    def apply[S](name: String, actionSequence: Action[S]*): Goal[S] = {
         Goal[S](name, actionSequence.toSet)
    }

    /**
     * A Goal factory; the system chooses the name of the Goal.
     *
     * @param actionSequence the set of Actions, any one of which may satisfy the Goal
     * @tparam S the state type
     * @return the Goal
     */
    def apply[S](actionSequence: Action[S]*): Goal[S] = {
         Goal[S](s"Unnamed Goal ${iterator.next()}", actionSequence.toSet)
    }
}

/**
 * An Event is a transition from a state to either Some state or no state.
 *
 * @param name the name of the Event.
 * @param transition the transition of the Event.
 * @tparam S the state type.
 */
case class Event[S](name: String, transition: S => Option[S]) extends ActionElement[S]

object Event {
    val iterator = Iterator.from(0)

    /**
     * An Event factory; the system chooses the name of the event.
     *
     * @param transition the transition of the Event.
     * @tparam S the state type.
     * @return the Event.
     */
    def apply[S](transition: S => Option[S]): Event[S] = {
        Event[S](s"Unnamed Event ${iterator.next()}", transition)
    }
}

// later?
//case class PEventTransition[S,A,B](run: (S,A) => Option[(S,B)]) {
//
//    def map[C](f: B => C) = PEventTransition[S,A,C] {
//        (s,a) => run(s,a) map { case (s,b) => (s,f(b)) }
//    }
//
//    def compose[C](t0: PEventTransition[S,C,A]): PEventTransition[S,C,B] = t0 andThen this
//
//    /** possibly useful functions */
//    def andThen[C](t2: PEventTransition[S,B,C]) = PEventTransition[S,A,C] {
//        (s,a) => run(s,a) flatMap { case (s,b) => t2.run(s,b) }
//    }
//
//    def toSimpleEvent(name: String)(implicit e1: Unit =:= A) =
//        new Event[S](name, s => run(s,e1(())) map { case (s,b) => s })
//
//}
//
//case class ParameterizedEvent[S,A,B](name: String, transition: (S,A) => Option[(S,B)]) extends ActionElement[S]
//object PEventTransition {
//    def event00[S](name: String, transition: S => Option[S]) = ParameterizedEvent[S,Unit,Unit](name, (s,_) => transition(s).map(_ -> ()))
//    def event20[S,A,B](name: String, transition: (S,A,B) => Option[S]) = ParameterizedEvent[S,(A,B),Unit](name, {
//        case (s, (a,b)) => transition(s,a,b).map(_ -> ())
//    })
//}
