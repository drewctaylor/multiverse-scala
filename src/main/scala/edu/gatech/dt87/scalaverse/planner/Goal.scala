package edu.gatech.dt87.scalaverse.planner

/**
 * A Goal is an author goal.
 *
 * An author goal consists of a set of story fragments that may satisfy the author goal.
 *
 * @param fragmentSet a set of story fragments that may satisfy the author goal
 * @tparam S a type for the story state
 */
case class Goal[S](fragmentSet: List[Fragment[S]] = List()) {
    /**
     * Constructs a new author goal from this author goal and the given story fragment as follows: the system appends 
     * the given story fragment to the set of story fragments for this author goal, then constructs a new goal from the 
     * resulting set of story fragments.
     *
     * @param fragment a story fragment
     * @return an author goal
     */
    def fragment(fragment: Fragment[S]): Goal[S] = {
        Goal(fragment :: fragmentSet)
    }

    /**
     * Given a story state, if a story fragment satisfies the author goal, return Some story state following the story
     * fragment; otherwise, return None.
     *
     * @param state the story state
     * @return if a story fragment satisfies the author goal, Some story state; otherwise, None
     */
    def apply(state: S): Option[S] = {
        scala.util.Random.shuffle(fragmentSet).foldLeft[Option[S]](None)((accumulator: Option[S], fragment: Fragment[S]) => {
            accumulator match {
                case Some(value) => accumulator
                case None => fragment(state)
            }
        })
    }
}