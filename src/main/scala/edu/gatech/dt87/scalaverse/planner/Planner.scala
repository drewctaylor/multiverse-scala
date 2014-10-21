package edu.gatech.dt87.scalaverse.planner

object Planner {
    /**
     * Given a state, an goal input, and a set of goals, return the goals that the planner can satisfy.
     *
     * @param state a state
     * @param input a goal input
     * @param goalSet a goal set
     * @tparam S the state type
     * @tparam X the goal input type
     * @tparam Y the goal output type
     * @return the subset of goals that the planner can satisfy
     */
    def satisfiableGoalSet[S, X, Y](state: S, input: X, goalSet: Set[Goal[S, X, Y]]): Set[Goal[S, X, Y]] = {
        goalSet.filter(goal => {
            goal.satisfy(state, input).successor() match {
                case Some(_) => true
                case None => false
            }
        })
    }
}