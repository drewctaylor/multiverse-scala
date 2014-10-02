package edu.gatech.dt87.scalaverse.planner

/**
 * A Strategy is a sequence of StrategySteps: Goals and Events.
 *
 * @param name the name of the Strategy
 * @param steps the sequence of StrategySteps: Goals and Events.
 * @tparam S the state type
 * @tparam T the parameter type
 */
case class Strategy[S, T](name: String, steps: StrategyStep[S, T]*)

object Strategy {
    val index = Iterator.from(0)

    /**
     * A Strategy factory; the system chooses the name of the Strategy.
     *
     * @param steps the sequence of StrategySteps: Goals and Events.
     * @tparam S the state type
     * @tparam T the parameter type
     * @return the Strategy
     */
    def apply[S, T](steps: StrategyStep[S, T]*): Strategy[S, T] = {
        Strategy(s"Unnamed Strategy ${index.next()}", steps: _*)
    }
}