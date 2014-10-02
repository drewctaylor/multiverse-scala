package edu.gatech.dt87.scalaverse.planner

import edu.gatech.dt87.scalaverse.planner.context._

object Planner {
    /**
     * Given a state, a parameter tuple, and a set of goals, return the goals that the planner can satisfy.
     * 
     * @param state a state
     * @param parameter a parameter tuple
     * @param goalSet a goal set
     * @tparam S the state type
     * @tparam T the parameter tuple type
     * @return the subset of goals that the planner can satisfy
     */
    def satisfiableGoalSet[S, T](state: S, parameter: T, goalSet: Set[Goal[S, T]]): Set[Goal[S, T]] = {
        goalSet.filter(goal => {
            satisfyGoal(state, parameter, goal).succeeding() match {
                case Some(_) => true
                case None => false
            }
        })
    }

    /**
     * Given a state, a parameter tuple, and a goal, return a description of the effort of the planner to satisfy that goal.
     * 
     * @param state a state
     * @param parameter a parameter tuple
     * @param goal a goal
     * @tparam S the state type
     * @tparam T the parameter tuple type
     * @return the description of the effort of the planner to satisfy the goal
     */
    def satisfyGoal[S, T](state: S, parameter: T, goal: Goal[S, T]): GoalContext[S, T] = {
        scala.util.Random.shuffle(goal.strategySet.toSeq).foldLeft[GoalContext[S, T]](GoalContext[S, T](goal, parameter))((goalContext, strategy) => {
            goalContext.succeeding() match {
                case Some(_) => goalContext
                case None => goalContext.append(satisfyStrategy(state, parameter, strategy))
            }
        })
    }

    /**
     * Given a state, a parameter tuple, and a strategy, return a description of the effort of the planner to satisfy that strategy.
     *
     * @param state a state
     * @param parameter a parameter tuple
     * @param strategy a strategy
     * @tparam S the state type
     * @tparam T the parameter tuple type
     * @return a description of the effort of the planner to satisfy the strategy
     */

    def satisfyStrategy[S, T](state: S, parameter: T, strategy: Strategy[S, T]): StrategyContext[S, T] = {
        strategy.stepSequence.tail.foldLeft[StrategyContext[S, T]](StrategyContext[S, T](strategy, parameter, satisfyStrategyStep(state, parameter, strategy.stepSequence.head)))((strategyContext, strategyStep) => {
            strategyContext.succeeding() match {
                case Some(succeeding) => strategyContext.append(satisfyStrategyStep(succeeding, parameter, strategyStep))
                case None => strategyContext
            }
        })
    }

    /**
     * Given a state, a parameter tuple, and a strategy step, return a description of the effort of the planner to satisfy that strategy step.
     * 
     * @param state a state
     * @param parameter a parameter tuple
     * @param strategyStep a strategy step
     * @tparam S the state type
     * @tparam T1 the parameter tuple type
     * @tparam T2 the succeeding parameter tuple type
     * @return a description of the effort of the planner to satisfy the strategy step
     */
    
    def satisfyStrategyStep[S, T1, T2](state: S, parameter: T1, strategyStep: StrategyStep[S, T1]): StrategyStepContext[S] = {
        strategyStep match {
            case event: Event[S, T1] => satisfyEvent(state, parameter, event)
            case subgoal: Subgoal[S, T1, T2] => satisfySubgoal(state, parameter, subgoal)
        }
    }

    /**
     * Given a state, a parameter tuple, and an event, return a description of the effort of the planner to satisfy that event.
     *
     * @param state a state
     * @param parameter a parameter tuple
     * @param event an event
     * @tparam S the state type
     * @tparam T the parameter tuple type
     * @return a description of the effort of the planner to satisfy the event
     */
    def satisfyEvent[S, T](state: S, parameter: T, event: Event[S, T]): EventContext[S, T] = {
        EventContext[S, T](event, event.transition(state, parameter))
    }

    /**
     * Given a state, a parameter tuple, and a sugboal, return a description of the effort of the planner to satisfy that subgoal.
     *
     * @param state a state
     * @param parameter a parameter tuple
     * @param subgoal a subgoal
     * @tparam S the state type
     * @tparam T1 the parameter tuple type
     * @tparam T2 the succeeding parameter tuple type
     * @return a description of the effort of the planner to satisfy the subgoal
     */
    def satisfySubgoal[S, T1, T2](state: S, parameter: T1, subgoal: Subgoal[S, T1, T2]): SubgoalContext[S, T1, T2] = {
        SubgoalContext[S, T1, T2](subgoal, satisfyGoal[S, T2](state, subgoal.transition(state, parameter), subgoal.goal))
    }
}