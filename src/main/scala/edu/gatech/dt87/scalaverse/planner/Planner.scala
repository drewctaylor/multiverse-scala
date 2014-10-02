package edu.gatech.dt87.scalaverse.planner

object Planner {
    def satisfiableGoalSet[S, T](state: S, parameter : T, goalSet : Set[Goal[S, T]]): Set[Goal[S, T]] = {
        goalSet.filter(goal => {
            satisfyGoal(state, parameter, goal).succeeding() match {
                case Some(_) => true
                case None => false
            }
        })
    }

    def satisfyGoal[S, T](state: S, parameter : T, goal: Goal[S, T]): GoalContext[S, T] = {
        scala.util.Random.shuffle(goal.actionSet.toSeq).foldLeft[GoalContext[S, T]](GoalContext[S, T](goal))((goalContext, action) => {
            goalContext.succeeding() match {
                case Some(_) => goalContext
                case None => goalContext.append(satisfyAction(state, parameter, action))
            }
        })
    }

    def satisfyAction[S, T](state: S, parameter : T, action: Action[S, T]): ActionContext[S, T] = {
        action.actionElementSequence.tail.foldLeft[ActionContext[S, T]](ActionContext[S, T](action, satisfyActionElement(state, parameter, action.actionElementSequence.head)))((actionContext, actionElement) => {
            actionContext.succeeding() match {
                case Some(succeeding) => actionContext.append(satisfyActionElement(succeeding, parameter, actionElement))
                case None => actionContext
            }
        })
    }

    def satisfyActionElement[S, T1, T2](state: S, parameter : T1, actionElement: ActionElement[S, T1]): ActionElementContext[S] = {
        actionElement match {
            case event: Event[S, T1] => satisfyEvent(state, parameter, event)
            case subgoal: Subgoal[S, T1, T2] => satisfySubgoal(state, parameter, subgoal)
        }
    }

    def satisfyEvent[S, T](state: S, parameter : T, event: Event[S, T]): EventContext[S, T] = {
        EventContext[S, T](event, event.transition(state, parameter))
    }

    def satisfySubgoal[S, T1, T2](state: S, parameter : T1, subgoal: Subgoal[S, T1, T2]): SubgoalContext[S, T1, T2] = {
        SubgoalContext[S, T1, T2](subgoal, satisfyGoal[S, T2](state, subgoal.transition(state, parameter), subgoal.goal))
    }
}