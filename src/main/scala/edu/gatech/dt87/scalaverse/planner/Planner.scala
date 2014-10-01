package edu.gatech.dt87.scalaverse.planner

/**
 * A Planner
 *
 * @param goalSet
 * @tparam S
 */
case class Planner[S](goalSet: Set[Goal[S]]) {
    def satisfiableGoalSet(state: S): Set[Goal[S]] = {
        goalSet.filter(goal => {
            satisfyGoal(state, goal).succeeding() match {
                case Some(_) => true
                case None => false
            }
        })
    }

    def satisfyGoal(state: S, goal: Goal[S]): GoalContext[S] = {
        scala.util.Random.shuffle(goal.actionSet.toSeq).foldLeft[GoalContext[S]](GoalContext[S](goal))((goalContext, action) => {
            goalContext.succeeding() match {
                case Some(_) => goalContext
                case None => goalContext.append(satisfyAction(state, action))
            }
        })
    }

    def satisfyAction(state: S, action: Action[S]): ActionContext[S] = {
        action.actionElementSequence.tail.foldLeft[ActionContext[S]](ActionContext[S](action, satisfyActionElement(state, action.actionElementSequence.head)))((actionContext, actionElement) => {
            actionContext.succeeding() match {
                case Some(succeeding) => actionContext.append(satisfyActionElement(succeeding, actionElement))
                case None => actionContext
            }
        })
    }

    def satisfyActionElement(state: S, actionElement: ActionElement[S]): ActionElementContext[S] = {
        actionElement match {
            case event: Event[S] => satisfyEvent(state, event)
            case goal: Goal[S] => satisfyGoal(state, goal)
        }
    }

    def satisfyEvent(state: S, event: Event[S]): EventContext[S] = {
        EventContext[S](event, event.transition(state))
    }
}

object Planner {
    def apply[S](goalSequence: Goal[S]*): Planner[S] = {
        Planner(goalSequence.toSet)
    }
}