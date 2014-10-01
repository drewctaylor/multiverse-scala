package edu.gatech.dt87.scalaverse

/**
 * Created by dt87 on 9/14/14.
 */
package object planner {
    implicit def prettyPrint[S](goalContext : GoalContext[S]) : String = {
        prettyPrint(goalContext, 0)
    }

    def prettyPrint[S](goalContext : GoalContext[S], depth : Int = 0): String = {
        "\n" + (" " * depth) + "Goal " + goalContext.goal.name + goalContext.actionContextSequence.foldLeft(" ")((string, actionContext) => {
            string + "\n" + (" " * depth) + "  Action " + actionContext.action.name + actionContext.actionElementContextSequence.foldLeft(" ")((string, actionElementContext) => {
                actionElementContext match {
                    case eventContext : EventContext[S] => string + "\n" + (" " * depth) + "    Event " + eventContext.event.name + " (" + (if(eventContext.succeeding.isDefined) "Success" else "Failure") + ")"
                    case goalContext : GoalContext[S] => string + prettyPrint(goalContext, 4)
                }
            })
        })
    }

    implicit def chronicle[S](goalContext : GoalContext[S]) : Seq[EventContext[S]] = {
        goalContext.actionContextSequence.headOption match {
            case Some(actionContext) => chronicle(actionContext)
            case None => Seq()
        }
    }

    def chronicle[S](actionContext : ActionContext[S]) : Seq[EventContext[S]] = {
        actionContext.actionElementContextSequence.flatMap {
            case eventContext: EventContext[S] => Seq(eventContext)
            case goalContext: GoalContext[S] => chronicle(goalContext)
        }
    }
}
