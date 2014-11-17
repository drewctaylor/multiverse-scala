package edu.gatech.dt87.multiverse.prettyPrinter

import edu.gatech.dt87.multiverse.planner._
import edu.gatech.dt87.multiverse.story.{StoryEntity, StoryEntitySet, StoryState}

object PrettyPrinter {

    def print[S, X, Y](goalExecution : GoalExecution[S, X, Y], depth : Int = 0) : String = {
        goalExecution.strategyExecutionSequence.foldLeft(s"""${" " * depth}Goal "${goalExecution.goal.label}" (${if(goalExecution.successor().isDefined) "Success" else "Failure"})\n""")((string, strategyExecution) => {
            string + print(strategyExecution, depth + 4)
        })
    }

    def print[S, X, Y](strategyExecution : StrategyExecution[S, X, Y], depth : Int) : String = {
        strategyExecution.strategyStepExecutionSequence.strategyStepExecutionSequence().foldLeft(s"""${" " * depth}Strategy "${strategyExecution.strategy.label}" (${if(strategyExecution.successor().isDefined) "Success" else "Failure"})\n""" )((string, strategyStepExecution) => {
            string + print(strategyStepExecution, depth + 4)
        })
    }

    def print[S, X, X1, Y1, Y](strategyStepExecution : StrategyStepExecution[S, Y], depth : Int) : String = {
        strategyStepExecution match {
            case noExecution : NoExecution[S, Y] => print(noExecution, depth)
            case eventContext : EventExecution[S, X, Y] => print(eventContext, depth)
            case subgoalContext : SubgoalExecution[S, X, X1, Y1, Y] => print(subgoalContext, depth)
        }
    }

    def print[S, Y](noExecution : NoExecution[S, Y], depth : Int) : String = {
        s"""${" " * depth}Noop (${if(noExecution.successor().isDefined) "Success" else "Failure"})\n"""
    }

    def print[S, X, Y](eventExecution : EventExecution[S, X, Y], depth : Int) : String = {
        s"""${" " * depth}Event "${eventExecution.event.label}" (${if(eventExecution.successor().isDefined) "Success" else "Failure"})\n"""
    }

    def print[S, X, X1, Y1, Y](subgoalExecution : SubgoalExecution[S, X, X1, Y1, Y], depth : Int) : String = {
        print(subgoalExecution.goalExecution, depth)
    }

    def print(storyState : StoryState, depth : Int) : String = {
        storyState.entitySetMap.foldLeft("Story Entity Sets\n")((string, entitySetMapEntry) => string + printEntitySetMapEntry(entitySetMapEntry, depth + 4)) +
        storyState.relationshipBidirectionalMap.foldLeft("Bidirectional Relationships\n")((string, relationshipBidirectionalMapEntry) => string + printRelationshipBidirectionalMapEntry(relationshipBidirectionalMapEntry, depth + 4)) +
        storyState.relationshipUnidirectionalMap.foldLeft("Unidirectional Relationships\n")((string, relationshipUnidirectionalMapEntry) => string + printRelationshipUnidirectionalMapEntry(relationshipUnidirectionalMapEntry, depth + 4))
    }

    def printEntitySetMapEntry(entitySetMapEntry : (Symbol, StoryEntitySet), depth : Int) : String = {
        entitySetMapEntry._2.entityMap.foldLeft(s"""${" " * depth}Story Entity "${entitySetMapEntry._1.name}"\n""")((string, entitySetEntry) => string + printEntitySetEntry(entitySetEntry, depth + 4))
    }

    def printRelationshipBidirectionalMapEntry(relationshipMapEntry : (((Symbol, Int), (Symbol, Int)), StoryEntity), depth : Int) : String = {
        relationshipMapEntry._2.attributeMap.foldLeft(s"""${" " * depth}(${relationshipMapEntry._1._1._1.name}, ${relationshipMapEntry._1._1._2}) <-> (${relationshipMapEntry._1._2._1.name}, ${relationshipMapEntry._1._2._2})\n""")((string, attributeMapEntry) => string + printAttributeMapEntry(attributeMapEntry, depth + 4))
    }

    def printRelationshipUnidirectionalMapEntry(relationshipMapEntry : (((Symbol, Int), (Symbol, Int)), StoryEntity), depth : Int) : String = {
        relationshipMapEntry._2.attributeMap.foldLeft(s"""${" " * depth}(${relationshipMapEntry._1._1._1.name}, ${relationshipMapEntry._1._1._2}) -> (${relationshipMapEntry._1._2._1.name}, ${relationshipMapEntry._1._2._2})\n""")((string, attributeMapEntry) => string + printAttributeMapEntry(attributeMapEntry, depth + 4))
    }

    def printEntitySetEntry(entitySetEntry : (Int, StoryEntity), depth : Int) : String = {
        entitySetEntry._2.attributeMap.foldLeft(s"""${" " * depth}ID ${entitySetEntry._1}\n""")((string, attributeMapEntry) => string + printAttributeMapEntry(attributeMapEntry, depth + 4))
    }

    def printAttributeMapEntry(attributeMapEntry : (Symbol, Set[Symbol]), depth : Int) : String = {
        s"""${" " * depth}${attributeMapEntry._1.name} := ${attributeMapEntry._2.map("\"" + _.name + "\"").mkString(" + ")}\n"""
    }
}
