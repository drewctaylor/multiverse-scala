package edu.gatech.dt87.multiverse.prettyPrinter

import edu.gatech.dt87.multiverse.planner._
import edu.gatech.dt87.multiverse.story._
import edu.gatech.dt87.multiverse.story.state._

object PrettyPrinter {

    def json(state: State): String = {
        val jsonEntitySetMapValue = jsonEntitySetMap(state.entitySetMap)
        val jsonRelationshipBidirectionalMap = jsonRelationshipSet(state.relationshipBidirectionalMap)
        val jsonRelationshipUnidirectionalMap = jsonRelationshipSet(state.relationshipUnidirectionalMap)
        val jsonNarration = json(state.narration)

        s"""{ "entitySetMap" : $jsonEntitySetMapValue, "relationshipBidirectionalMap" : $jsonRelationshipBidirectionalMap, "relationshipUnidirectionalMap" : $jsonRelationshipUnidirectionalMap, "narration" : $jsonNarration }"""
    }

    def jsonEntitySetMap(entitySetMap: Map[Symbol, EntitySet]): String = {
        "{" + entitySetMap.map((entry) => {
            val jsonEntity = json(entry._2)

            s""""${entry._1.name}" : $jsonEntity"""
        }).mkString(",") + "}"
    }

    def json(entitySet: EntitySet): String = {
        "{" + entitySet.entityMap.map((entry) => {
            val jsonEntity = json(entry._2)

            s""""${entry._1}" : $jsonEntity"""
        }).mkString(",") + "}"
    }

    def json(entity: Entity): String = {
        "{" + entity.attributeMap.map((entry) => {
            val jsonAttributeValue = json(entry._2)

            s""""${entry._1.name}" : $jsonAttributeValue"""
        }).mkString(",") + "}"
    }

    def json(attributeValueSet: Set[AttributeValue]): String = {
        "[" + attributeValueSet.map({
            case AttributeValueBoolean(value) => value.toString
            case AttributeValueEntity(key) => jsonEntityKey(key)
            case AttributeValueNumber(value) => value.toString()
            case AttributeValueSymbol(value) => s""""${value.name}""""
            case AttributeValueSymbolOrdered(value, symbolSequence) => s""""${value.name}""""
        }).mkString(",") + "]"
    }

    def jsonRelationshipSet(entitySetMap: Map[((Symbol, Int), (Symbol, Int)), Entity]): String = {
        "[" + entitySetMap.map((entry) => {
            val key = jsonRelationshipKey(entry._1)
            val entity = json(entry._2)

            s"""{ "key" : $key, "entity" : $entity }"""
        }).mkString(",") + "]"
    }
    
    def jsonEntityKey(key : (Symbol, Int)) : String = {
        s"""{ "kind" : "${key._1.name}", "id" : "${key._2}" }"""
    }

    def jsonRelationshipKey(key: ((Symbol, Int), (Symbol, Int))): String = {
        val left = jsonEntityKey(key._1)
        val right = jsonEntityKey(key._2)

        s"""{ "left" : $left, "right" : $right }"""
    }

    def json(narration: Option[String]): String = {
        """"""" + narration.getOrElse("") + """""""
    }

    def json[S, X, Y](goalExecution: GoalExecution[S, X, Y]): String = {
        s"""{ "goal" : "${goalExecution.goal.label}", "result" : "${if (goalExecution.successor().isDefined) "Success" else "Failure"}", "strategyExecution" : [ """ + goalExecution.strategyExecutionSequence.map(json(_)).mkString(",") + " ] }"
    }

    def json[S, X, Y](strategyExecution: StrategyExecution[S, X, Y]): String = {
        s"""{ "strategy" : "${strategyExecution.strategy.label}", "result" : "${if (strategyExecution.successor().isDefined) "Success" else "Failure"}", "strategyStepExecution" : [ """ + strategyExecution.strategyStepExecutionSequence.strategyStepExecutionSequence.map(json(_)).mkString(",") + " ] }"
    }

    def json[S, Y](noExecution: NoExecution[S, Y]): String = {
        s"""{ "noop" : "", "result" : "${if (noExecution.successor().isDefined) "Success" else "Failure"}" }"""
    }

    def json[S, X, Y](eventExecution: EventExecution[S, X, Y]): String = {
        s"""{ "event" : "${eventExecution.event.label}", "result" : "${if (eventExecution.successor().isDefined) "Success" else "Failure"}" }"""
    }

    def json[S, X, X1, Y1, Y](subgoalExecution: SubgoalExecution[S, X, X1, Y1, Y]): String = {
        json(subgoalExecution.goalExecution)
    }

    def json[S, X, X1, Y1, Y](strategyStepExecution: StrategyStepExecution[S, Y]): String = {
        strategyStepExecution match {
            case noExecution: NoExecution[S, Y] => json(noExecution)
            case eventContext: EventExecution[S, X, Y] => json(eventContext)
            case subgoalContext: SubgoalExecution[S, X, X1, Y1, Y] => json(subgoalContext)
        }
    }

    def print[S, X, Y](goalExecution: GoalExecution[S, X, Y], depth: Int = 0): String = {
        goalExecution.strategyExecutionSequence.foldLeft( s"""${" " * depth}Goal "${goalExecution.goal.label}" (${if (goalExecution.successor().isDefined) "Success" else "Failure"})\n""")((string, strategyExecution) => {
            string + print(strategyExecution, depth + 4)
        })
    }

    def print[S, X, Y](strategyExecution: StrategyExecution[S, X, Y], depth: Int): String = {
        strategyExecution.strategyStepExecutionSequence.strategyStepExecutionSequence().foldLeft( s"""${" " * depth}Strategy "${strategyExecution.strategy.label}" (${if (strategyExecution.successor().isDefined) "Success" else "Failure"})\n""")((string, strategyStepExecution) => {
            string + print(strategyStepExecution, depth + 4)
        })
    }

    def print[S, X, X1, Y1, Y](strategyStepExecution: StrategyStepExecution[S, Y], depth: Int): String = {
        strategyStepExecution match {
            case noExecution: NoExecution[S, Y] => print(noExecution, depth)
            case eventContext: EventExecution[S, X, Y] => print(eventContext, depth)
            case subgoalContext: SubgoalExecution[S, X, X1, Y1, Y] => print(subgoalContext, depth)
        }
    }

    def print[S, Y](noExecution: NoExecution[S, Y], depth: Int): String = {
        s"""${" " * depth}Noop (${if (noExecution.successor().isDefined) "Success" else "Failure"})\n"""
    }

    def print[S, X, Y](eventExecution: EventExecution[S, X, Y], depth: Int): String = {
        s"""${" " * depth}Event "${eventExecution.event.label}" (${if (eventExecution.successor().isDefined) "Success" else "Failure"})\n"""
    }

    def print[S, X, X1, Y1, Y](subgoalExecution: SubgoalExecution[S, X, X1, Y1, Y], depth: Int): String = {
        print(subgoalExecution.goalExecution, depth)
    }

    def print(storyState: State, depth: Int): String = {
        storyState.entitySetMap.foldLeft("Story Entity Sets\n")((string, entitySetMapEntry) => string + printEntitySetMapEntry(entitySetMapEntry, depth + 4)) +
            storyState.relationshipBidirectionalMap.foldLeft("Bidirectional Relationships\n")((string, relationshipBidirectionalMapEntry) => string + printRelationshipBidirectionalMapEntry(relationshipBidirectionalMapEntry, depth + 4)) +
            storyState.relationshipUnidirectionalMap.foldLeft("Unidirectional Relationships\n")((string, relationshipUnidirectionalMapEntry) => string + printRelationshipUnidirectionalMapEntry(relationshipUnidirectionalMapEntry, depth + 4))
    }

    def printEntitySetMapEntry(entitySetMapEntry: (Symbol, EntitySet), depth: Int): String = {
        entitySetMapEntry._2.entityMap.foldLeft( s"""${" " * depth}Story Entity "${entitySetMapEntry._1.name}"\n""")((string, entitySetEntry) => string + printEntitySetEntry(entitySetEntry, depth + 4))
    }

    def printRelationshipBidirectionalMapEntry(relationshipMapEntry: (((Symbol, Int), (Symbol, Int)), Entity), depth: Int): String = {
        relationshipMapEntry._2.attributeMap.foldLeft( s"""${" " * depth}(${relationshipMapEntry._1._1._1.name}, ${relationshipMapEntry._1._1._2}) <-> (${relationshipMapEntry._1._2._1.name}, ${relationshipMapEntry._1._2._2})\n""")((string, attributeMapEntry) => string + printAttributeMapEntry(attributeMapEntry, depth + 4))
    }

    def printRelationshipUnidirectionalMapEntry(relationshipMapEntry: (((Symbol, Int), (Symbol, Int)), Entity), depth: Int): String = {
        relationshipMapEntry._2.attributeMap.foldLeft( s"""${" " * depth}(${relationshipMapEntry._1._1._1.name}, ${relationshipMapEntry._1._1._2}) -> (${relationshipMapEntry._1._2._1.name}, ${relationshipMapEntry._1._2._2})\n""")((string, attributeMapEntry) => string + printAttributeMapEntry(attributeMapEntry, depth + 4))
    }

    def printEntitySetEntry(entitySetEntry: (Int, Entity), depth: Int): String = {
        entitySetEntry._2.attributeMap.foldLeft( s"""${" " * depth}ID ${entitySetEntry._1}\n""")((string, attributeMapEntry) => string + printAttributeMapEntry(attributeMapEntry, depth + 4))
    }

    def printAttributeMapEntry(attributeMapEntry: (Symbol, Set[AttributeValue]), depth: Int): String = {
        s"""${" " * depth}${attributeMapEntry._1.name} := ${attributeMapEntry._2.map(printAttributeValue(_)).mkString(" + ")}\n"""
    }

    def printAttributeValue(attributeValue: AttributeValue): String = {
        attributeValue match {
            case AttributeValueBoolean(s) => s""""$s""""
            case AttributeValueEntity(e) => s"""(${e._1.name}, ${e._2})"""
            case AttributeValueNumber(n) => s"""($n)"""
            case AttributeValueSymbol(s) => s""""${s.name}""""
            case a => s"""$a"""
        }
    }
}
