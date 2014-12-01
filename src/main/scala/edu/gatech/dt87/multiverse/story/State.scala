package edu.gatech.dt87.multiverse.story

import monocle.Lenser

case class Entity(attributeMap: Map[Symbol, Set[AttributeValue]] = Map())

case class EntitySet(entityMap: Map[Int, Entity] = Map())

case class State(title: Option[String],
                 seed: Option[Int],
                 entitySetMap: Map[Symbol, EntitySet] = Map(),
                 relationshipUnidirectionalMap: Map[((Symbol, Int), (Symbol, Int)), Entity] = Map(),
                 relationshipBidirectionalMap: Map[((Symbol, Int), (Symbol, Int)), Entity] = Map(),
                 narration: Option[String] = None)


object State {
    val focusFactory = Lenser[State]
    val focusTitle = focusFactory(_.title)
    val focusSeed = focusFactory(_.seed)
    val focusEntitySetMap = focusFactory(_.entitySetMap)
    val focusRelationshipUnidirectionalMap = focusFactory(_.relationshipUnidirectionalMap)
    val focusRelationshipBidirectionalMap = focusFactory(_.relationshipBidirectionalMap)
    val focusNarration = focusFactory(_.narration)
}

object EntitySet {
    val focusFactory = Lenser[EntitySet]
    val focusEntityMap = focusFactory(_.entityMap)
}

object Entity {
    val focusFactory = Lenser[Entity]
    val focusAttributeMap = focusFactory(_.attributeMap)
}