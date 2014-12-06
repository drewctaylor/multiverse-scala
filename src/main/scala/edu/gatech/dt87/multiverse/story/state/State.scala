package edu.gatech.dt87.multiverse.story.state

import monocle.Lenser
import monocle.function._
import monocle.std._
import monocle.syntax._

/**
 * An entity consists of a map from symbols to sets of attribute values.
 * @param attributeMap a map from symbols to sets of attribute values.
 */
case class Entity(attributeMap: Map[Symbol, Set[AttributeValue]] = Map())

object Entity {
    val focusFactory = Lenser[Entity]
    val focusAttributeMap = focusFactory(_.attributeMap)
}

/**
 * An entity set consists of a map from an id to an entity.
 * @param entityMap a map from an id to an entity.
 */
case class EntitySet(entityMap: Map[Int, Entity] = Map())

object EntitySet {
    val focusFactory = Lenser[EntitySet]
    val focusEntityMap = focusFactory(_.entityMap)
}

/**
 * @param title
 * @param entitySetMap
 * @param relationshipUnidirectionalMap
 * @param relationshipBidirectionalMap
 * @param narration
 */
case class State(title: Option[String] = None,
                 entitySetMap: Map[Symbol, EntitySet] = Map(),
                 relationshipUnidirectionalMap: Map[((Symbol, Int), (Symbol, Int)), Entity] = Map(),
                 relationshipBidirectionalMap: Map[((Symbol, Int), (Symbol, Int)), Entity] = Map(),
                 narration: Option[String] = None)

object State {
    val focusFactory = Lenser[State]
    val focusTitle = focusFactory(_.title)
    val focusEntitySetMap = focusFactory(_.entitySetMap)
    val focusRelationshipUnidirectionalMap = focusFactory(_.relationshipUnidirectionalMap)
    val focusRelationshipBidirectionalMap = focusFactory(_.relationshipBidirectionalMap)
    val focusNarration = focusFactory(_.narration)

    def insert(state: State, entity: (Symbol, Int)): State = {
        state.applyLens(State.focusEntitySetMap)
            .composeTraversal(at(entity._1))
            .modify({
            case None => Some(EntitySet())
            case Some(es) => Some(es)
        }).applyLens(State.focusEntitySetMap)
            .composeTraversal(index(entity._1))
            .composeTraversal(EntitySet.focusEntityMap)
            .composeTraversal(at(entity._2))
            .modify({
            case None => Some(Entity())
            case Some(en) => Some(en)
        })
    }

    def upsertRelationshipUnidirectional(state: State, left: (Symbol, Int), right: (Symbol, Int)): State = {
        state.applyLens(State.focusRelationshipUnidirectionalMap)
            .composeTraversal(at(left, right))
            .modify({
            case None => Some(Entity())
            case Some(e) => Some(e)
        })
    }

    def upsertRelationshipBidirectional(state: State, left: (Symbol, Int), right: (Symbol, Int)): State = {
        state.applyLens(State.focusRelationshipBidirectionalMap)
            .composeTraversal(at(left, right))
            .modify({
            case None => Some(Entity())
            case Some(e) => Some(e)
        })
    }

    def update(state: State, stateLocation: StateLocation, symbolSet: Option[Set[AttributeValue]]): Option[State] = {

        stateLocation match {
            case StateLocationAttribute(StateLocationEntity(entity), attribute) =>
                Some(state.applyLens(State.focusEntitySetMap)
                    .composeTraversal(index(entity._1))
                    .composeTraversal(EntitySet.focusEntityMap)
                    .composeTraversal(index(entity._2))
                    .composeTraversal(Entity.focusAttributeMap)
                    .composeTraversal(at(attribute))
                    .set(symbolSet))

            case StateLocationAttribute(StateLocationRelationshipBidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute) =>
                Some(upsertRelationshipBidirectional(state, left, right)
                    .applyLens(State.focusRelationshipBidirectionalMap)
                    .composeTraversal(index((left, right)))
                    .composeTraversal(Entity.focusAttributeMap)
                    .composeTraversal(at(attribute))
                    .set(symbolSet))

            case StateLocationAttribute(StateLocationRelationshipUnidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute) =>
                Some(upsertRelationshipUnidirectional(state, left, right)
                    .applyLens(State.focusRelationshipUnidirectionalMap)
                    .composeTraversal(index((left, right)))
                    .composeTraversal(Entity.focusAttributeMap)
                    .composeTraversal(at(attribute))
                    .set(symbolSet))

            case _ =>
                println("The system can only update an attribute; it cannot update an entity or a relationship.")
                None

        }
    }

    def insert(state: State, stateLocation: StateLocation, symbolSet: Option[Set[AttributeValue]]): Option[State] = {
        val union = AttributeValueOperation.union(symbolSet) _

        stateLocation match {
            case StateLocationAttribute(StateLocationEntity(entity), attribute) =>
                Some(state.applyLens(State.focusEntitySetMap)
                    .composeTraversal(index(entity._1))
                    .composeTraversal(EntitySet.focusEntityMap)
                    .composeTraversal(index(entity._2))
                    .composeTraversal(Entity.focusAttributeMap)
                    .composeTraversal(at(attribute))
                    .modify(union))

            case StateLocationAttribute(StateLocationRelationshipBidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute) =>
                Some(upsertRelationshipBidirectional(state, left, right)
                    .applyLens(State.focusRelationshipBidirectionalMap)
                    .composeTraversal(index((left, right)))
                    .composeTraversal(Entity.focusAttributeMap)
                    .composeTraversal(at(attribute))
                    .modify(union))

            case StateLocationAttribute(StateLocationRelationshipUnidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute) =>
                Some(upsertRelationshipUnidirectional(state, left, right)
                    .applyLens(State.focusRelationshipUnidirectionalMap)
                    .composeTraversal(index((left, right)))
                    .composeTraversal(Entity.focusAttributeMap)
                    .composeTraversal(at(attribute))
                    .modify(union))

            case _ =>
                println("The system can only insert into an attribute; it cannot insert to an entity or a relationship.")
                None
        }
    }

    def remove(state: State, stateLocation: StateLocation, symbolSet: Option[Set[AttributeValue]]): Option[State] = {
        val difference = AttributeValueOperation.difference(symbolSet) _

        stateLocation match {
            case StateLocationAttribute(StateLocationEntity(entity), attribute) =>
                Some(state.applyLens(State.focusEntitySetMap)
                    .composeTraversal(index(entity._1))
                    .composeTraversal(EntitySet.focusEntityMap)
                    .composeTraversal(index(entity._2))
                    .composeTraversal(Entity.focusAttributeMap)
                    .composeTraversal(at(attribute))
                    .modify(difference))

            case StateLocationAttribute(StateLocationRelationshipBidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute) =>
                Some(upsertRelationshipBidirectional(state, left, right)
                    .applyLens(State.focusRelationshipUnidirectionalMap)
                    .composeTraversal(index((left, right)))
                    .composeTraversal(Entity.focusAttributeMap)
                    .composeTraversal(at(attribute))
                    .modify(difference))

            case StateLocationAttribute(StateLocationRelationshipUnidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute) =>
                Some(upsertRelationshipUnidirectional(state, left, right)
                    .applyLens(State.focusRelationshipBidirectionalMap)
                    .composeTraversal(index((left, right)))
                    .composeTraversal(Entity.focusAttributeMap)
                    .composeTraversal(at(attribute))
                    .modify(difference))

            case _ =>
                println("The system can only remove from an attribute; it cannot remove from an entity or a relationship.")
                None
        }
    }

}
