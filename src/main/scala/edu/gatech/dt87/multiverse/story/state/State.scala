package edu.gatech.dt87.multiverse.story.state

import monocle.Lens
import monocle.function.all._
import monocle.macros.GenLens
import monocle.syntax.all._

/**
  * An entity consists of a map from symbols to sets of attribute values.
  *
  * @param attributeMap a map from symbols to sets of attribute values.
  */
case class Entity(attributeMap: Map[Symbol, Set[AttributeValue]] = Map())

object Entity {
  val focusFactory: GenLens[Entity] = GenLens[Entity]
  val focusAttributeMap: Lens[Entity, Map[Symbol, Set[AttributeValue]]] = focusFactory(_.attributeMap)
}

/**
  * An entity set consists of a map from an id to an entity.
  *
  * @param entityMap a map from an id to an entity.
  */
case class EntitySet(entityMap: Map[Int, Entity] = Map())

object EntitySet {
  val focusFactory: GenLens[EntitySet] = GenLens[EntitySet]
  val focusEntityMap: Lens[EntitySet, Map[Int, Entity]] = focusFactory(_.entityMap)
}

/**
  * @param title                         the title of the state
  * @param entitySetMap                  the entities in the state
  * @param relationshipUnidirectionalMap the unidirectional relationships in the state
  * @param relationshipBidirectionalMap  the bidirectional relationships in the state
  * @param narration                     the narration for the state
  */
case class State(title: Option[String] = None,
                 entitySetMap: Map[Symbol, EntitySet] = Map(),
                 relationshipUnidirectionalMap: Map[((Symbol, Int), (Symbol, Int)), Entity] = Map(),
                 relationshipBidirectionalMap: Map[((Symbol, Int), (Symbol, Int)), Entity] = Map(),
                 narration: Option[String] = None)

object State {
  val focusFactory: GenLens[State] = GenLens[State]
  val focusTitle: Lens[State, Option[String]] = focusFactory(_.title)
  val focusEntitySetMap: Lens[State, Map[Symbol, EntitySet]] = focusFactory(_.entitySetMap)
  val focusRelationshipUnidirectionalMap: Lens[State, Map[((Symbol, Int), (Symbol, Int)), Entity]] = focusFactory(_.relationshipUnidirectionalMap)
  val focusRelationshipBidirectionalMap: Lens[State, Map[((Symbol, Int), (Symbol, Int)), Entity]] = focusFactory(_.relationshipBidirectionalMap)
  val focusNarration: Lens[State, Option[String]] = focusFactory(_.narration)

  def insert(state: State, entity: (Symbol, Int)): State = {

    state.applyLens(State.focusEntitySetMap)
      .composeLens(at(entity._1))
      .modify({
        case None => Some(EntitySet())
        case Some(es) => Some(es)
      }).applyLens(State.focusEntitySetMap)
      .composeOptional(index(entity._1))
      .composeLens(EntitySet.focusEntityMap)
      .composeLens(at(entity._2))
      .modify({
        case None => Some(Entity())
        case Some(en) => Some(en)
      })
  }

  def upsertRelationshipUnidirectional(state: State, left: (Symbol, Int), right: (Symbol, Int)): State = {
    state.applyLens(State.focusRelationshipUnidirectionalMap)
      .composeLens(at(left, right))
      .modify({
        case None => Some(Entity())
        case Some(e) => Some(e)
      })
  }

  def upsertRelationshipBidirectional(state: State, left: (Symbol, Int), right: (Symbol, Int)): State = {
    state.applyLens(State.focusRelationshipBidirectionalMap)
      .composeLens(at(left, right))
      .modify({
        case None => Some(Entity())
        case Some(e) => Some(e)
      })
  }

  def update(state: State, stateLocation: StateLocation, symbolSet: Option[Set[AttributeValue]]): Option[State] = {

    stateLocation match {
      case StateLocationAttribute(StateLocationEntity(entity), attribute) =>
        Some(state.applyLens(State.focusEntitySetMap)
          .composeOptional(index(entity._1))
          .composeLens(EntitySet.focusEntityMap)
          .composeOptional(index(entity._2))
          .composeLens(Entity.focusAttributeMap)
          .composeLens(at(attribute))
          .set(symbolSet))

      case StateLocationAttribute(StateLocationRelationshipBidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute) =>
        Some(upsertRelationshipBidirectional(state, left, right)
          .applyLens(State.focusRelationshipBidirectionalMap)
          .composeOptional(index((left, right)))
          .composeLens(Entity.focusAttributeMap)
          .composeLens(at(attribute))
          .set(symbolSet))

      case StateLocationAttribute(StateLocationRelationshipUnidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute) =>
        Some(upsertRelationshipUnidirectional(state, left, right)
          .applyLens(State.focusRelationshipUnidirectionalMap)
          .composeOptional(index((left, right)))
          .composeLens(Entity.focusAttributeMap)
          .composeLens(at(attribute))
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
          .composeOptional(index(entity._1))
          .composeLens(EntitySet.focusEntityMap)
          .composeOptional(index(entity._2))
          .composeLens(Entity.focusAttributeMap)
          .composeLens(at(attribute))
          .modify(union))

      case StateLocationAttribute(StateLocationRelationshipBidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute) =>
        Some(upsertRelationshipBidirectional(state, left, right)
          .applyLens(State.focusRelationshipBidirectionalMap)
          .composeOptional(index((left, right)))
          .composeLens(Entity.focusAttributeMap)
          .composeLens(at(attribute))
          .modify(union))

      case StateLocationAttribute(StateLocationRelationshipUnidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute) =>
        Some(upsertRelationshipUnidirectional(state, left, right)
          .applyLens(State.focusRelationshipUnidirectionalMap)
          .composeOptional(index((left, right)))
          .composeLens(Entity.focusAttributeMap)
          .composeLens(at(attribute))
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
          .composeOptional(index(entity._1))
          .composeLens(EntitySet.focusEntityMap)
          .composeOptional(index(entity._2))
          .composeLens(Entity.focusAttributeMap)
          .composeLens(at(attribute))
          .modify(difference))

      case StateLocationAttribute(StateLocationRelationshipBidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute) =>
        Some(upsertRelationshipBidirectional(state, left, right)
          .applyLens(State.focusRelationshipUnidirectionalMap)
          .composeOptional(index((left, right)))
          .composeLens(Entity.focusAttributeMap)
          .composeLens(at(attribute))
          .modify(difference))

      case StateLocationAttribute(StateLocationRelationshipUnidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute) =>
        Some(upsertRelationshipUnidirectional(state, left, right)
          .applyLens(State.focusRelationshipBidirectionalMap)
          .composeOptional(index((left, right)))
          .composeLens(Entity.focusAttributeMap)
          .composeLens(at(attribute))
          .modify(difference))

      case _ =>
        println("The system can only remove from an attribute; it cannot remove from an entity or a relationship.")
        None
    }
  }

}
