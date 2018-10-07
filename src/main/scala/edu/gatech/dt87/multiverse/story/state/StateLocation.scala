package edu.gatech.dt87.multiverse.story.state

import edu.gatech.dt87.multiverse.story.StateStrategyStep.SymbolMap
import edu.gatech.dt87.multiverse.story.dsl.parser._

/**
 * A location is an attribute or an attribute owner.
 */
sealed trait StateLocation

/**
 * An attribute location consists of an attribute owner and an attribute symbol.
 * @param owner an attribute owner
 * @param attribute an attribute symbol
 */
case class StateLocationAttribute(owner: StateLocationAttributeOwner, attribute: Symbol) extends StateLocation

/**
 * An attribute owner location is an entity or a relationship.
 */
sealed trait StateLocationAttributeOwner extends StateLocation

/**
 * An entity location is a tuple consisting of its kind and its id.
 * @param entity a tuple consisting of the entity's kind and its id.
 */
case class StateLocationEntity(entity: (Symbol, Int)) extends StateLocationAttributeOwner

/**
 * A relationship location is either a bidirectional relationship location or a unidirectional relationship location.
 */
sealed trait StateLocationRelationship extends StateLocationAttributeOwner {
    /**
     * The left entity location.
     * @return the left entity location
     */
    def left: StateLocationEntity

    /**
     * The right entity location
     * @return the right entity location
     */
    def right: StateLocationEntity
}

/**
 * A bidirectional relationship location consists of a left entity location and a right entity location
 * @param left the left entity location
 * @param right the right entity location
 */
case class StateLocationRelationshipBidirectional(left: StateLocationEntity, right: StateLocationEntity) extends StateLocationRelationship

/**
 * A unidirectional relationship location consists of a left entity location and a right entity location
 * @param left the left entity location
 * @param right the right entity location
 */
case class StateLocationRelationshipUnidirectional(left: StateLocationEntity, right: StateLocationEntity) extends StateLocationRelationship

/**
 * Resolve the location of an identifier, given a state and a symbol map.
 */
object StateLocation {

    /**
     * Resolve a symbol sequence in the context of a bidirectional relationship as follows:
     * 1) If the symbol sequence is empty
     *      the location is the bidirectional relationship
     *
     * 2) If the symbol sequence is one symbol
     *      the location is the attribute associated with that symbol
     *
     * 3) If the symbol sequence is more than one symbol
     *      if there is an attribute associated with the first symbol and the attribute is an entity
     *        resolve the rest of the symbol sequence in the context of that entity
     *      otherwise
     *        the system cannot resolve the identifier.
     *
     * @param state the state
     * @param leftIn the left entity
     * @param rightIn the right entity
     * @param symbolSequence the identifier sequence
     * @return the location of the identifier, if any
     */
    def resolveRelationshipBidirectional(state: State, leftIn: (Symbol, Int), rightIn: (Symbol, Int), symbolSequence: Seq[Symbol]): Option[StateLocation] = {
        val left = if (leftIn._1.name > rightIn._1.name) rightIn else if (leftIn._1.name == rightIn._1.name && leftIn._2 > rightIn._2) rightIn else leftIn
        val right = if (leftIn._1.name > rightIn._1.name) leftIn else if (leftIn._1.name == rightIn._1.name && leftIn._2 > rightIn._2) leftIn else rightIn

        symbolSequence match {
            case Nil =>
                Some(StateLocationRelationshipBidirectional(StateLocationEntity(left), StateLocationEntity(right)))

            case symbol :: Nil =>
                Some(StateLocationAttribute(StateLocationRelationshipBidirectional(StateLocationEntity(left), StateLocationEntity(right)), symbol))

            case symbol :: identifierSeqRest =>
                val entityOption = state.relationshipBidirectionalMap.get((left, right))
                val attributeValueSetOption = entityOption.flatMap(_.attributeMap.get(symbol))
                val attributeValueSeqOption = attributeValueSetOption.map(_.toSeq)

                attributeValueSeqOption match {
                    case Some(Seq(AttributeValueEntity(entityNext))) =>
                        resolve(state, entityNext, identifierSeqRest)

                    case _ =>
                        println(s"""The symbol "${symbol.name}" does not reference an entity-valued attribute.""")
                        None
                }
        }
    }


    /**
     * Resolve a symbol sequence in the context of a unidirectional relationship as follows:
     * 1) If the symbol sequence is empty
     *      the location is the unidirectional relationship
     *
     * 2) If the symbol sequence is one symbol
     *      the location is the attribute associated with that symbol
     *
     * 3) If the symbol sequence is more than one symbol
     *      if there is an attribute associated with the first symbol and the attribute is an entity
     *        resolve the rest of the symbol sequence in the context of that entity
     *      otherwise
     *        the system cannot resolve the identifier.
     *
     * @param state the state
     * @param left the left entity
     * @param right the right entity
     * @param symbolSequence the identifier sequence
     * @return the location of the identifier, if any
     */
    def resolveRelationshipUnidirectional(state: State, left: (Symbol, Int), right: (Symbol, Int), symbolSequence: Seq[Symbol]): Option[StateLocation] = {
        symbolSequence match {
            case Nil =>
                Some(StateLocationRelationshipUnidirectional(StateLocationEntity(left), StateLocationEntity(right)))

            case symbol :: Nil =>
                Some(StateLocationAttribute(StateLocationRelationshipUnidirectional(StateLocationEntity(left), StateLocationEntity(right)), symbol))

            case symbol :: symbolSequenceRest =>
                val entityOption = state.relationshipUnidirectionalMap.get((left, right))
                val attributeValueSetOption = entityOption.flatMap(_.attributeMap.get(symbol))
                val attributeValueSeqOption = attributeValueSetOption.map(_.toSeq)

                attributeValueSeqOption match {
                    case Some(Seq(AttributeValueEntity(entityNext))) =>
                        resolve(state, entityNext, symbolSequenceRest)

                    case _ =>
                        println(s"""The symbol "${symbol.name}" does not reference an entity-valued attribute.""")
                        None
                }
        }
    }

    /**
     * Resolve a symbol sequence in the context of an entity as follows:
     * 1) If the symbol sequence is empty
     *      the location is the entity
     *
     * 2) If the symbol sequence is one symbol
     *      the location is the attribute associated with that symbol
     *
     * 3) If the symbol sequence is more than one symbol
     *      if there is an attribute associated with the first symbol and the attribute is an entity
     *        resolve the rest of the symbol sequence in the context of that entity
     *      otherwise
     *        the system cannot resolve the identifier.
     *
     * @param state the state
     * @param entity the entity
     * @param symbolSequence the identifier sequence
     * @return the location of the identifier, if any
     */
    def resolve(state: State, entity: (Symbol, Int), symbolSequence: Seq[Symbol]): Option[StateLocation] = {
        symbolSequence match {
            case Nil =>
                Some(StateLocationEntity(entity))

            case symbol :: Nil =>
                Some(StateLocationAttribute(StateLocationEntity(entity), symbol))

            case symbol :: symbolSequenceRest =>
                val entitySetOption = state.entitySetMap.get(entity._1) // should be Some
                val entityOption = entitySetOption.flatMap(_.entityMap.get(entity._2)) // should be Some
                val attributeValueSetOption = entityOption.flatMap(_.attributeMap.get(symbol)) // should be Some
                val attributeValueSeqOption = attributeValueSetOption.map(_.toSeq) // may be None

                attributeValueSeqOption match {
                    case Some(Seq(AttributeValueEntity(entityNext))) =>
                        resolve(state, entityNext, symbolSequenceRest)

                    case _ =>
                        println(s"""The symbol "${symbol.name}" does not reference an entity-valued attribute.""")
                        None
                }
        }
    }

    def resolveEntity(state: State, symbolMap: SymbolMap, identifier: IdentifierAttributeQualified): Option[StateLocationEntity] = {
        val helper : (Option[Seq[AttributeValue]]) => Option[StateLocationEntity] = {
            case Some(Seq(AttributeValueEntity(entityNext))) =>
                Some(StateLocationEntity(entityNext))

            case _ =>
                println(s"""The symbol "${identifier.toString}" does not reference an entity-valued attribute.""")
                None
        }


        resolve(state, symbolMap, identifier) match {
            case Some(StateLocationAttribute(StateLocationEntity(entity), attribute)) =>
                val entitySetOption = state.entitySetMap.get(entity._1)
                val entityOption = entitySetOption.map(_.entityMap.get(entity._2)).flatten
                val attributeValueSetOption = entityOption.map(_.attributeMap.get(attribute)).flatten
                val attributeValueSeqOption = attributeValueSetOption.map(_.toSeq)

                helper(attributeValueSeqOption)

            case Some(StateLocationAttribute(StateLocationRelationshipBidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute)) =>
                val entityOption = state.relationshipBidirectionalMap.get((left, right))
                val attributeValueSetOption = entityOption.map(_.attributeMap.get(attribute)).flatten
                val attributeValueSeqOption = attributeValueSetOption.map(_.toSeq)

                helper(attributeValueSeqOption)

            case Some(StateLocationAttribute(StateLocationRelationshipUnidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute)) =>
                val entityOption = state.relationshipUnidirectionalMap.get((left, right))
                val attributeValueSetOption = entityOption.map(_.attributeMap.get(attribute)).flatten
                val attributeValueSeqOption = attributeValueSetOption.map(_.toSeq)

                helper(attributeValueSeqOption)

            case Some(sle : StateLocationEntity) =>
                Some(sle)

            case _ =>
                println(s"""The symbol "${identifier.toString}" does not reference an entity-valued attribute.""")
                None
        }
    }

    /**
     * Resolve an identifier as follows:
     * 1) If the owner is an entity
     *      if the symbol is in the symbol map
     *        resolve the symbol sequence in the context of the entity associated with that symbol.
     *      otherwise
     *        the system cannot resolve the identifier.
     *
     * 2) If the owner is a relationship (bidirectional or unidirectional) 
     *      if the system can resolve the left-entity and the right-entity,
     *        resolve the symbol sequence in the context of the relationship associated with the left-entity and the right-entity
     *      otherwise
     *        the system cannot resolve the identifier.
     *
     * @param state the state
     * @param symbolMap the symbol map
     * @param identifier an identifier
     * @return the location of the identifier, if any
     */
    def resolve(state: State, symbolMap: SymbolMap, identifier: IdentifierAttributeQualified): Option[StateLocation] = {
        identifier match {
            case IdentifierAttributeQualified(IdentifierEntity(kind, symbol), symbolSequence) =>
                val entityOption = symbolMap.get(symbol)

                entityOption match {
                    case Some(e) =>
                        resolve(state, e, symbolSequence)

                    case None =>
                        println( s"""The symbol "${symbol.name}" does not reference an entity.""")
                        println( s"""The system could not resolve "${identifier.toString}"""")
                        None
                }

            case IdentifierAttributeQualified(IdentifierRelationshipBidirectional(left, right), symbolSequence) =>
                (resolveEntity(state, symbolMap, left), resolveEntity(state, symbolMap, right)) match {
                    case (Some(StateLocationEntity(el)), Some(StateLocationEntity(er))) =>
                        resolveRelationshipBidirectional(state, el, er, symbolSequence)

                    case _ =>
                        println(s"""The system could not resolve "${identifier.toString}"""")
                        None
                }

            case IdentifierAttributeQualified(IdentifierRelationshipUnidirectional(left, right), symbolSequence) =>
                (resolveEntity(state, symbolMap, left), resolveEntity(state, symbolMap, right)) match {
                    case (Some(StateLocationEntity(el)), Some(StateLocationEntity(er))) =>
                        resolveRelationshipUnidirectional(state, el, er, symbolSequence)

                    case _ =>
                        println(s"""The system could not resolve "${identifier.toString}"""")
                        None
                }
        }
    }
}