package edu.gatech.dt87.multiverse.story

import edu.gatech.dt87.multiverse.story.StateStrategyStep.SymbolMap
import edu.gatech.dt87.multiverse.story.dsl.parser._

sealed trait StateLocation

case class StateLocationAttribute(owner: StateLocationAttributeOwner, attribute: Symbol) extends StateLocation

sealed trait StateLocationAttributeOwner extends StateLocation

case class StateLocationEntity(entity: (Symbol, Int)) extends StateLocationAttributeOwner

sealed trait StateLocationRelationship extends StateLocationAttributeOwner {
    def left: StateLocationEntity

    def right: StateLocationEntity
}

case class StateLocationRelationshipBidirectional(left: StateLocationEntity, right: StateLocationEntity) extends StateLocationRelationship

case class StateLocationRelationshipUnidirectional(left: StateLocationEntity, right: StateLocationEntity) extends StateLocationRelationship

object StateLocation {

    def resolveRelationshipBidirectional(state: State, leftIn: (Symbol, Int), rightIn: (Symbol, Int), identifierSeq: Seq[ExpressionIdentifier]): Option[StateLocation] = {
        val left = if (leftIn._1.name > rightIn._1.name) rightIn else if (leftIn._1.name == rightIn._1.name && leftIn._2 > rightIn._2) rightIn else leftIn
        val right = if (leftIn._1.name > rightIn._1.name) leftIn else if (leftIn._1.name == rightIn._1.name && leftIn._2 > rightIn._2) leftIn else rightIn

        identifierSeq match {
            case Nil =>
                Some(StateLocationRelationshipBidirectional(StateLocationEntity(left), StateLocationEntity(right)))

            case ExpressionIdentifier(identifier) :: Nil =>
                Some(StateLocationAttribute(StateLocationRelationshipBidirectional(StateLocationEntity(left), StateLocationEntity(right)), identifier))

            case ExpressionIdentifier(identifier) :: identifierSeqRest =>
                val entityOption = state.relationshipBidirectionalMap.get((left, right))
                val attributeValueSetOption = entityOption.map(_.attributeMap(identifier))
                val attributeValueSeqOption = attributeValueSetOption.map(_.toSeq)

                attributeValueSeqOption match {
                    case Some(Seq(AttributeValueEntity(entityNext))) =>
                        resolve(state, entityNext, identifierSeqRest)

                    case Some(Seq(AttributeValueRelationshipBidirectional(leftNext, rightNext))) =>
                        resolveRelationshipBidirectional(state, leftNext, rightNext, identifierSeqRest)

                    case Some(Seq(AttributeValueRelationshipUnidirectional(leftNext, rightNext))) =>
                        resolveRelationshipUnidirectional(state, leftNext, rightNext, identifierSeqRest)

                    case _ =>
                        println("The system encountered a symbol which does not reference an attribute")
                        None
                }
        }
    }

    def resolveRelationshipUnidirectional(state: State, left: (Symbol, Int), right: (Symbol, Int), identifierSeq: Seq[ExpressionIdentifier]): Option[StateLocation] = {
        identifierSeq match {
            case Nil =>
                Some(StateLocationRelationshipUnidirectional(StateLocationEntity(left), StateLocationEntity(right)))

            case ExpressionIdentifier(identifier) :: Nil =>
                Some(StateLocationAttribute(StateLocationRelationshipUnidirectional(StateLocationEntity(left), StateLocationEntity(right)), identifier))

            case ExpressionIdentifier(identifier) :: identifierSeqRest =>
                val entityOption = state.relationshipUnidirectionalMap.get((left, right))
                val attributeValueSetOption = entityOption.map(_.attributeMap(identifier))
                val attributeValueSeqOption = attributeValueSetOption.map(_.toSeq)

                attributeValueSeqOption match {
                    case Some(Seq(AttributeValueEntity(entityNext))) =>
                        resolve(state, entityNext, identifierSeqRest)

                    case Some(Seq(AttributeValueRelationshipBidirectional(leftNext, rightNext))) =>
                        resolveRelationshipBidirectional(state, leftNext, rightNext, identifierSeqRest)

                    case Some(Seq(AttributeValueRelationshipUnidirectional(leftNext, rightNext))) =>
                        resolveRelationshipUnidirectional(state, leftNext, rightNext, identifierSeqRest)

                    case _ =>
                        println("The system encountered a symbol which does not reference an attribute")
                        None
                }
        }
    }

    def resolve(state: State, entity: (Symbol, Int), identifierSeq: Seq[ExpressionIdentifier]): Option[StateLocation] = {
        identifierSeq match {
            case Nil =>
                Some(StateLocationEntity(entity))

            case ExpressionIdentifier(identifier) :: Nil =>
                Some(StateLocationAttribute(StateLocationEntity(entity), identifier))

            case ExpressionIdentifier(identifier) :: identifierSeqRest =>
                val entitySetOption = state.entitySetMap.get(entity._1)
                val entityOption = entitySetOption.map(_.entityMap.get(entity._2)).flatten
                val attributeValueSetOption = entityOption.map(_.attributeMap(identifier))
                val attributeValueSeqOption = attributeValueSetOption.map(_.toSeq)

                attributeValueSeqOption match {
                    case Some(Seq(AttributeValueEntity(entityNext))) =>
                        resolve(state, entityNext, identifierSeqRest)

                    case Some(Seq(AttributeValueRelationshipBidirectional(left, right))) =>
                        resolveRelationshipBidirectional(state, left, right, identifierSeqRest)

                    case Some(Seq(AttributeValueRelationshipUnidirectional(left, right))) =>
                        resolveRelationshipUnidirectional(state, left, right, identifierSeqRest)

                    case _ =>
                        println("The system encountered a symbol which does not reference an attribute")
                        None
                }
        }
    }

    def resolve(state: State, symbolMap: SymbolMap, expressionAttributeOwner: ExpressionAttributeOwner, identifierSeq: Seq[ExpressionIdentifier]): Option[StateLocation] = {
        expressionAttributeOwner match {
            case ExpressionEntity(ExpressionIdentifier(entitySymbol)) =>
                symbolMap.get(entitySymbol) match {
                    case Some(entity) =>
                        resolve(state, entity, identifierSeq)

                    case _ =>
                        println("The system encountered an symbol which does not reference an entity.")
                        None
                }

            case ExpressionRelationshipBidirectional(left, right) =>
                (resolve(state, symbolMap, left), resolve(state, symbolMap, right)) match {
                    case (Some(StateLocationEntity(entityLeft)), Some(StateLocationEntity(entityRight))) =>
                        resolveRelationshipBidirectional(state, entityLeft, entityRight, identifierSeq)

                    case _ =>
                        println("The system encountered a bidirectional relationship with an unresolvable entity.")
                        None
                }
            case ExpressionRelationshipUnidirectional(left, right) =>
                (resolve(state, symbolMap, left), resolve(state, symbolMap, right)) match {
                    case (Some(StateLocationEntity(entityLeft)), Some(StateLocationEntity(entityRight))) =>
                        resolveRelationshipUnidirectional(state, entityLeft, entityRight, identifierSeq)

                    case _ =>
                        println("The system encountered a unidirectional relationship with an unresolvable entity.")
                        None
                }
        }
    }

    def resolve(state: State, symbolMap: SymbolMap, identifierSequence: ExpressionIdentifierSequence): Option[StateLocation] = {
        identifierSequence match {
            case ExpressionIdentifierSequence(Some(expressionAttributeOwner), identifierSeq) =>
                resolve(state, symbolMap, expressionAttributeOwner, identifierSeq)

            case ExpressionIdentifierSequence(None, _) =>
                println("The system could not resolve the entity or relationship for the attribute.")
                None

            case _ =>
                println("The system could not resolve the entity, relationship, or attribute.")
                None
        }
    }
}