package edu.gatech.dt87.scalaverse.story.character

import monocle.Lenser

trait CharacterRelationship {
    def characterTuple: (Int, Int)

    def attributeValueMap: Map[Symbol, Set[Symbol]]
}

case class CharacterBidirectionalRelationship(characterTuple: (Int, Int), attributeValueMap: Map[Symbol, Set[Symbol]] = Map()) extends CharacterRelationship

case class CharacterUnidirectionalRelationship(characterTuple: (Int, Int), attributeValueMap: Map[Symbol, Set[Symbol]] = Map()) extends CharacterRelationship

object CharacterBidirectionalRelationship {
    val focusFactory = Lenser[CharacterBidirectionalRelationship]
    val focusAttributeValueMap = focusFactory(_.attributeValueMap)
}

object CharacterUnidirectionalRelationship {
    val focusFactory = Lenser[CharacterUnidirectionalRelationship]
    val focusAttributeValueMap = focusFactory(_.attributeValueMap)
}