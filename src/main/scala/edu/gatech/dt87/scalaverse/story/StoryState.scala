package edu.gatech.dt87.scalaverse.story

import edu.gatech.dt87.scalaverse.story.attribute.Attribute
import edu.gatech.dt87.scalaverse.story.character.{CharacterUnidirectionalRelationship, CharacterBidirectionalRelationship, Character}
import monocle.Lenser

case class StoryState(
                         attributeMap: Map[Symbol, Attribute] = Map(),
                         characterMap: Map[Int, Character] = Map(),
                         relationshipBidirectionalMap: Map[(Int, Int), CharacterBidirectionalRelationship] = Map(),
                         relationshipUnidirectionalMap: Map[(Int, Int), CharacterUnidirectionalRelationship] = Map())

object StoryState {
    val focusFactory = Lenser[StoryState]
    val focusAttributeMap = focusFactory(_.attributeMap)
    val focusCharacterMap = focusFactory(_.characterMap)
    val focusRelationshipUnidirectionalMap = focusFactory(_.relationshipUnidirectionalMap)
    val focusRelationshipBidirectionalMap = focusFactory(_.relationshipBidirectionalMap)
}