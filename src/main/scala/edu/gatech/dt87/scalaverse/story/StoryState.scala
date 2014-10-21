package edu.gatech.dt87.scalaverse.story

import edu.gatech.dt87.scalaverse.story.character.Character
import monocle.Lenser

case class StoryState(characterMap : Map[Int, Character], relationshipMap : Map[(Int, Int), Map[String, Set[String]]] = Map(), narration : Option[String] = None)

object StoryState {
    val focusFactory = Lenser[StoryState]
    val focusCharacterMap = focusFactory(_.characterMap)
    val focusRelationshipMap = focusFactory(_.relationshipMap)
    val focusNarration = focusFactory(_.narration)
}