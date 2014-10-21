package edu.gatech.dt87.scalaverse.story.character

import edu.gatech.dt87.scalaverse.story.StoryState
import monocle.Lenser

case class Character(id : Int, attributeValueMap : Map[String, Set[String]])

object Character {
    val focusFactory = Lenser[Character]
    val focusAttributeValueMap = focusFactory(_.attributeValueMap)
}

