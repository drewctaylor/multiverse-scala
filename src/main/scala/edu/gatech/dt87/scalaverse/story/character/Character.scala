package edu.gatech.dt87.scalaverse.story.character

import monocle.Lenser

case class Character(attributeValueMap: Map[Symbol, Set[Symbol]] = Map())

object Character {
    val focusFactory = Lenser[Character]
    val focusAttributeValueMap = focusFactory(_.attributeValueMap)
}

