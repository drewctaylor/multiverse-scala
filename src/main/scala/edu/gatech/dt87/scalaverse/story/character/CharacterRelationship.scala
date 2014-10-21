package edu.gatech.dt87.scalaverse.story.character

import edu.gatech.dt87.scalaverse.story.attribute.AttributeValue

case class CharacterRelationship(tuple : (Character, Character), attributeValueMap : Map[String, Set[String]])