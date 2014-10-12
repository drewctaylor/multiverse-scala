package edu.gatech.dt87.scalaverse.story.character

abstract sealed class CharacterLife(val name: String)

case object ALIVE extends CharacterLife("alive")

case object DEAD extends CharacterLife("dead")