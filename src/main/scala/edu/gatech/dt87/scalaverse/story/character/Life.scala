package edu.gatech.dt87.scalaverse.story.character

abstract sealed class Life(val name: String)

case object ALIVE extends Life("alive")

case object DEAD extends Life("dead")