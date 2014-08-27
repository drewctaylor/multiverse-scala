package edu.gatech.dt87.scalaverse.story

abstract sealed class Life(val name: String);

case object ALIVE extends Life("alive");

case object DEAD extends Life("dead");