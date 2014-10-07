package edu.gatech.dt87.scalaverse.story

import edu.gatech.dt87.scalaverse.story.character.Character
import monocle.SimpleLens

/**
 * A State is a set of Characters.
 *
 * @param characterSet a set of Characters
 */
case class State(characterSet: List[Character]) {
}

object State {
    val characterSet = SimpleLens[State](_.characterSet)((state, v) => state.copy(characterSet = v))
}