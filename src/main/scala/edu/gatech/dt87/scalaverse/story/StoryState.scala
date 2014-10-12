package edu.gatech.dt87.scalaverse.story

import edu.gatech.dt87.scalaverse.story.character.Character
import monocle.SimpleLens

/**
 * A State is a set of Characters.
 *
 * @param characterSet a set of Characters
 */
case class StoryState(characterSet: List[Character]) {
}

object StoryState {
    val characterSetLens = SimpleLens[StoryState](_.characterSet)((state, v) => state.copy(characterSet = v))
}