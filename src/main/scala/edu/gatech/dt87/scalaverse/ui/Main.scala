package edu.gatech.dt87.scalaverse.ui

import edu.gatech.dt87.scalaverse.planner._
import edu.gatech.dt87.scalaverse.predicate.Predicate._
import edu.gatech.dt87.scalaverse.story.character._
import edu.gatech.dt87.scalaverse.story.character.CharacterPredicate._
import edu.gatech.dt87.scalaverse.story._
import monocle.function._
import monocle.std._
import monocle.syntax._

object Main {
    def main(argument: Array[String]): Unit = {
        var state = StateNarration(StoryState(List(
            Character("Amanda", "Woodward", FEMALE, Set(MALE), 30),
            Character("Alison", "Parker", FEMALE, Set(MALE), 30),
            Character("Jane", "Andrews", FEMALE, Set(MALE), 30),
            Character("Jake", "Hanson", MALE, Set(MALE, FEMALE), 30),
            Character("Billy", "Campbell", MALE, Set(FEMALE), 30),
            Character("Matt", "Fielding", MALE, Set(MALE), 30),
            Character("Peter", "Burns", MALE, Set(FEMALE), 30),
            Character("Michael", "Mancini", MALE, Set(FEMALE), 30),
            Character("Sydney", "Andrews", FEMALE, Set(MALE, FEMALE), 30),
            Character("Kimberly", "Shaw", FEMALE, Set(MALE, FEMALE), 30))), Map("" -> Narration("", "Once upon a time . . .")))

        val goalLife = Goal[StateNarration, Character, Unit]("Life",
            new Strategy[StateNarration, Character, Unit]("Life - No Operation",
                new Event[StateNarration, Character, Character]("Precondition", (stateNarration, character) => if (character.life == ALIVE) Some(stateNarration, character) else None) merge
                    new Event[StateNarration, Character, Unit]("Narration", (stateNarration, character) => {
                        Some(StateNarration(stateNarration.state, Map("" -> Narration("", s"${character.first} lies by the pool."))), ())
                    })
            ),
            new Strategy[StateNarration, Character, Unit]("Life - Revive",
                new Event[StateNarration, Character, Character]("Precondition", (stateNarration, character) => if (character.life == DEAD) Some(stateNarration, character) else None) merge
                    new Event[StateNarration, Character, Unit]("Narration", (stateNarration, character) => {
                        val state1 = stateNarration.state |-> StoryState.characterSetLens |->> index(stateNarration.state.characterSet.indexOf(character)) |->> Character.life set ALIVE

                        Some(StateNarration(state1, Map("" -> Narration("", s"${character.first} returns to Melrose Place, very much alive."))), ())
                    })
            )
        )

        val goalSingle = Goal[StateNarration, Character, Unit]("Single",
            new Strategy[StateNarration, Character, Unit]("Single - No Operation",
                new Event[StateNarration, Character, Character]("Precondition", (stateNarration, character) => if (character.spouse == None) Some(stateNarration, character) else None) merge
                    new Event[StateNarration, Character, Unit]("Narration", (stateNarration, character) => {
                        Some(StateNarration(stateNarration.state, Map("" -> Narration("", s"${character.first} lies by the pool, dreaming of love."))), ())
                    })
            ),
            new Strategy[StateNarration, Character, Unit]("Single - By Death",
                new Event[StateNarration, Character, Character]("Precondition", (stateNarration, character) => if (character.spouse.isDefined) Some(stateNarration, character) else None) merge
                    new Event[StateNarration, Character, Unit]("Narration", (stateNarration, character) => {
                        val spouse = character.spouse.get
                        val state1 = stateNarration.state |-> StoryState.characterSetLens |->> index(stateNarration.state.characterSet.indexOf(character)) |->> Character.spouse set None
                        val state2 = state1 |-> StoryState.characterSetLens |->> index(state1.characterSet.indexOf(spouse)) |->> Character.spouse set None
                        val state3 = state2 |-> StoryState.characterSetLens |->> index(state2.characterSet.indexOf(spouse)) |->> Character.life set DEAD
                        Some(StateNarration(state3, Map("" -> Narration("", s"$spouse dies, leaving ${character.first} alone."))), ())
                    })
            ),
            new Strategy[StateNarration, Character, Unit]("Single - By Divorce",
                new Event[StateNarration, Character, Character]("Precondition", (stateNarration, character) => if (character.spouse.isDefined) Some(stateNarration, character) else None) merge
                    new Event[StateNarration, Character, Unit]("Narration", (stateNarration, character) => {
                        val spouse = character.spouse.get
                        val state1 = stateNarration.state |-> StoryState.characterSetLens |->> index(stateNarration.state.characterSet.indexOf(character)) |->> Character.spouse set None
                        val state2 = state1 |-> StoryState.characterSetLens |->> index(state1.characterSet.indexOf(spouse)) |->> Character.spouse set None

                        Some(StateNarration(state2, Map("" -> Narration("", s"${character.first} and $spouse divorce."))), ())
                    })
            )
        )

        def tIn1(stateNarration: StateNarration, couple: (Character, Character)): Character = couple._1
        def tIn2(stateNarration: StateNarration, couple: (Character, Character)): Character = couple._2
        def tOut(stateNarration: StateNarration, couple: (Character, Character), output: Unit): (Character, Character) = couple

        val goalMarriage = new Goal[StateNarration, (Character, Character), Unit]("Marriage, Given Two Characters",
            Set(new Strategy[StateNarration, (Character, Character), Unit]("Marriage, Given Two Characters",
                Subgoal[StateNarration, (Character, Character), Character, Unit, (Character, Character)](tIn1 _, goalLife, tOut _) merge
                    Subgoal[StateNarration, (Character, Character), Character, Unit, (Character, Character)](tIn2 _, goalLife, tOut _) merge
                    Subgoal[StateNarration, (Character, Character), Character, Unit, (Character, Character)](tIn1 _, goalSingle, tOut _) merge
                    Subgoal[StateNarration, (Character, Character), Character, Unit, (Character, Character)](tIn2 _, goalSingle, tOut _) merge
                    new Event[StateNarration, (Character, Character), Unit]("Narration", (stateNarration, couple) => {
                        val state1 = stateNarration.state |-> StoryState.characterSetLens |->> index(stateNarration.state.characterSet.indexOf(couple._1)) |->> Character.spouse set Some(couple._2.id)
                        val state2 = state1 |-> StoryState.characterSetLens |->> index(stateNarration.state.characterSet.indexOf(couple._2)) |->> Character.spouse set Some(couple._1.id)

                        Some(StateNarration(state2, Map("" -> Narration("", s"${couple._1.first} and ${couple._2.first} marry."))), ())
                    })
            ))
        )

        def tInP(stateNarration: StateNarration, input: Unit): (Character, Character) = given(CharacterPredicate.character, CharacterPredicate.character).thereExists((character1, character2) => {
            CharacterPredicate.compatible(character1, character2) && !CharacterPredicate.areMarried(character1, character2)
        })(stateNarration.state).get

        def tOutP(stateNarration: StateNarration, input: Unit, output: Unit): Unit = Unit

        val goalMarriageTop = new Goal[StateNarration, Unit, Unit]("Marriage",
            Set(new Strategy[StateNarration, Unit, Unit]("Marriage",
                Subgoal[StateNarration, Unit, (Character, Character), Unit, Unit](tInP _, goalMarriage, tOutP _)
            )))

        var goalSet = Set(goalMarriageTop)

        new JFrameWithWebView("ui.html", new Server(state, goalSet))
    }
}