package edu.gatech.dt87.scalaverse

import edu.gatech.dt87.scalaverse.planner._
import edu.gatech.dt87.scalaverse.predicate.Predicate._
import edu.gatech.dt87.scalaverse.prettyPrinter._
import edu.gatech.dt87.scalaverse.story.Predicate._
import edu.gatech.dt87.scalaverse.story._
import monocle.function._
import monocle.std._
import monocle.syntax._

object Main {
    def main(args: Array[String]) {
        val state = StateNarration(State(List(
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

        val goalLife = Goal[StateNarration, (Character)]("Life",
            Strategy[StateNarration, (Character)]("Life - No Operation",
                new Event[StateNarration, (Character)]("Precondition", (stateNarration, parameter) => if(parameter.life == ALIVE) Some(stateNarration) else None),
                new Event[StateNarration, (Character)]("Narration", (stateNarration, parameter) => {
                    Some(StateNarration(stateNarration.state, Map("" -> Narration("", s"${parameter.first} lies by the pool."))))
                })
            ),
            Strategy[StateNarration, (Character)]("Life - Revive",
                new Event[StateNarration, (Character)]("Precondition", (stateNarration, character) => if(character.life == DEAD) Some(stateNarration) else None),
                new Event[StateNarration, (Character)]("Narration", (stateNarration, character) => {
                    val state1 = stateNarration.state |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(character)) |->> Character.life set ALIVE

                    Some(StateNarration(state1, Map("" -> Narration("", s"${character.first} returns to Melrose Place, very much alive."))))
                })
            )
        )

        val goalSingle = Goal[StateNarration, (Character)]("Single",
            Strategy[StateNarration, (Character)]("Single - No Operation",
                new Event[StateNarration, (Character)]("Precondition", (stateNarration, character) => if(character.spouse == None) Some(stateNarration) else None),
                new Event[StateNarration, (Character)]("Narration", (stateNarration, character) => {
                    Some(StateNarration(stateNarration.state, Map("" -> Narration("", s"${character.first} lies by the pool, dreaming of love."))))
                })
            ),
            Strategy[StateNarration, (Character)]("Single - By Death",
                new Event[StateNarration, (Character)]("Precondition", (stateNarration, character) => if(character.spouse.isDefined) Some(stateNarration) else None),
                new Event[StateNarration, (Character)]("Narration", (stateNarration, character) => {
                    val spouse = character.spouse.get
                    val state1 = stateNarration.state |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(character)) |->> Character.spouse set None
                    val state2 = state1 |-> State.characterSet |->> index(state1.characterSet.indexOf(spouse)) |->> Character.spouse set None
                    val state3 = state2 |-> State.characterSet |->> index(state2.characterSet.indexOf(spouse)) |->> Character.life set DEAD

                    Some(StateNarration(state2, Map("" -> Narration("", s"${spouse.first} dies, leaving ${character.first} alone."))))
                })
            ),
            Strategy[StateNarration, (Character)]("Single - By Divorce",
                new Event[StateNarration, (Character)]("Precondition", (stateNarration, character) => if(character.spouse.isDefined) Some(stateNarration) else None),
                new Event[StateNarration, (Character)]("Narration", (stateNarration, character) => {
                    val spouse = character.spouse.get
                    val state1 = stateNarration.state |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(character)) |->> Character.spouse set None
                    val state2 = state1 |-> State.characterSet |->> index(state1.characterSet.indexOf(spouse)) |->> Character.spouse set None

                    Some(StateNarration(state2, Map("" -> Narration("", s"${character.first} and ${spouse.first} divorce."))))
                })
            )
        )

        val goalMarriage = new Goal[StateNarration, (Character, Character)]("Marriage, Given Two Characters",
            Set(new Strategy[StateNarration, (Character, Character)]("Marriage, Given Two Characters",
                new Subgoal[StateNarration, (Character, Character), (Character)]((stateNarration, couple) => {  (couple._1) }, goalLife),
                new Subgoal[StateNarration, (Character, Character), (Character)]((stateNarration, couple) => {  (couple._2) }, goalLife),
                new Subgoal[StateNarration, (Character, Character), (Character)]((stateNarration, couple) => {  (couple._1) }, goalSingle),
                new Subgoal[StateNarration, (Character, Character), (Character)]((stateNarration, couple) => {  (couple._2) }, goalSingle),
                new Event[StateNarration, (Character, Character)]("Narration", (stateNarration, couple) => {
                    val state1 = stateNarration.state |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._1)) |->> Character.spouse set Some(couple._2)
                    val state2 = state1 |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._2)) |->> Character.spouse set Some(couple._1)

                    Some(StateNarration(state2, Map("" -> Narration("", s"${couple._1.first} and ${couple._2.first} marry."))))
                })
            ))
        )

        val goalMarriageTop = new Goal[StateNarration, Any]("Marriage",
            Set(new Strategy[StateNarration, Any]("Marriage",
                new Subgoal[StateNarration, Any, (Character, Character)]((stateNarration, any) => {
                    given(character, character).thereExists((character1, character2) => {
                        distinct(character1, character2) && compatible(character1, character2) && (
                            (character1.spouse.isDefined && character1.spouse != character2) ||
                            !character1.spouse.isDefined)
                    })(stateNarration.state).get
                }, goalMarriage))
        ))

        var goalSet = Set(goalMarriageTop)

        val eventSequence = Seq[EventContext[StateNarration, _]]()
        val goalContext1 = Planner.satisfyGoal(state, (), scala.util.Random.shuffle(Planner.satisfiableGoalSet(state, (), goalSet).toSeq).head)
        val goalContext2 = Planner.satisfyGoal(goalContext1.succeeding().get, (), scala.util.Random.shuffle(Planner.satisfiableGoalSet(goalContext1.succeeding().get, (), goalSet).toSeq).head)
        val goalContext3 = Planner.satisfyGoal(goalContext2.succeeding().get, (), scala.util.Random.shuffle(Planner.satisfiableGoalSet(goalContext2.succeeding().get, (), goalSet).toSeq).head)
        val goalContext4 = Planner.satisfyGoal(goalContext3.succeeding().get, (), scala.util.Random.shuffle(Planner.satisfiableGoalSet(goalContext3.succeeding().get, (), goalSet).toSeq).head)
        val goalContext5 = Planner.satisfyGoal(goalContext4.succeeding().get, (), scala.util.Random.shuffle(Planner.satisfiableGoalSet(goalContext4.succeeding().get, (), goalSet).toSeq).head)
        val goalContext6 = Planner.satisfyGoal(goalContext5.succeeding().get, (), scala.util.Random.shuffle(Planner.satisfiableGoalSet(goalContext5.succeeding().get, (), goalSet).toSeq).head)

        Fabula.fabula(goalContext1).foreach((eventContext : EventContext[StateNarration, _]) => {
            System.out.println(eventContext.succeeding.get.narrationMap("").text)
        })

        Fabula.fabula(goalContext2).foreach((eventContext : EventContext[StateNarration, _]) => {
            System.out.println(eventContext.succeeding.get.narrationMap("").text)
        })

        Fabula.fabula(goalContext3).foreach((eventContext : EventContext[StateNarration, _]) => {
            System.out.println(eventContext.succeeding.get.narrationMap("").text)
        })

        Fabula.fabula(goalContext4).foreach((eventContext : EventContext[StateNarration, _]) => {
            System.out.println(eventContext.succeeding.get.narrationMap("").text)
        })

        Fabula.fabula(goalContext5).foreach((eventContext : EventContext[StateNarration, _]) => {
            System.out.println(eventContext.succeeding.get.narrationMap("").text)
        })

        Fabula.fabula(goalContext6).foreach((eventContext : EventContext[StateNarration, _]) => {
            System.out.println(eventContext.succeeding.get.narrationMap("").text)
        })

        System.out.println(PrettyPrinter.print(goalContext1));
        System.out.println(PrettyPrinter.print(goalContext2));
        System.out.println(PrettyPrinter.print(goalContext3));
        System.out.println(PrettyPrinter.print(goalContext4));
        System.out.println(PrettyPrinter.print(goalContext5));
        System.out.println(PrettyPrinter.print(goalContext6));

   }
}