package edu.gatech.dt87.scalaverse

import edu.gatech.dt87.scalaverse.planner._
import edu.gatech.dt87.scalaverse.predicate.Predicate._
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

        val divorceCauseAffair = Action[StateNarration]("Divorce Caused By Affair",
            new Event[StateNarration]("Divorce Caused By Affair Precondition", (stateNarration) => {
                given(character, character).thereExists(areMarried).apply(stateNarration.state) match {
                    case Some(_) => Some(StateNarration(stateNarration.state, Map(""->Narration("",""))))
                    case None => None
                }
            }),
            new Event[StateNarration]("Divorce Caused By Affair Text", (stateNarration) => {
                val couple = given(character, character).thereExists(areMarried).apply(stateNarration.state).get

                val state1 = stateNarration.state |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._1)) |->> Character.spouse set None
                val state2 = state1 |-> State.characterSet |->> index(state1.characterSet.indexOf(couple._2)) |->> Character.spouse set None

                Some(StateNarration(state2, Map("" -> Narration("", s"${couple._1.first} and ${couple._2.first} divorce because of an affair."))))
            })
        )

        val divorceCauseAddiction = Action[StateNarration]("Divorce Caused By Addiction",
            new Event[StateNarration]("Divorce Caused By Addiction Precondition", stateNarration => {
                given(character, character).thereExists(areMarried).apply(stateNarration.state) match {
                    case Some(_) => Some(StateNarration(stateNarration.state, Map(""->Narration("",""))))
                    case None => None
                }
            }),
            new Event[StateNarration]("Divorce Caused By Addiction Text", stateNarration => {
                val couple = given(character, character).thereExists(areMarried).apply(stateNarration.state).get

                val state1 = stateNarration.state |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._1)) |->> Character.spouse set None
                val state2 = state1 |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._2)) |->> Character.spouse set None

                Some(StateNarration(state2, Map("" -> Narration("", s"${couple._1.first} and ${couple._2.first} divorce because of an addiction."))))
            })
        )

        val divorceCauseWork = Action[StateNarration]("Divorce Caused By Work",
            new Event[StateNarration]("Divorce Caused By Work Precondition", stateNarration => {
                given(character, character).thereExists(areMarried).apply(stateNarration.state) match {
                    case Some(_) => Some(StateNarration(stateNarration.state, Map(""->Narration("",""))))
                    case None => None
                }
            }),
            new Event[StateNarration]("Divorce Caused By Work Text", stateNarration => {
                val couple = given(character, character).thereExists(areMarried).apply(stateNarration.state).get

                val state1 = stateNarration.state |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._1)) |->> Character.spouse set None
                val state2 = state1 |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._2)) |->> Character.spouse set None

                Some(StateNarration(state2, Map("" -> Narration("", s"${couple._1.first} and ${couple._2.first} divorce because of work."))))
            })
        )

        val divorceCauseDistance = Action[StateNarration]("Divorce Caused By Distance",
            new Event[StateNarration]("Divorce Caused By Distance Precondition", stateNarration => {
                given(character, character).thereExists(areMarried).apply(stateNarration.state) match {
                    case Some(_) => Some(StateNarration(stateNarration.state, Map(""->Narration("",""))))
                    case None => None
                }
            }),
            new Event[StateNarration]("Divorce Caused By Distance Text", stateNarration => {
                val couple = given(character, character).thereExists(areMarried).apply(stateNarration.state).get

                val state1 = stateNarration.state |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._1)) |->> Character.spouse set None
                val state2 = state1 |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._2)) |->> Character.spouse set None

                Some(StateNarration(state2, Map("" -> Narration("", s"${couple._1.first} and ${couple._2.first} divorce because of distance."))))
            })
        )

        val divorceCauseDisapproval = Action[StateNarration]("Divorce Caused By Disapproval",
            new Event[StateNarration]("Divorce Caused By Disapproval Precondition", stateNarration => {
                given(character, character).thereExists(areMarried).apply(stateNarration.state) match {
                    case Some(_) => Some(StateNarration(stateNarration.state, Map(""->Narration("",""))))
                    case None => None
                }
            }),
            new Event[StateNarration]("Divorce Caused By Disapproval Text", stateNarration => {
                val couple = given(character, character).thereExists(areMarried).apply(stateNarration.state).get

                val state1 = stateNarration.state |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._1)) |->> Character.spouse set None
                val state2 = state1 |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._2)) |->> Character.spouse set None

                Some(StateNarration(state2, Map("" -> Narration("", s"${couple._1.first} and ${couple._2.first} divorce because of distance."))))
            })
        )

        val marriage = Action[StateNarration]("Marriage",
            Event[StateNarration]((stateNarration : StateNarration) => {
                given(characterIs(single), characterIs(single)).thereExists(compatible).apply(stateNarration.state) match {
                    case Some(_) => Some(StateNarration(stateNarration.state, Map(""->Narration("",""))))
                    case None => None
                }
            }),
            Event[StateNarration]((stateNarration : StateNarration) => {
                val couple = given(characterIs(single), characterIs(single)).thereExists(compatible).apply(stateNarration.state) .get

                val state1 = stateNarration.state |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._1)) |->> Character.spouse set Some(couple._2)
                val state2 = state1 |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._2)) |->> Character.spouse set Some(couple._1)

                Some(StateNarration(state2, Map("" -> Narration("", s"${couple._1.first} and ${couple._2.first} marry."))))
            })
        )

        val divorceGoal = Goal[StateNarration]("Divorce",
            divorceCauseAddiction,
            divorceCauseAffair,
            divorceCauseAddiction,
            divorceCauseAffair,
            divorceCauseDisapproval,
            divorceCauseDistance,
            divorceCauseWork)

        val marriageGoal =  Goal[StateNarration]("Marriage",
            marriage)

        val eventSequence = Seq[EventContext[StateNarration]]()
        val planner = Planner[StateNarration](marriageGoal, divorceGoal)
        val goalContext1 = planner.satisfyGoal(state, scala.util.Random.shuffle(planner.satisfiableGoalSet(state).toSeq).head)
        val goalContext2 = planner.satisfyGoal(goalContext1.succeeding().get, scala.util.Random.shuffle(planner.satisfiableGoalSet(goalContext1.succeeding().get).toSeq).head)
        val goalContext3 = planner.satisfyGoal(goalContext2.succeeding().get, scala.util.Random.shuffle(planner.satisfiableGoalSet(goalContext2.succeeding().get).toSeq).head)
        val goalContext4 = planner.satisfyGoal(goalContext3.succeeding().get, scala.util.Random.shuffle(planner.satisfiableGoalSet(goalContext3.succeeding().get).toSeq).head)
        val goalContext5 = planner.satisfyGoal(goalContext4.succeeding().get, scala.util.Random.shuffle(planner.satisfiableGoalSet(goalContext4.succeeding().get).toSeq).head)
        val goalContext6 = planner.satisfyGoal(goalContext5.succeeding().get, scala.util.Random.shuffle(planner.satisfiableGoalSet(goalContext5.succeeding().get).toSeq).head)

        var lastState = state

        val fullEventSequence = eventSequence ++
            goalContext1 ++
            goalContext2 ++
            goalContext3 ++
            goalContext4 ++
            goalContext5 ++
            goalContext6

        fullEventSequence.map(eventContext => {
            System.out.println(eventContext.event.name)
            System.out.println(eventContext.succeeding.get.state.characterSet.filter(_.first=="Amanda"))
        })

        fullEventSequence.map(eventContext => {
            System.out.println(eventContext.succeeding.get.narrationMap("").text);
            if(lastState.state.characterSet.filter(_.first=="Amanda") != eventContext.succeeding.get.state.characterSet.filter(_.first=="Amanda")) {
                System.out.println("(This affected Amanda.)");
            } else {
            }
            lastState = eventContext.succeeding.get
        })
    }
}