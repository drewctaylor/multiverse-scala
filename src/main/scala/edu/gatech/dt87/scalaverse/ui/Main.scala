package edu.gatech.dt87.scalaverse.ui

import edu.gatech.dt87.scalaverse.planner._
import edu.gatech.dt87.scalaverse.predicate.Predicate._
import edu.gatech.dt87.scalaverse.story.Predicate._
import edu.gatech.dt87.scalaverse.story._
import monocle.function._
import monocle.std._
import monocle.syntax._

object Main {
    def main(argument: Array[String]): Unit = {
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

        val alwaysFail = Action[StateNarration, Unit]("Action - Always Fail",
            new Event[StateNarration, Unit]("Action - Always Fail Precondition", (stateNarration, _) => None))

        val divorceCauseAffair1 = Action[StateNarration, Unit]("Divorce Caused By Affair",
            new Event[StateNarration, Unit]("Divorce Caused By Affair Precondition", (stateNarration, _) => {
                given(character, character, character).thereExists((spouse1, spouse2, other) => {
                    areMarried(spouse1, spouse2) && compatible(spouse1, other) && other.spouse == None
                }).apply(stateNarration.state) match {
                    case Some(_) => Some(StateNarration(stateNarration.state, Map("" -> Narration("", ""))))
                    case None => None
                }
            }),
            new Event[StateNarration, Unit]("Divorce Caused By Affair Text", (stateNarration, _) => {
                val (spouse1, spouse2, other) = given(character, character, character).thereExists((spouse1, spouse2, other) => {
                    areMarried(spouse1, spouse2) && compatible(spouse1, other)
                }).apply(stateNarration.state).get

                val state1 = stateNarration.state |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(spouse1)) |->> Character.spouse set None
                val state2 = state1 |-> State.characterSet |->> index(state1.characterSet.indexOf(spouse2)) |->> Character.spouse set None

                Some(StateNarration(state2, Map("" -> Narration("", s"${spouse1.first} and ${spouse2.first} divorce because ${spouse1.first} had an affair with ${other.first}."))))
            })
        )

        val divorceCauseAffair2 = Action[StateNarration, Unit]("Divorce Caused By Affair With Married",
            new Event[StateNarration, Unit]("Divorce Caused By Affair Precondition", (stateNarration, _) => {
                given(character, character, character, character).thereExists((spouse1, spouse2, spouse3, spouse4) => {
                    areMarried(spouse1, spouse2) && areMarried(spouse3, spouse4) && compatible(spouse1, spouse3) && distinct(spouse1, spouse3) && distinct(spouse1, spouse4)
                }).apply(stateNarration.state) match {
                    case Some(_) => Some(StateNarration(stateNarration.state, Map("" -> Narration("", ""))))
                    case None => None
                }
            }),
            new Event[StateNarration, Unit]("Divorce Caused By Affair Text", (stateNarration, _) => {
                val (spouse1, spouse2, spouse3, spouse4) = given(character, character, character, character).thereExists((spouse1, spouse2, spouse3, spouse4) => {
                    areMarried(spouse1, spouse2) && areMarried(spouse3, spouse4) && compatible(spouse1, spouse3)
                }).apply(stateNarration.state).get

                val state1 = stateNarration.state |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(spouse1)) |->> Character.spouse set None
                val state2 = state1 |-> State.characterSet |->> index(state1.characterSet.indexOf(spouse2)) |->> Character.spouse set None
                val state3 = state2 |-> State.characterSet |->> index(state2.characterSet.indexOf(spouse3)) |->> Character.spouse set None
                val state4 = state3 |-> State.characterSet |->> index(state3.characterSet.indexOf(spouse4)) |->> Character.spouse set None

                Some(StateNarration(state4, Map("" -> Narration("", s"${spouse1.first} and ${spouse2.first} divorce; ${spouse3.first} and ${spouse4.first} divorce - because ${spouse1.first} had an affair with ${spouse3.first}."))))
            })
        )

        val divorceCauseAddiction = Action[StateNarration, Unit]("Divorce Caused By Addiction",
            new Event[StateNarration, Unit]("Divorce Caused By Addiction Precondition", (stateNarration, _) => {
                given(character, character).thereExists(areMarried).apply(stateNarration.state) match {
                    case Some(_) => Some(StateNarration(stateNarration.state, Map("" -> Narration("", ""))))
                    case None => None
                }
            }),
            new Event[StateNarration, Unit]("Divorce Caused By Addiction Text", (stateNarration, _) => {
                val couple = given(character, character).thereExists(areMarried).apply(stateNarration.state).get

                val state1 = stateNarration.state |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._1)) |->> Character.spouse set None
                val state2 = state1 |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._2)) |->> Character.spouse set None

                Some(StateNarration(state2, Map("" -> Narration("", s"${couple._1.first} and ${couple._2.first} divorce because of an addiction."))))
            })
        )

        val divorceCauseWork = Action[StateNarration, Unit]("Divorce Caused By Work",
            new Event[StateNarration, Unit]("Divorce Caused By Work Precondition", (stateNarration, _) => {
                given(character, character).thereExists(areMarried).apply(stateNarration.state) match {
                    case Some(_) => Some(StateNarration(stateNarration.state, Map("" -> Narration("", ""))))
                    case None => None
                }
            }),
            new Event[StateNarration, Unit]("Divorce Caused By Work Text", (stateNarration, _) => {
                val couple = given(character, character).thereExists(areMarried).apply(stateNarration.state).get

                val state1 = stateNarration.state |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._1)) |->> Character.spouse set None
                val state2 = state1 |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._2)) |->> Character.spouse set None

                Some(StateNarration(state2, Map("" -> Narration("", s"${couple._1.first} and ${couple._2.first} divorce because of work."))))
            })
        )

        val divorceCauseDistance = Action[StateNarration, Unit]("Divorce Caused By Distance",
            new Event[StateNarration, Unit]("Divorce Caused By Distance Precondition", (stateNarration, _) => {
                given(character, character).thereExists(areMarried).apply(stateNarration.state) match {
                    case Some(_) => Some(StateNarration(stateNarration.state, Map("" -> Narration("", ""))))
                    case None => None
                }
            }),
            new Event[StateNarration, Unit]("Divorce Caused By Distance Text", (stateNarration, _) => {
                val couple = given(character, character).thereExists(areMarried).apply(stateNarration.state).get

                val state1 = stateNarration.state |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._1)) |->> Character.spouse set None
                val state2 = state1 |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._2)) |->> Character.spouse set None

                Some(StateNarration(state2, Map("" -> Narration("", s"${couple._1.first} and ${couple._2.first} divorce because of distance."))))
            })
        )

        val divorceCauseDisapproval = Action[StateNarration, Unit]("Divorce Caused By Disapproval",
            new Event[StateNarration, Unit]("Divorce Caused By Disapproval Precondition", (stateNarration, _) => {
                given(character, character).thereExists(areMarried).apply(stateNarration.state) match {
                    case Some(_) => Some(StateNarration(stateNarration.state, Map("" -> Narration("", ""))))
                    case None => None
                }
            }),
            new Event[StateNarration, Unit]("Divorce Caused By Disapproval Text", (stateNarration, _) => {
                val couple = given(character, character).thereExists(areMarried).apply(stateNarration.state).get

                val state1 = stateNarration.state |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._1)) |->> Character.spouse set None
                val state2 = state1 |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._2)) |->> Character.spouse set None

                Some(StateNarration(state2, Map("" -> Narration("", s"${couple._1.first} and ${couple._2.first} divorce because of distance."))))
            })
        )

        lazy val marriageGoal =  Goal[StateNarration, Unit]("Marriage",
            alwaysFail,
            marriageNoHistory)

        lazy val marriageNoHistory: Action[StateNarration, Unit] = Action[StateNarration, Unit]("Marriage - No History",
            new Event[StateNarration, Unit]("Marriage - No History - Precondition", (stateNarration, _) => {
                given(characterIs(single), characterIs(single)).thereExists((lover1, lover2) => {
                    compatible(lover1, lover2) && !lover1.ex.contains(lover2)
                }).apply(stateNarration.state) match {
                    case Some(_) => Some(StateNarration(stateNarration.state, Map("" -> Narration("", ""))))
                    case None => None
                }
            }),
            new Event[StateNarration, Unit]("Marriage - No History - Text", (stateNarration, _) => {
                val couple = given(characterIs(single), characterIs(single)).thereExists(compatible).apply(stateNarration.state).get

                val state1 = stateNarration.state |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._1)) |->> Character.spouse set Some(couple._2)
                val state2 = state1 |-> State.characterSet |->> index(stateNarration.state.characterSet.indexOf(couple._2)) |->> Character.spouse set Some(couple._1)

                Some(StateNarration(state2, Map("" -> Narration("", s"${couple._1.first} and ${couple._2.first} marry."))))
            })
        )



        lazy val marriageAndThenDivorce = Goal[StateNarration, Unit]("Marriage, then Divorce",
            alwaysFail,
            marriageAndThenDivorceAction)

        lazy val divorceGoal = Goal[StateNarration, Unit]("Divorce",
            alwaysFail,
            alwaysFail,
            alwaysFail,
            alwaysFail,
            alwaysFail,
            alwaysFail,
            divorceCauseAddiction,
            divorceCauseAffair1,
            divorceCauseAddiction,
            divorceCauseAffair2,
            divorceCauseDisapproval,
            divorceCauseDistance,
            divorceCauseWork)

        lazy val marriageAndThenDivorceAction = new Action[StateNarration, Unit]("Marriage, then Divorce",
           new  Subgoal[StateNarration, (Unit), (Unit)]((s, t) => { t }, marriageGoal),
            new Subgoal[StateNarration, (Unit), (Unit)]((s, t) => { t }, divorceGoal))

        new JFrameWithWebView("ui.html", new Server(state, Set(marriageGoal, divorceGoal, marriageAndThenDivorce)))
    }
}