//package edu.gatech.dt87.scalaverse
//
//import edu.gatech.dt87.scalaverse.planner._
//import edu.gatech.dt87.scalaverse.predicate.Predicate._
//import edu.gatech.dt87.scalaverse.prettyPrinter.PrettyPrinter
//import edu.gatech.dt87.scalaverse.random._
//import edu.gatech.dt87.scalaverse.story._
//import edu.gatech.dt87.scalaverse.story.character._
//import monocle.Optional
//import monocle.function._
//import monocle.std._
//import monocle.syntax._
//
//object Main {
//    def main(args: Array[String]) {
//        var state = StateNarration(StoryState(List(
//            Character("Amanda", "Woodward", FEMININE, Set(MASCULINE), 30),
//            Character("Alison", "Parker", FEMININE, Set(MASCULINE), 30),
//            Character("Jane", "Andrews", FEMININE, Set(MASCULINE), 30),
//            Character("Jake", "Hanson", MASCULINE, Set(MASCULINE, FEMININE), 30),
//            Character("Billy", "Campbell", MASCULINE, Set(FEMININE), 30),
//            Character("Matt", "Fielding", MASCULINE, Set(MASCULINE), 30),
//            Character("Peter", "Burns", MASCULINE, Set(FEMININE), 30),
//            Character("Michael", "Mancini", MASCULINE, Set(FEMININE), 30),
//            Character("Sydney", "Andrews", FEMININE, Set(MASCULINE, FEMININE), 30),
//            Character("Kimberly", "Shaw", FEMININE, Set(MASCULINE, FEMININE), 30))), Map("" -> Narration("", "Once upon a time . . .")))
//
//        def eventPrecondition[Y](precondition: (StateNarration, Y) => Boolean): Event[StateNarration, Y, Y] = {
//            Event[StateNarration, Y, Y]((x: StateNarration, y: Y) => {
//                if (precondition(x, y)) Some(StateNarration(x.state, Map(""->Narration("",""))), y) else None
//            })
//        }
//
//        def eventNarration[Y](f: Y => String): Event[StateNarration, Y, Y] = {
//            Event[StateNarration, Y, Y]((stateNarration: StateNarration, y: Y) => {
//                Some(StateNarration(stateNarration.state, Map("" -> Narration("", f(y)))), y)
//            })
//        }
//
//        def eventNoop[Y](): Event[StateNarration, Y, Unit] = {
//            Event[StateNarration, Y, Unit]((stateNarration: StateNarration, y: Y) => Some(StateNarration(stateNarration.state, Map("" -> Narration("", ""))), ()))
//        }
//
//        def eventBind[Y, Z](f: (StoryState, Y) => Z): Event[StateNarration, Y, Z] = {
//            Event[StateNarration, Y, Z]((stateNarration : StateNarration, y: Y) => {
//                Some(StateNarration(stateNarration.state, Map("" -> Narration("", ""))), f(stateNarration.state, y))
//            })
//        }
//
//        def eventChangeState[Y](f: (StoryState, Y) => StoryState) : Event[StateNarration, Y, Y] = {
//            Event[StateNarration, Y, Y]((stateNarration : StateNarration, y : Y) => {
//                Some(StateNarration(f(stateNarration.state, y), Map("" -> Narration("", ""))), y)
//            })
//        }
//
//        def eventCharacterAlive(): Event[StateNarration, Character, Character] = {
//            Event[StateNarration, Character, Character]((stateNarration: StateNarration, character: Character) => {
//                Some(StateNarration(stateNarration.state |-> StoryState.characterSetLens |->> index(stateNarration.state.characterSet.indexOf(stateNarration.state.characterSet.find(_.id==character.id).get)) |->> Character.life set ALIVE, Map("" -> Narration("", ""))), character)
//            })
//        }
//
//        def eventCharacterDead(): Event[StateNarration, Character, Character] = {
//            Event[StateNarration, Character, Character]((stateNarration: StateNarration, character: Character) => {
//                Some(StateNarration(stateNarration.state |-> StoryState.characterSetLens |->> index(stateNarration.state.characterSet.indexOf(stateNarration.state.characterSet.find(_.id==character.id).get)) |->> Character.life set DEAD, Map("" -> Narration("", ""))), character)
//            })
//        }
//
//        val goalLife = Goal[StateNarration, Character, Unit]("Life",
//            new Strategy[StateNarration, Character, Unit]("Life - No Operation",
//                eventPrecondition[Character]((stateNarration, character) => character.life == ALIVE) merge
//                    eventNarration[Character](t => s"${t.first} lies by the pool. ${t.life}") merge
//                    eventNoop[Character]()
//            ),
//            new Strategy[StateNarration, Character, Unit]("Life - Revive",
//                eventPrecondition[Character]((stateNarration, character) => character.life == DEAD) merge
//                    eventCharacterAlive() merge
//                    eventNarration[Character](t => s"${t.first} returns to Melrose Place, very much alive.") merge
//                    eventNoop[Character]()
//            )
//        )
//
//        val goalSingle = Goal[StateNarration, Character, Unit]("Single",
//            new Strategy[StateNarration, Character, Unit]("Single - No Operation",
//                eventPrecondition[Character]((stateNarration, character) => character.spouse == None) merge
//                    eventNarration[Character](t => s"${t.first} lies by the pool, dreaming of love.") merge
//                    eventNoop[Character]()
//            ),
//            new Strategy[StateNarration, Character, Unit]("Single - By Death",
//                eventPrecondition[Character]((stateNarration, character) => character.spouse.isDefined) merge
//                    eventBind[Character, (Character, Character)]((state, character) => (character, state.characterSet.find(spouse => spouse.id == character.spouse.get).get)) merge
//                    eventChangeState[(Character, Character)]((state, y) => state |-> StoryState.characterSetLens |->> index(state.characterSet.indexOf(state.characterSet.find(_.id==y._1.id).get)) |->> Character.spouse set None) merge
//                    eventChangeState[(Character, Character)]((state, y) => state |-> StoryState.characterSetLens |->> index(state.characterSet.indexOf(state.characterSet.find(_.id==y._2.id).get)) |->> Character.spouse set None) merge
//                    eventChangeState[(Character, Character)]((state, y) => {
//                        state |-> StoryState.characterSetLens |->> index(state.characterSet.indexOf(state.characterSet.find(_.id == y._2.id).get)) |->> Character.life set DEAD
//                    }) merge
//                    eventNarration[(Character, Character)](t => s"${t._2.first} dies, leaving ${t._1.first} alone. ${t._2.life}") merge
//                    eventNoop[(Character, Character)]()
//            ),
//            new Strategy[StateNarration, Character, Unit]("Single - By Divorce",
//                eventPrecondition[Character]((stateNarration, character) => character.spouse.isDefined) merge
//                    eventBind[Character, (Character, Character)]((state, character) => (character, state.characterSet.find(spouse => spouse.id == character.spouse.get).get)) merge
//                    eventChangeState[(Character, Character)]((state, y) => state |-> StoryState.characterSetLens |->> index(state.characterSet.indexOf(state.characterSet.find(_.id==y._1.id).get)) |->> Character.spouse set None) merge
//                    eventChangeState[(Character, Character)]((state, y) => state |-> StoryState.characterSetLens |->> index(state.characterSet.indexOf(state.characterSet.find(_.id==y._2.id).get)) |->> Character.spouse set None) merge
//                    eventNarration[(Character, Character)](t => s"${t._2.first} and ${t._1.first} divorce.") merge
//                    eventNoop[(Character, Character)]()
//            )
//        )
//
//        def tIn1(stateNarration: StateNarration, couple: (Character, Character)): Character = couple._1
//        def tIn2(stateNarration: StateNarration, couple: (Character, Character)): Character = couple._2
//        def tOut(stateNarration: StateNarration, couple: (Character, Character), output: Unit): (Character, Character) = couple
//
//        val goalMarriage = new Goal[StateNarration, (Character, Character), Unit]("Marriage, Given Two Characters",
//            Set(new Strategy[StateNarration, (Character, Character), Unit]("Marriage, Given Two Characters",
//                Subgoal[StateNarration, (Character, Character), Character, Unit, (Character, Character)](tIn1 _, goalLife, tOut _) merge
//                    Subgoal[StateNarration, (Character, Character), Character, Unit, (Character, Character)](tIn2 _, goalLife, tOut _) merge
//                    Subgoal[StateNarration, (Character, Character), Character, Unit, (Character, Character)](tIn1 _, goalSingle, tOut _) merge
//                    Subgoal[StateNarration, (Character, Character), Character, Unit, (Character, Character)](tIn2 _, goalSingle, tOut _) merge
//                    eventChangeState[(Character, Character)]((state, y) => state |-> StoryState.characterSetLens |->> index(state.characterSet.indexOf(state.characterSet.find(_.id==y._1.id).get)) |->> Character.spouse set Some(y._2.id)) merge
//                    eventChangeState[(Character, Character)]((state, y) => state |-> StoryState.characterSetLens |->> index(state.characterSet.indexOf(state.characterSet.find(_.id==y._2.id).get)) |->> Character.spouse set Some(y._1.id)) merge
//                    eventNarration[(Character, Character)](t => s"${t._2.first} and ${t._1.first} marry.") merge
//                    eventNoop[(Character, Character)]()
//            ))
//        )
//
//        def tInP(stateNarration: StateNarration, input: Unit): (Character, Character) = given(CharacterPredicate.character, CharacterPredicate.character).thereExists((character1, character2) => {
//            CharacterPredicate.compatible(character1, character2) && !CharacterPredicate.areMarried(character1, character2)
//        })(stateNarration.state).get
//
//        def tOutP(stateNarration: StateNarration, input: Unit, output: Unit): Unit = Unit
//
//        val goalMarriageTop = new Goal[StateNarration, Unit, Unit]("Marriage",
//            Set(new Strategy[StateNarration, Unit, Unit]("Marriage",
//                Subgoal[StateNarration, Unit, (Character, Character), Unit, Unit](tInP _, goalMarriage, tOutP _)
//            )))
//
//        val goalSet = Set(goalMarriageTop)
//        var goalExecution = Random.shuffle(Planner.satisfiableGoalSet(state, (), goalSet).toSeq).head.satisfy(state, ())
//
//        for (i <- 1 to 100) {
//            System.out.println("")
//            System.out.println(PrettyPrinter.print(goalExecution))
//            Fabula.fabula(goalExecution).foreach((eventExecution: EventExecution[StateNarration, _, _]) => {
//                if(eventExecution.successor.get._1.narrationMap("").text.length() > 0) System.out.println(eventExecution.successor.get._1.narrationMap("").text)
//            })
//            System.out.println(goalExecution.successor())
//            System.out.println("executing goal")
//
//            goalExecution = Random.shuffle(Planner.satisfiableGoalSet(goalExecution.successor().get._1, (), goalSet).toSeq).head.satisfy(goalExecution.successor().get._1, ())
//        }
//
//        val lens = StoryState.characterSetLens |->> index(1) : monocle.Traversal[StoryState, StoryState, Character, Character]
//        val narrationState = state.state |->> lens |->> Character.first set "Drew"
//        System.out.println(narrationState)
//    }
//}