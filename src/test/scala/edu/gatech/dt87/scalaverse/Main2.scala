package edu.gatech.dt87.scalaverse

import edu.gatech.dt87.scalaverse.planner.{Goal, Strategy}
import edu.gatech.dt87.scalaverse.prettyPrinter.PrettyPrinter
import edu.gatech.dt87.scalaverse.story.character._
import edu.gatech.dt87.scalaverse.story.{Fabula, StoryEvent, StoryState}

/**
 * Created by dt87 on 10/18/14.
 */
object Main2 extends App {

    lazy val marriage = Goal(
        Strategy(
            StoryEvent.findCharacterCharacter("c1", "c2", List(
                StoryEvent.contains("c1", "orientation", "c2", "gender"),
                StoryEvent.contains("c2", "orientation", "c1", "gender"),
                StoryEvent.relationshipDoesNotContain("c1", "c2", "status", "marriage"),
                StoryEvent.isNot("c1", "c2")
            )) merge
                StoryEvent.subgoal(alive, List("c1")) merge
                StoryEvent.subgoal(alive, List("c2")) merge
                StoryEvent.subgoal(single, List("c1")) merge
                StoryEvent.subgoal(single, List("c2")) merge
                StoryEvent.insertRelationship("c1", "c2", "status", "marriage") merge
                StoryEvent.narrate("{{c1.first.iterator.next}} and {{c2.first.iterator.next}} marry.")
        )
    )

    lazy val alive = Goal("Alive",
        Strategy("Alive - Alive",
            StoryEvent.formal(List("c")) merge
            StoryEvent.findCharacter("c1", List(
                StoryEvent.is("c1", "c"),
                StoryEvent.is("c1", "life", "dead")
            )) merge
                StoryEvent.update("c", "life", "alive") merge
                StoryEvent.narrate("{{c1.first.iterator.next}} returns to Melrose Place, very much alive.")
        ),
        Strategy("Alive - No Operation",
            StoryEvent.formal(List("c")) merge
            StoryEvent.findCharacter("c1", List(
                StoryEvent.is("c1", "c"),
                StoryEvent.is("c1", "life", "alive")
            )) merge
                StoryEvent.narrate("{{c1.first}} is alive.")
        )
    )

    lazy val single = Goal("Single",
        Strategy("Single - By Divorce",
            StoryEvent.formal(List("c")) merge
            StoryEvent.findCharacterCharacter("c1", "c2", List(
                StoryEvent.is("c1", "c"),
                StoryEvent.isNot("c1", "c2"),
                StoryEvent.relationshipContains("c1", "c2", "status", "marriage")
            )) merge
                StoryEvent.removeRelationship("c1", "c2", "status", "marriage") merge
                StoryEvent.narrate("{{c1.first.iterator.next}} and {{c2.first.iterator.next}} divorce.")
        ),
        Strategy("Single - By Death",
            StoryEvent.formal(List("c")) merge
            StoryEvent.findCharacterCharacter("c1", "c2", List(
                StoryEvent.is("c1", "c"),
                StoryEvent.isNot("c1", "c2"),
                StoryEvent.relationshipContains("c1", "c2", "status", "marriage")
            )) merge
                StoryEvent.removeRelationship("c1", "c2", "status", "marriage") merge
                StoryEvent.update("c2", "life", "dead") merge
                StoryEvent.narrate("{{c2.first.iterator.next}} dies, leaving {{c1.first.iterator.next}} alone.")
        ),
        Strategy("Single - No Operation",
            StoryEvent.formal(List("c")) merge
            StoryEvent.noCharacterCharacter("c1", "c2", List(
                StoryEvent.is("c1", "c"),
                StoryEvent.isNot("c1", "c2"),
                StoryEvent.relationshipContains("c1", "c2", "status", "marriage")
            )) merge
                StoryEvent.narrate("{{c.first.iterator.next}} is single.")
        )
    )

    val c1 = new Character(1, Map("first" -> Set("Matt"), "gender" -> Set("male"), "orientation" -> Set("male"), "life" -> Set("dead")))
    val c2 = new Character(2, Map("first" -> Set("Alison"), "gender" -> Set("female"), "orientation" -> Set("male"), "life" -> Set("dead")))
    val c3 = new Character(3, Map("first" -> Set("Jake"), "gender" -> Set("male"), "orientation" -> Set("female", "male"), "life" -> Set("dead")))
    val state0 = new StoryState(Map(1 -> c1, 2 -> c2, 3 -> c3), Map(
        (1, 1) -> Map("status" -> Set()),
        (1, 2) -> Map("status" -> Set()),
        (1, 3) -> Map("status" -> Set()),
        (2, 1) -> Map("status" -> Set()),
        (2, 2) -> Map("status" -> Set()),
        (2, 3) -> Map("status" -> Set()),
        (3, 3) -> Map("status" -> Set())
    ))

    val map = Map[String, Int]()

    val ctx1 = marriage.satisfy(state0, map)
    Fabula.fabula(ctx1).foreach(eventExecution => System.out.println(eventExecution.successor.get._1.narration))

    val state1 = ctx1.successor().get._1
    val ctx2 = marriage.satisfy(state1, map)
    Fabula.fabula(ctx2).foreach(eventExecution => System.out.println(eventExecution.successor.get._1.narration))

    val state2 = ctx2.successor().get._1
    val ctx3 = marriage.satisfy(state2, map)
    Fabula.fabula(ctx3).foreach(eventExecution => System.out.println(eventExecution.successor.get._1.narration))

    val state3 = ctx3.successor().get._1

//    System.out.println(state0)
//    System.out.println(PrettyPrinter.print(ctx1))
//    System.out.println(state1)
//    System.out.println(PrettyPrinter.print(ctx2))
//    System.out.println(state2)
//    System.out.println(PrettyPrinter.print(ctx3))
//    System.out.println(state3)
}
