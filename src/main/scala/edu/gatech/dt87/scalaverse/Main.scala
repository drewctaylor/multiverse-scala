package edu.gatech.dt87.scalaverse

import edu.gatech.dt87.scalaverse.story._
import edu.gatech.dt87.scalaverse.story.Predicate._
import edu.gatech.dt87.scalaverse.planner._
import edu.gatech.dt87.scalaverse.predicate.Predicate._

object Main {
    def main(args: Array[String]) {
        val story = Story(List(
            Character("Amanda", "Woodward", FEMALE, Set(MALE), 30),
            Character("Alison", "Parker", FEMALE, Set(MALE), 30),
            Character("Jane", "Andrews", FEMALE, Set(MALE), 30),
            Character("Jake", "Hanson", MALE, Set(MALE, FEMALE), 30),
            Character("Billy", "Campbell", MALE, Set(FEMALE), 30),
            Character("Matt", "Fielding", MALE, Set(MALE), 30),
            Character("Peter", "Burns", MALE, Set(FEMALE), 30),
            Character("Michael", "Mancini", MALE, Set(FEMALE), 30),
            Character("Sydney", "Andrews", FEMALE, Set(MALE, FEMALE), 30),
            Character("Kimberly", "Shaw", FEMALE, Set(MALE, FEMALE), 30)))

        lazy val goal : Goal[Story] = Goal(List(
            Fragment[Story](given(character).forAll(married).apply, story => {
                System.out.printf("Everyone lives happily ever after.")
                Some(story)
            }),
            Fragment[Story](given(characterIs(single), characterIs(single)).thereExists(compatible).apply, story => {
                val couple = given(characterIs(single), characterIs(single)).thereExists(compatible)(story).get
                couple._1.spouse = Some(couple._2);
                couple._2.spouse = Some(couple._1);
                System.out.printf("%s and %s marry.%n", couple._1.first, couple._2.first)
                goal(story)
            }),
            Fragment[Story](given(characterIs(single), characterIs(single)).forAll(incompatible).apply, story => {
                System.out.printf("Everyone lives happily ever after, except for: ")
                characterIs(single)(story).map(character => System.out.printf(character.first + " "));
                Some(story)
            })));

        goal(story);
    }
}