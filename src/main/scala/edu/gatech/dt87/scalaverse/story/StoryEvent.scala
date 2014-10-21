package edu.gatech.dt87.scalaverse.story

import com.gilt.handlebars.scala.Handlebars
import com.gilt.handlebars.scala.binding.Binding
import com.gilt.handlebars.scala.binding.dynamic._
import edu.gatech.dt87.scalaverse.planner.{Event, Goal, Subgoal}
import edu.gatech.dt87.scalaverse.predicate.Predicate
import edu.gatech.dt87.scalaverse.story.character.Character
import monocle.function._
import monocle.std._
import monocle.syntax._

object StoryEvent {
    type StoryData = Map[String, Int]
    type StoryPredicate = StoryData => StoryState => Boolean

    def narrate(text: String): Event[StoryState, StoryData, StoryData] = {
        Event((state, entityMap) => {
            val f : Int => Map[String, Set[String]] = state.characterMap(_).attributeValueMap
            val m = entityMap mapValues f
            val t = Handlebars[Any](text)

            Some(state |-> StoryState.focusNarration set Some(t(DynamicBinding(m))), entityMap)
        })
    }

    def findCharacterCharacter(entity1: String, entity2: String, predicate: List[StoryPredicate]): Event[StoryState, StoryData, StoryData] = {
        Event((state, entityMap) => {
            val f0: StoryState => List[Character] = state => state.characterMap.values.toList
            Predicate.given(f0, f0).thereExists((e1, e2) => {
                predicate.forall(predicate => predicate(entityMap + (entity1 -> e1.id) + (entity2 -> e2.id))(state))
            })(state) match {
                case None => None
                case Some(t) => Some(state, entityMap + (entity1 -> t._1.id) + (entity2 -> t._2.id))
            }
        })
    }

    def noCharacterCharacter(entity1: String, entity2: String, predicate: Seq[StoryPredicate]): Event[StoryState, StoryData, StoryData] = {
        Event((state, entityMap) => {
            val f0: StoryState => List[Character] = state => state.characterMap.values.toList
            Predicate.given(f0, f0).forAll((e1, e2) => {
                predicate.forall(predicate => predicate(entityMap + (entity1 -> e1.id) + (entity2 -> e2.id))(state))
            })(state) match {
                case None => Some(state, entityMap)
                case Some(t) => None
            }
        })
    }

    def findCharacter(entity: String, predicate: Seq[StoryPredicate]): Event[StoryState, StoryData, StoryData] = {
        Event((state, entityMap) => {
            val f0: StoryState => List[Character] = state => state.characterMap.values.toList
            Predicate.given(f0, f0).thereExists((e1, e2) => {
                predicate.forall(predicate => predicate(entityMap + (entity -> e1.id))(state))
            })(state) match {
                case None => None
                case Some(t) => Some(state, entityMap + (entity -> t._1.id))
            }
        })
    }

    def isRelationship(entity1: String, entity2: String, attribute: String, symbol: String): StoryPredicate = {
        (entityMap) => {
            (storyState) => {
                val index1 = entityMap(entity1)
                val index2 = entityMap(entity2)
                if (index1 < index2) {
                    storyState.relationshipMap(entityMap(entity1), entityMap(entity2))(attribute).contains(symbol)
                } else {
                    storyState.relationshipMap(entityMap(entity2), entityMap(entity1))(attribute).contains(symbol)
                }
            }
        }
    }

    def is(entity: String, attribute: String, symbol: String): StoryPredicate = {
        (entityMap) => {
            (storyState) => {
                storyState.characterMap(entityMap(entity)).attributeValueMap(attribute).contains(symbol)
            }
        }
    }

    def is(entity1: String, entity2: String): StoryPredicate = {
        (entityMap) => {
            (storyState) => {
                storyState.characterMap(entityMap(entity1)).id == storyState.characterMap(entityMap(entity2)).id
            }
        }
    }

    def isNot(entity1: String, entity2: String): StoryPredicate = {
        (entityMap) => {
            (storyState) => {
                storyState.characterMap(entityMap(entity1)).id != storyState.characterMap(entityMap(entity2)).id
            }
        }
    }

    def contains(entity: String, attribute: String, symbol: String): StoryPredicate = {
        (entityMap) => {
            (storyState) => {
                storyState.characterMap(entityMap(entity)).attributeValueMap(attribute).contains(symbol)
            }
        }
    }

    def contains(entity1: String, attribute1: String, entity2: String, attribute2: String): StoryPredicate = {
        (entityMap) => {
            (storyState) => {
                storyState.characterMap(entityMap(entity1)).attributeValueMap(attribute1).contains(storyState.characterMap(entityMap(entity2)).attributeValueMap(attribute2).iterator.next())
            }
        }
    }

    def relationshipContains(entity1: String, entity2: String, attribute: String, symbol: String): StoryPredicate = {
        (entityMap) => {
            (storyState) => {
                val index1 = entityMap(entity1)
                val index2 = entityMap(entity2)
                if (index1 < index2) {
                    storyState.relationshipMap(entityMap(entity1), entityMap(entity2))(attribute).contains(symbol)
                } else {
                    storyState.relationshipMap(entityMap(entity2), entityMap(entity1))(attribute).contains(symbol)
                }
            }
        }
    }

    def relationshipDoesNotContain(entity1: String, entity2: String, attribute: String, symbol: String): StoryPredicate = {
        (entityMap) => {
            (storyState) => {
                val index1 = entityMap(entity1)
                val index2 = entityMap(entity2)
                if (index1 < index2) {
                    !storyState.relationshipMap(entityMap(entity1), entityMap(entity2))(attribute).contains(symbol)
                } else {
                    !storyState.relationshipMap(entityMap(entity2), entityMap(entity1))(attribute).contains(symbol)
                }
            }
        }
    }

    def updateRelationship(entity1: String, entity2: String, attribute: String, symbol: String): Event[StoryState, StoryData, StoryData] = {
        val f: (Option[Set[String]]) => Option[Set[String]] = (set) => Some(Set(symbol))

        Event((state, entityMap) => {
            val index1 = entityMap(entity1)
            val index2 = entityMap(entity2)
            if (index1 < index2) {
                Some(state |-> StoryState.focusRelationshipMap |->> index(entityMap(entity1), entityMap(entity2)) |->> at(attribute) modify f, entityMap)
            } else {
                Some(state |-> StoryState.focusRelationshipMap |->> index(entityMap(entity2), entityMap(entity1)) |->> at(attribute) modify f, entityMap)
            }
        })
    }

    def update(entity: String, attribute: String, symbol: String): Event[StoryState, StoryData, StoryData] = {
        val f: (Option[Set[String]]) => Option[Set[String]] = (set) => Some(Set(symbol))

        Event((state, entityMap) => Some(state |-> StoryState.focusCharacterMap |->> index(entityMap(entity)) |->> Character.focusAttributeValueMap |->> at(attribute) modify f, entityMap))
    }

    def insertRelationship(entity1: String, entity2: String, attribute: String, symbol: String): Event[StoryState, StoryData, StoryData] = {
        val f: (Option[Set[String]]) => Option[Set[String]] = (set) => set map (_ + symbol)

        Event((state, entityMap) => {
            val index1 = entityMap(entity1)
            val index2 = entityMap(entity2)
            if (index1 < index2) {
                Some(state |-> StoryState.focusRelationshipMap |->> index(entityMap(entity1), entityMap(entity2)) |->> at(attribute) modify f, entityMap)
            } else {
                Some(state |-> StoryState.focusRelationshipMap |->> index(entityMap(entity2), entityMap(entity1)) |->> at(attribute) modify f, entityMap)
            }
        })
    }

    def insert(entity: String, attribute: String, symbol: String): Event[StoryState, StoryData, StoryData] = {
        val f: (Option[Set[String]]) => Option[Set[String]] = (set) => set map (_ + symbol)

        Event((state, entityMap) => Some(state |-> StoryState.focusCharacterMap |->> index(entityMap(entity)) |->> Character.focusAttributeValueMap |->> at(attribute) modify f, entityMap))
    }

    def removeRelationship(entity1: String, entity2: String, attribute: String, symbol: String): Event[StoryState, StoryData, StoryData] = {
        val f: (Option[Set[String]]) => Option[Set[String]] = (set) => set map (_ - symbol)

        Event((state, entityMap) => {
            val index1 = entityMap(entity1)
            val index2 = entityMap(entity2)
            if (index1 < index2) {
                Some(state |-> StoryState.focusRelationshipMap |->> index(entityMap(entity1), entityMap(entity2)) |->> at(attribute) modify f, entityMap)
            } else {
                Some(state |-> StoryState.focusRelationshipMap |->> index(entityMap(entity2), entityMap(entity1)) |->> at(attribute) modify f, entityMap)
            }
        })
    }

    def remove(entity: String, attribute: String, symbol: String): Event[StoryState, StoryData, StoryData] = {
        val f: (Option[Set[String]]) => Option[Set[String]] = (set) => set map (_ - symbol)

        Event((state, entityMap) => Some(state |-> StoryState.focusCharacterMap |->> index(entityMap(entity)) |->> Character.focusAttributeValueMap |->> at(attribute) modify f, entityMap))
    }

    def log(): Event[StoryState, StoryData, StoryData] = {
        Event((state, entityMap) => {
            Some(state, entityMap)
        })
    }

    def end(): Event[StoryState, StoryData, Unit] = {
        Event((state, entityMap) => Some(state, ()))
    }

    def formal(formalList : List[String]) : Event[StoryState, StoryData, StoryData] = {
        Event((state, entityMap) => {

            val formalMap = (formalList.indices.map(_.toString) zip formalList).toMap
            val entityMap2 = entityMap.map(t => (formalMap.get(t._1).get, t._2)).toMap

            Some(state, entityMap2)
        })
    }

    def subgoal(goal: Goal[StoryState, StoryData, StoryData], actualList : List[String]): Subgoal[StoryState, StoryData, StoryData, StoryData, StoryData] = {
        val actualMap = (actualList zip actualList.indices.map(_.toString)).toMap
        val f0 = (t: (String, Int)) => (actualMap(t._1), t._2)

        val f1 = (state: StoryState, inputEntityMap: StoryData) => {
            inputEntityMap.filterKeys(actualList.toSet).map(f0)
        }
        val f2 = (state: StoryState, inputEntityMap: StoryData, _: StoryData) => inputEntityMap

        Subgoal(f1, goal, f2)
    }
}