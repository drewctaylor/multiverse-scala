package edu.gatech.dt87.scalaverse.story

import com.gilt.handlebars.scala.Handlebars
import com.gilt.handlebars.scala.binding.dynamic._
import edu.gatech.dt87.scalaverse.planner.{Event, Goal, Subgoal}
import edu.gatech.dt87.scalaverse.predicate.Predicate
import edu.gatech.dt87.scalaverse.story.character.Character
import monocle.function._
import monocle.std._
import monocle.syntax._

object StoryStrategyStep {
    type StoryData = Map[Symbol, Int]

//    def narrate(text: Symbol): Event[StoryState, StoryData, StoryData] = {
//        Event((state, entityMap) => {
//            val f: Int => Map[Symbol, Set[Symbol]] = state.characterMap(_).attributeValueMap
//            val m = entityMap mapValues f
//            val t = Handlebars[Any](text)
//
//            Some(state |-> StoryState.focusNarration set Some(t(DynamicBinding(m))), entityMap)
//        })
//    }
//
//    def findCharacterCharacter(entity1: Symbol, entity2: Symbol, predicate: List[StoryData => StoryState => Boolean]): Event[StoryState, StoryData, StoryData] = {
//        Event((state, entityMap) => {
//            val f0: StoryState => List[Character] = state => state.characterMap.values.toList
//            Predicate.given(f0, f0).thereExists((e1, e2) => {
//                predicate.forall(predicate => predicate(entityMap + (entity1 -> e1.id) + (entity2 -> e2.id))(state))
//            })(state) match {
//                case None => None
//                case Some(t) => Some(state, entityMap + (entity1 -> t._1.id) + (entity2 -> t._2.id))
//            }
//        })
//    }
//
//    def noCharacterCharacter(entity1: Symbol, entity2: Symbol, predicate: Seq[StoryData => StoryState => Boolean]): Event[StoryState, StoryData, StoryData] = {
//        Event((state, entityMap) => {
//            val f0: StoryState => List[Character] = state => state.characterMap.values.toList
//            Predicate.given(f0, f0).forAll((e1, e2) => {
//                predicate.forall(predicate => predicate(entityMap + (entity1 -> e1.id) + (entity2 -> e2.id))(state))
//            })(state) match {
//                case None => Some(state, entityMap)
//                case Some(t) => None
//            }
//        })
//    }
//
//    def findCharacter(entity: Symbol, predicate: Seq[StoryData => StoryState => Boolean]): Event[StoryState, StoryData, StoryData] = {
//        Event((state, entityMap) => {
//            val f0: StoryState => List[Character] = state => state.characterMap.values.toList
//            Predicate.given(f0, f0).thereExists((e1, e2) => {
//                predicate.forall(predicate => predicate(entityMap + (entity -> e1.id))(state))
//            })(state) match {
//                case None => None
//                case Some(t) => Some(state, entityMap + (entity -> t._1.id))
//            }
//        })
//    }

    def log(): Event[StoryState, StoryData, StoryData] = {
        Event((state, entityMap) => {
            Some(state, entityMap)
        })
    }
//
//    def formal(formalList: List[Symbol]): Event[StoryState, StoryData, StoryData] = {
//        Event((state, entityMap) => {
//
//            val formalMap = (formalList.indices.map(_.toSymbol) zip formalList).toMap
//            val entityMap2 = entityMap.map(t => (formalMap.get(t._1).get, t._2)).toMap
//
//            Some(state, entityMap2)
//        })
//    }
//
//    def subgoal(goal: Goal[StoryState, StoryData, StoryData], actualList: List[Symbol]): Subgoal[StoryState, StoryData, StoryData, StoryData, StoryData] = {
//        val actualMap = (actualList zip actualList.indices.map(_.toSymbol)).toMap
//        val f0 = (t: (Symbol, Int)) => (actualMap(t._1), t._2)
//
//        val f1 = (state: StoryState, inputEntityMap: StoryData) => {
//            inputEntityMap.filterKeys(actualList.toSet).map(f0)
//        }
//        val f2 = (state: StoryState, inputEntityMap: StoryData, _: StoryData) => inputEntityMap
//
//        Subgoal(f1, goal, f2)
//    }

    def update(e: Symbol, a: Symbol, s: Symbol): Event[StoryState, StoryData, StoryData] = {
        val f: (Option[Set[Symbol]]) => Option[Set[Symbol]] = (set) => Some(Set(s))
        modify(e, a, s, f)
    }

//    def updateRelationship(el: Symbol, er: Symbol, a: Symbol, s: Symbol): Event[StoryState, StoryData, StoryData] = {
//        val f: (Option[Set[Symbol]]) => Option[Set[Symbol]] = (set) => Some(Set(s))
//        modify(el, er, a, s, f)
//    }

    def insert(e: Symbol, a: Symbol, s: Symbol): Event[StoryState, StoryData, StoryData] = {
        val f: (Option[Set[Symbol]]) => Option[Set[Symbol]] = (set) => set map (_ + s)
        modify(e, a, s, f)
    }

//    def insertRelationship(el: Symbol, er: Symbol, a: Symbol, s: Symbol): Event[StoryState, StoryData, StoryData] = {
//        val f: (Option[Set[Symbol]]) => Option[Set[Symbol]] = (set) => set map (_ + s)
//        modify(el, er, a, s, f)
//    }

    def remove(e: Symbol, a: Symbol, s: Symbol): Event[StoryState, StoryData, StoryData] = {
        val f: (Option[Set[Symbol]]) => Option[Set[Symbol]] = (set) => set map (_ - s)
        modify(e, a, s, f)
    }

//    def removeRelationship(el: Symbol, er: Symbol, a: Symbol, s: Symbol): Event[StoryState, StoryData, StoryData] = {
//        val f: (Option[Set[Symbol]]) => Option[Set[Symbol]] = (set) => set map (_ - s)
//        modify(el, er, a, s, f)
//    }

    def modify(e: Symbol, a: Symbol, s: Symbol, modifier: (Option[Set[Symbol]]) => Option[Set[Symbol]]): Event[StoryState, StoryData, StoryData] = {
        Event((state, entityMap) => modify(e, a, s, modifier, state, entityMap))
    }

//    def modify(el: Symbol, er: Symbol, a: Symbol, s: Symbol, modifier: (Option[Set[Symbol]]) => Option[Set[Symbol]]): Event[StoryState, StoryData, StoryData] = {
//        Event((state, entityMap) => {
//            val i1 = entityMap(el)
//            val i2 = entityMap(er)
//            if (i1 < i2) {
//                modify(el, er, a, s, modifier, state, entityMap)
//            } else {
//                modify(er, el, a, s, modifier, state, entityMap)
//            }
//        })
//    }

    def modify(e: Symbol, a: Symbol, s: Symbol, modifier: (Option[Set[Symbol]]) => Option[Set[Symbol]], state: StoryState, entityMap: StoryData): Option[(StoryState, StoryData)] = {
        Some(state.applyLens(StoryState.focusCharacterMap).composeTraversal(index(entityMap(e))).composeTraversal(Character.focusAttributeValueMap).composeTraversal(at(a)).modify(modifier), entityMap)
    }

    //
    //    def modifyFromAttribute(el: Symbol, al: Symbol, er: Symbol, ar: Symbol, modifier :  (Option[Set[Symbol]]) => Option[Set[Symbol]], state : StoryState, entityMap: StoryData): Option[(StoryState, StoryData)] = {
    //        state.characterMap(entityMap(er)).attributeValueMap(ar)
    //        Some(state.applyLens(StoryState.focusCharacterMap).composeTraversal(index(entityMap(el))).composeTraversal(Character.focusAttributeValueMap).composeTraversal(at(al)).modify(modifier), entityMap)
    //    }

//    def modify(el: Symbol, er: Symbol, a: Symbol, s: Symbol, modifier: (Option[Set[Symbol]]) => Option[Set[Symbol]], state: StoryState, entityMap: StoryData): Option[(StoryState, StoryData)] = {
//        Some(state.applyLens(StoryState.focusRelationshipMap).composeTraversal(index(entityMap(el), entityMap(er))).composeTraversal(at(a)).modify(modifier), entityMap)
//    }
}
