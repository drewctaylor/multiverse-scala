package edu.gatech.dt87.multiverse.story

import monocle.Lenser
import monocle.function._
import monocle.std._
import monocle.syntax._

case class StoryEntity(attributeMap: Map[Symbol, Set[Symbol]] = Map())

case class StoryEntitySet(entityMap: Map[Int, StoryEntity] = Map())

case class StoryState(entitySetMap: Map[Symbol, StoryEntitySet] = Map(),
                      relationshipUnidirectionalMap: Map[((Symbol, Int), (Symbol, Int)), StoryEntity] = Map(),
                      relationshipBidirectionalMap: Map[((Symbol, Int), (Symbol, Int)), StoryEntity] = Map(),
                      narration: Option[String] = None)

object StoryState {
    val focusFactory = Lenser[StoryState]
    val focusEntitySetMap = focusFactory(_.entitySetMap)
    val focusRelationshipUnidirectionalMap = focusFactory(_.relationshipUnidirectionalMap)
    val focusRelationshipBidirectionalMap = focusFactory(_.relationshipBidirectionalMap)
    val focusNarration = focusFactory(_.narration)

    def insertEntityAttribute(storyState: StoryState, entity: (Symbol, Int), attribute: Symbol, symbolSet: Option[Set[Symbol]]): StoryState = {
        storyState.applyLens(StoryState.focusEntitySetMap)
            .composeTraversal(index(entity._1))
            .composeTraversal(StoryEntitySet.focusEntityMap)
            .composeTraversal(index(entity._2))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .modify(symbolSetOld => for (sso <- symbolSetOld; ss <- symbolSet) yield sso ++ ss)
    }

    def updateEntityAttribute(storyState: StoryState, entity: (Symbol, Int), attribute: Symbol, symbolSet: Option[Set[Symbol]]): StoryState = {
        storyState.applyLens(StoryState.focusEntitySetMap)
            .composeTraversal(index(entity._1))
            .composeTraversal(StoryEntitySet.focusEntityMap)
            .composeTraversal(index(entity._2))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .set(symbolSet)
    }

    def removeEntityAttribute(storyState: StoryState, entity: (Symbol, Int), attribute: Symbol, symbolSet: Option[Set[Symbol]]): StoryState = {
        storyState.applyLens(StoryState.focusEntitySetMap)
            .composeTraversal(index(entity._1))
            .composeTraversal(StoryEntitySet.focusEntityMap)
            .composeTraversal(index(entity._2))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .modify(symbolSetOld => for (sso <- symbolSetOld; ss <- symbolSet) yield sso -- ss)
    }

    def insertRelationshipUnidirectionalAttribute(storyState: StoryState, left: (Symbol, Int), right: (Symbol, Int), attribute: Symbol, symbolSet: Option[Set[Symbol]]): StoryState = {
        storyState.applyLens(StoryState.focusRelationshipUnidirectionalMap)
            .composeTraversal(index((left, right)))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .modify(symbolSetOld => for (sso <- symbolSetOld; ss <- symbolSet) yield sso ++ ss)
    }

    def updateRelationshipUnidirectionalAttribute(storyState: StoryState, left: (Symbol, Int), right: (Symbol, Int), attribute: Symbol, symbolSet: Option[Set[Symbol]]): StoryState = {
        storyState.applyLens(StoryState.focusRelationshipUnidirectionalMap)
            .composeTraversal(index((left, right)))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .set(symbolSet)
    }

    def removeRelationshipUnidirectionalAttribute(storyState: StoryState, left: (Symbol, Int), right: (Symbol, Int), attribute: Symbol, symbolSet: Option[Set[Symbol]]): StoryState = {
        storyState.applyLens(StoryState.focusRelationshipUnidirectionalMap)
            .composeTraversal(index((left, right)))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .modify(symbolSetOld => for (sso <- symbolSetOld; ss <- symbolSet) yield sso -- ss)
    }

    def ordered(left: (Symbol, Int), right: (Symbol, Int)) : ((Symbol, Int), (Symbol, Int)) = {
        val l = if (left._1.name > right._1.name) right else if (left._1.name == right._1.name && left._2 > right._2) right else left
        val r = if (left._1.name > right._1.name) left  else if (left._1.name == right._1.name && left._2 > right._2) left  else right

        (l, r)
    }

    def insertRelationshipBidirectionalAttribute(storyState: StoryState, left: (Symbol, Int), right: (Symbol, Int), attribute: Symbol, symbolSet: Option[Set[Symbol]]): StoryState = {
        storyState.applyLens(StoryState.focusRelationshipBidirectionalMap)
            .composeTraversal(index(ordered(left, right)))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .modify(symbolSetOld => for (sso <- symbolSetOld; ss <- symbolSet) yield sso ++ ss)
    }

    def updateRelationshipBidirectionalAttribute(storyState: StoryState, left: (Symbol, Int), right: (Symbol, Int), attribute: Symbol, symbolSet: Option[Set[Symbol]]): StoryState = {
        storyState.applyLens(StoryState.focusRelationshipBidirectionalMap)
            .composeTraversal(index(ordered(left, right)))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .set(symbolSet)
    }

    def removeRelationshipBidirectionalAttribute(storyState: StoryState, left: (Symbol, Int), right: (Symbol, Int), attribute: Symbol, symbolSet: Option[Set[Symbol]]): StoryState = {
        storyState.applyLens(StoryState.focusRelationshipBidirectionalMap)
            .composeTraversal(index(ordered(left, right)))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .modify(symbolSetOld => for (sso <- symbolSetOld; ss <- symbolSet) yield sso -- ss)
    }
}

object StoryEntitySet {
    val focusFactory = Lenser[StoryEntitySet]
    val focusEntityMap = focusFactory(_.entityMap)
}

object StoryEntity {
    val focusFactory = Lenser[StoryEntity]
    val focusAttributeMap = focusFactory(_.attributeMap)
}


/**
 * Extension methods for story entities.
 */
object StoryEntityExtension {

    /**
     * Extension methods that return the capitalized and uncapitalized subject, object, and possessive pronouns and determiners for a story entity.
     *
     * @param storyEntity a story entity
     */
    implicit class Pronoun(storyEntity: StoryEntity) {

        /**
         * @return given a story entity, return the uncapitalized subject pronoun for that story entity's gender
         */
        def sub: String = storyEntity.attributeMap('gender).toSeq match {
            case Seq('male) => "he"
            case Seq('female) => "she"
            case Seq('neuter) => "it"
        }

        /**
         * @return given a story entity, return the capitalized subject pronoun for that story entity's gender
         */
        def Sub: String = sub.capitalize

        /**
         * @return given a story entity, return the uncapitalized object pronoun for that story entity's gender
         */
        def obj: String = storyEntity.attributeMap('gender).toSeq match {
            case Seq('male) => "him"
            case Seq('female) => "her"
            case Seq('neuter) => "it"
        }

        /**
         * @return given a story entity, return the capitalized object pronoun for that story entity's gender
         */
        def Obj: String = obj.capitalize

        /**
         * @return given a story entity, return the uncapitalized possessive pronoun for that story entity's gender
         */
        def pos: String = storyEntity.attributeMap('gender).toSeq match {
            case Seq('male) => "his"
            case Seq('female) => "hers"
            case Seq('neuter) => "its"
        }

        /**
         * @return given a story entity, return the capitalized possessive pronoun for that story entity's gender
         */
        def Pos: String = pos.capitalize

        /**
         * @return given a story entity, return the uncapitalized determiner for that story entity's gender
         */
        def det: String = storyEntity.attributeMap('gender).toSeq match {
            case Seq('male) => "his"
            case Seq('female) => "her"
            case Seq('neuter) => "its"
        }

        /**
         * @return given a story entity, return the capitalized determiner for that story entity's gender
         */
        def Det: String = det.capitalize
    }

}

