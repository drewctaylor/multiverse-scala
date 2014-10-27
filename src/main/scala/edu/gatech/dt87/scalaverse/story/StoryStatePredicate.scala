//package edu.gatech.dt87.scalaverse.story
//
//import edu.gatech.dt87.scalaverse.story.StoryStrategyStep.StoryData
//
//object StoryStatePredicate {
//
//    def lt(el : String, al : String, er : String, ar : String): StoryData => StoryState => Boolean = { null }
//    def lte(el : String, al : String, er : String, ar : String): StoryData => StoryState => Boolean = { null }
//    def gt(el : String, al : String, er : String, ar : String): StoryData => StoryState => Boolean = { null }
//    def gte(el : String, al : String, er : String, ar : String): StoryData => StoryState => Boolean = { null }
//    def eq(el : String, al : String, er : String, ar : String): StoryData => StoryState => Boolean = { null }
//    def neq(el : String, al : String, er : String, ar : String): StoryData => StoryState => Boolean = { null }
//    def in(el : String, al : String, er : String, ar : String): StoryData => StoryState => Boolean = { null }
//    def nin(el : String, al : String, er : String, ar : String): StoryData => StoryState => Boolean = { null }
//
//    def isRelationship(entity1: String, entity2: String, attribute: String, symbol: String): StoryData => StoryState => Boolean = {
//        (entityMap) => {
//            (storyState) => {
//                val index1 = entityMap(entity1)
//                val index2 = entityMap(entity2)
//                if (index1 < index2) {
//                    storyState.relationshipMap(entityMap(entity1), entityMap(entity2))(attribute).contains(symbol)
//                } else {
//                    storyState.relationshipMap(entityMap(entity2), entityMap(entity1))(attribute).contains(symbol)
//                }
//            }
//        }
//    }
//
//    def is(entity: String, attribute: String, symbol: String): StoryData => StoryState => Boolean = {
//        (entityMap) => {
//            (storyState) => {
//                storyState.characterMap(entityMap(entity)).attributeValueMap(attribute).contains(symbol)
//            }
//        }
//    }
//
//    def is(entity1: String, entity2: String): StoryData => StoryState => Boolean = {
//        (entityMap) => {
//            (storyState) => {
//                storyState.characterMap(entityMap(entity1)).id == storyState.characterMap(entityMap(entity2)).id
//            }
//        }
//    }
//
//    def isNot(entity1: String, entity2: String): StoryData => StoryState => Boolean = {
//        (entityMap) => {
//            (storyState) => {
//                storyState.characterMap(entityMap(entity1)).id != storyState.characterMap(entityMap(entity2)).id
//            }
//        }
//    }
//
//    def contains(entity: String, attribute: String, symbol: String): StoryData => StoryState => Boolean = {
//        (entityMap) => {
//            (storyState) => {
//                storyState.characterMap(entityMap(entity)).attributeValueMap(attribute).contains(symbol)
//            }
//        }
//    }
//
//    def contains(entity1: String, attribute1: String, entity2: String, attribute2: String): StoryData => StoryState => Boolean = {
//        (entityMap) => {
//            (storyState) => {
//                storyState.characterMap(entityMap(entity1)).attributeValueMap(attribute1).contains(storyState.characterMap(entityMap(entity2)).attributeValueMap(attribute2).iterator.next())
//            }
//        }
//    }
//
//    def relationshipContains(entity1: String, entity2: String, attribute: String, symbol: String): StoryData => StoryState => Boolean = {
//        (entityMap) => {
//            (storyState) => {
//                val index1 = entityMap(entity1)
//                val index2 = entityMap(entity2)
//                if (index1 < index2) {
//                    storyState.relationshipMap(entityMap(entity1), entityMap(entity2))(attribute).contains(symbol)
//                } else {
//                    storyState.relationshipMap(entityMap(entity2), entityMap(entity1))(attribute).contains(symbol)
//                }
//            }
//        }
//    }
//
//    def relationshipDoesNotContain(entity1: String, entity2: String, attribute: String, symbol: String): StoryData => StoryState => Boolean = {
//        (entityMap) => {
//            (storyState) => {
//                val index1 = entityMap(entity1)
//                val index2 = entityMap(entity2)
//                if (index1 < index2) {
//                    !storyState.relationshipMap(entityMap(entity1), entityMap(entity2))(attribute).contains(symbol)
//                } else {
//                    !storyState.relationshipMap(entityMap(entity2), entityMap(entity1))(attribute).contains(symbol)
//                }
//            }
//        }
//    }
//}
