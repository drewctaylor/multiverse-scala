package edu.gatech.dt87.scalaverse.story.dsl

import edu.gatech.dt87.scalaverse.planner.Goal
import edu.gatech.dt87.scalaverse.story.attribute._
import edu.gatech.dt87.scalaverse.story.character.{Character, CharacterBidirectionalRelationship, CharacterUnidirectionalRelationship}
import edu.gatech.dt87.scalaverse.story.{StoryState, StoryStrategyStep}
import monocle.function._
import monocle.std._
import monocle.syntax._

import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

class Parser extends RegexParsers {

    sealed trait RelationshipDeclaration {
        def entityLeft: Symbol

        def entityRight: Symbol

        def modifyStatementList: List[ModifyStatement]
    }

    case class RelationshipBidirectionalDeclaration(entityLeft: Symbol, entityRight: Symbol, modifyStatementList: List[ModifyStatement]) extends RelationshipDeclaration

    case class RelationshipUnidirectionalDeclaration(entityLeft: Symbol, entityRight: Symbol, modifyStatementList: List[ModifyStatement]) extends RelationshipDeclaration

    sealed trait ModifyStatement {
        def field: FieldSpecification

        def value: ValueSpecification
    }

    case class UpdateStatement(field: FieldSpecification, value: ValueSpecification) extends ModifyStatement

    case class InsertStatement(field: FieldSpecification, value: ValueSpecification) extends ModifyStatement

    case class RemoveStatement(field: FieldSpecification, value: ValueSpecification) extends ModifyStatement

    sealed trait ValueSpecification {
        def valueCharacter(storyState: StoryState, that: Int, storyData: mutable.Map[Symbol, Int]): Option[Option[Set[Symbol]]]

        def valueBidirectionalRelationship(storyState: StoryState, that: (Int, Int), storyData: mutable.Map[Symbol, Int]): Option[Option[Set[Symbol]]]

        def valueUnidirectionalRelationship(storyState: StoryState, that: (Int, Int), storyData: mutable.Map[Symbol, Int]): Option[Option[Set[Symbol]]]
    }

    case class ValueSpecification0(symbol: Symbol) extends ValueSpecification {
        def valueCharacter(storyState: StoryState, that: Int, storyData: mutable.Map[Symbol, Int]): Option[Option[Set[Symbol]]] = {
            Some(Some(Set(symbol)))
        }

        def valueBidirectionalRelationship(storyState: StoryState, that: (Int, Int), storyData: mutable.Map[Symbol, Int]): Option[Option[Set[Symbol]]] = {
            Some(Some(Set(symbol)))
        }

        def valueUnidirectionalRelationship(storyState: StoryState, that: (Int, Int), storyData: mutable.Map[Symbol, Int]): Option[Option[Set[Symbol]]] = {
            Some(Some(Set(symbol)))
        }
    }


    case class ValueSpecification1(attribute: Symbol) extends ValueSpecification {
        def valueCharacter(storyState: StoryState, that: Int, storyData: mutable.Map[Symbol, Int]): Option[Option[Set[Symbol]]] = {
            storyState.applyLens(StoryState.focusCharacterMap)
                .composeTraversal(index(that))
                .composeTraversal(Character.focusAttributeValueMap)
                .composeTraversal(at(attribute))
                .headOption
        }

        def valueBidirectionalRelationship(storyState: StoryState, that: (Int, Int), storyData: mutable.Map[Symbol, Int]): Option[Option[Set[Symbol]]] = {
            storyState.applyLens(StoryState.focusRelationshipBidirectionalMap)
                .composeTraversal(index(that))
                .composeTraversal(CharacterBidirectionalRelationship.focusAttributeValueMap)
                .composeTraversal(at(attribute))
                .headOption
        }

        def valueUnidirectionalRelationship(storyState: StoryState, that: (Int, Int), storyData: mutable.Map[Symbol, Int]): Option[Option[Set[Symbol]]] = {
            storyState.applyLens(StoryState.focusRelationshipUnidirectionalMap)
                .composeTraversal(index(that))
                .composeTraversal(CharacterUnidirectionalRelationship.focusAttributeValueMap)
                .composeTraversal(at(attribute))
                .headOption
        }
    }

    case class ValueSpecification2(entity: Symbol, attribute: Symbol) extends ValueSpecification {
        def valueCharacter(storyState: StoryState, that: Int, storyData: mutable.Map[Symbol, Int]): Option[Option[Set[Symbol]]] = {
            storyState.applyLens(StoryState.focusCharacterMap)
                .composeTraversal(index(storyData(entity)))
                .composeTraversal(Character.focusAttributeValueMap)
                .composeTraversal(at(attribute))
                .headOption
        }

        def valueBidirectionalRelationship(storyState: StoryState, that: (Int, Int), storyData: mutable.Map[Symbol, Int]): Option[Option[Set[Symbol]]] = {
            throw new RuntimeException()
        }

        def valueUnidirectionalRelationship(storyState: StoryState, that: (Int, Int), storyData: mutable.Map[Symbol, Int]): Option[Option[Set[Symbol]]] = {
            throw new RuntimeException()
        }
    }

    case class ValueSpecification3(entityLeft: Symbol, entityRight: Symbol, attribute: Symbol) extends ValueSpecification {
        def valueCharacter(storyState: StoryState, that: Int, storyData: mutable.Map[Symbol, Int]): Option[Option[Set[Symbol]]] = {
            storyState.applyLens(StoryState.focusRelationshipBidirectionalMap)
                .composeTraversal(index((storyData(entityLeft), storyData(entityRight))))
                .composeTraversal(CharacterBidirectionalRelationship.focusAttributeValueMap)
                .composeTraversal(at(attribute))
                .headOption
        }

        def valueBidirectionalRelationship(storyState: StoryState, that: (Int, Int), storyData: mutable.Map[Symbol, Int]): Option[Option[Set[Symbol]]] = {
            throw new RuntimeException()
        }

        def valueUnidirectionalRelationship(storyState: StoryState, that: (Int, Int), storyData: mutable.Map[Symbol, Int]): Option[Option[Set[Symbol]]] = {
            throw new RuntimeException()
        }
    }


    case class ValueSpecification4(entityLeft: Symbol, entityRight: Symbol, attribute: Symbol) extends ValueSpecification {
        def valueCharacter(storyState: StoryState, that: Int, storyData: mutable.Map[Symbol, Int]): Option[Option[Set[Symbol]]] = {
            storyState.applyLens(StoryState.focusRelationshipUnidirectionalMap)
                .composeTraversal(index((storyData(entityLeft), storyData(entityRight))))
                .composeTraversal(CharacterUnidirectionalRelationship.focusAttributeValueMap)
                .composeTraversal(at(attribute))
                .headOption
        }

        def valueBidirectionalRelationship(storyState: StoryState, that: (Int, Int), storyData: mutable.Map[Symbol, Int]): Option[Option[Set[Symbol]]] = {
            throw new RuntimeException()
        }

        def valueUnidirectionalRelationship(storyState: StoryState, that: (Int, Int), storyData: mutable.Map[Symbol, Int]): Option[Option[Set[Symbol]]] = {
            throw new RuntimeException()
        }
    }

    sealed trait FieldSpecification {
        def modifyCharacter(storyState: StoryState, that: Int, storyData: mutable.Map[Symbol, Int], f: Option[Set[Symbol]] => Option[Set[Symbol]]): StoryState

        def modifyBidirectionalRelationship(storyState: StoryState, that: (Int, Int), storyData: mutable.Map[Symbol, Int], f: Option[Set[Symbol]] => Option[Set[Symbol]]): StoryState

        def modifyUnidirectionalRelationship(storyState: StoryState, that: (Int, Int), storyData: mutable.Map[Symbol, Int], f: Option[Set[Symbol]] => Option[Set[Symbol]]): StoryState
    }

    case class FieldSpecification1(attribute: Symbol) extends FieldSpecification {

        def modifyCharacter(storyState: StoryState, that: Int, storyData: mutable.Map[Symbol, Int], f: Option[Set[Symbol]] => Option[Set[Symbol]]): StoryState = {
            storyState.applyLens(StoryState.focusCharacterMap)
                .composeTraversal(index(that))
                .composeTraversal(Character.focusAttributeValueMap)
                .composeTraversal(at(attribute))
                .modify(f)
        }

        def modifyBidirectionalRelationship(storyState: StoryState, that: (Int, Int), storyData: mutable.Map[Symbol, Int], f: Option[Set[Symbol]] => Option[Set[Symbol]]): StoryState = {
            storyState.applyLens(StoryState.focusRelationshipBidirectionalMap)
                .composeTraversal(index(that))
                .composeTraversal(CharacterBidirectionalRelationship.focusAttributeValueMap)
                .composeTraversal(at(attribute))
                .modify(f)
        }

        def modifyUnidirectionalRelationship(storyState: StoryState, that: (Int, Int), storyData: mutable.Map[Symbol, Int], f: Option[Set[Symbol]] => Option[Set[Symbol]]): StoryState = {
            storyState.applyLens(StoryState.focusRelationshipUnidirectionalMap)
                .composeTraversal(index(that))
                .composeTraversal(CharacterUnidirectionalRelationship.focusAttributeValueMap)
                .composeTraversal(at(attribute))
                .modify(f)
        }
    }

    case class FieldSpecification2(entity: Symbol, attribute: Symbol) extends FieldSpecification {

        def modifyCharacter(storyState: StoryState, that: Int, storyData: mutable.Map[Symbol, Int], f: Option[Set[Symbol]] => Option[Set[Symbol]]): StoryState = {
            storyState.applyLens(StoryState.focusCharacterMap)
                .composeTraversal(index(that))
                .composeTraversal(Character.focusAttributeValueMap)
                .composeTraversal(at(attribute))
                .modify(f)
        }

        def modifyBidirectionalRelationship(storyState: StoryState, that: (Int, Int), storyData: mutable.Map[Symbol, Int], f: Option[Set[Symbol]] => Option[Set[Symbol]]): StoryState = {
            throw new RuntimeException()
        }

        def modifyUnidirectionalRelationship(storyState: StoryState, that: (Int, Int), storyData: mutable.Map[Symbol, Int], f: Option[Set[Symbol]] => Option[Set[Symbol]]): StoryState = {
            throw new RuntimeException()
        }
    }

    case class FieldSpecification3(entityLeft: Symbol, entityRight: Symbol, attribute: Symbol) extends FieldSpecification {

        def modifyCharacter(storyState: StoryState, that: Int, storyData: mutable.Map[Symbol, Int], f: Option[Set[Symbol]] => Option[Set[Symbol]]): StoryState = {
            storyState.applyLens(StoryState.focusRelationshipBidirectionalMap)
                .composeTraversal(index((storyData(entityLeft), storyData(entityRight))))
                .composeTraversal(CharacterBidirectionalRelationship.focusAttributeValueMap)
                .composeTraversal(at(attribute))
                .modify(f)
        }

        def modifyBidirectionalRelationship(storyState: StoryState, that: (Int, Int), storyData: mutable.Map[Symbol, Int], f: Option[Set[Symbol]] => Option[Set[Symbol]]): StoryState = {
            throw new RuntimeException()
        }

        def modifyUnidirectionalRelationship(storyState: StoryState, that: (Int, Int), storyData: mutable.Map[Symbol, Int], f: Option[Set[Symbol]] => Option[Set[Symbol]]): StoryState = {
            throw new RuntimeException()
        }
    }

    case class FieldSpecification4(entityLeft: Symbol, entityRight: Symbol, attribute: Symbol) extends FieldSpecification {

        def modifyCharacter(storyState: StoryState, that: Int, storyData: mutable.Map[Symbol, Int], f: Option[Set[Symbol]] => Option[Set[Symbol]]): StoryState = {
            storyState.applyLens(StoryState.focusRelationshipUnidirectionalMap)
                .composeTraversal(index((storyData(entityLeft), storyData(entityRight))))
                .composeTraversal(CharacterUnidirectionalRelationship.focusAttributeValueMap)
                .composeTraversal(at(attribute))
                .modify(f)
        }

        def modifyBidirectionalRelationship(storyState: StoryState, that: (Int, Int), storyData: mutable.Map[Symbol, Int], f: Option[Set[Symbol]] => Option[Set[Symbol]]): StoryState = {
            throw new RuntimeException()
        }

        def modifyUnidirectionalRelationship(storyState: StoryState, that: (Int, Int), storyData: mutable.Map[Symbol, Int], f: Option[Set[Symbol]] => Option[Set[Symbol]]): StoryState = {
            throw new RuntimeException()
        }
    }

    val characterIndex = Iterator.from(0)

    val goalMap = scala.collection.mutable.Map[String, Goal[StoryState, StoryStrategyStep.StoryData, StoryStrategyStep.StoryData]]()

    val nameDoubleQuote = """"[\p{Alnum}\p{Punct} &&[^"]]*"""".r

    def template = """"[\p{Alnum}\p{Punct} &&[^"]]*"""".r

    //    def endStatement = ";"
    //
    //    def goalKeyword = "goal" | "g"
    //
    //    def goalDeclaration = goalDeclarationWithoutIdentifier | goalDeclarationWithIdentifier
    //
    //    def goalDeclarationWithoutIdentifier = goalKeyword ~ opt(nameDoubleQuote) ~ beginBlock ~ rep1(strategyDeclaration) ~ endBlock ^^ {
    //        case keyword ~ name ~ begin ~ strategyDeclarationList ~ end => name match {
    //            case None => Goal(strategyDeclarationList.toSeq: _*)
    //            case Some(_) => Goal(strategyDeclarationList.toSeq: _*)
    //        }
    //    }
    //
    //    def goalDeclarationWithIdentifier = goalKeyword ~ identifier ~ opt(nameDoubleQuote) ~ beginBlock ~ rep1(strategyDeclaration) ~ endBlock ^^ {
    //        case keyword ~ g ~ name ~ begin ~ strategyDeclarationList ~ end => name match {
    //            case None => Goal(strategyDeclarationList.toSeq: _*)
    //            case Some(n) => {
    //                val goal = Goal(n, strategyDeclarationList.toSeq: _*)
    //                goalMap.update(g, goal)
    //                goalMap(g)
    //            }
    //        }
    //    }
    //
    //    def strategyKeyword = "strategy" | "s"
    //
    //    def strategyDeclaration = strategyKeyword ~ rep(identifier) ~ opt(nameDoubleQuote) ~ beginBlock ~ rep1(eventStatement) ~ endBlock ^^ {
    //        case strategy ~ identifierList ~ name ~ begin ~ eventList ~ end => name match {
    //            case None => Strategy((StoryStrategyStep.formal(identifierList) +: eventList).reduce[StrategyStep[StoryState, StoryStrategyStep.StoryData, StoryStrategyStep.StoryData]]((sss, ss) => sss merge ss))
    //            case Some(_) => Strategy((StoryStrategyStep.formal(identifierList) +: eventList).reduce[StrategyStep[StoryState, StoryStrategyStep.StoryData, StoryStrategyStep.StoryData]]((sss, ss) => sss merge ss))
    //        }
    //    }
    //
    //    def eventStatement = (updateStatement | insertStatement | removeStatement) ^^ {
    //        case UpdateStatement(FieldSpecification1(s), value) => null
    //        case UpdateStatement(FieldSpecification2(e, a), value) => null
    //        case UpdateStatement(FieldSpecification3(el, er, a), value) => null
    //        case UpdateStatement(FieldSpecification4(el, er, a), value) => null
    //        case InsertStatement(FieldSpecification1(s), value) => null
    //        case InsertStatement(FieldSpecification2(e, a), value) => null
    //        case InsertStatement(FieldSpecification3(el, er, a), value) => null
    //        case InsertStatement(FieldSpecification4(el, er, a), value) => null
    //        case RemoveStatement(FieldSpecification1(s), value) => null
    //        case RemoveStatement(FieldSpecification2(e, a), value) => null
    //        case RemoveStatement(FieldSpecification3(el, er, a), value) => null
    //        case RemoveStatement(FieldSpecification4(el, er, a), value) => null
    //    }
    //
    //        def subgoalKeyword = "subgoal"
    //
    //        def subgoalStatement = subgoalKeyword ~ identifier ~ rep(identifier) ~ endStatement ^^ {
    //            case subgoal ~ n ~ parameterList ~ end => {
    //                StoryStrategyStep.subgoal(new Goal[StoryState, StoryStrategyStep.StoryData, StoryStrategyStep.StoryData]() {
    //                    def name: String = {
    //                        n
    //                    }
    //
    //                    def strategySet: Set[Strategy[StoryState, StoryStrategyStep.StoryData, StoryStrategyStep.StoryData]] = {
    //                        goalMap(n).strategySet
    //                    }
    //
    //                    def satisfy(state: StoryState, input: StoryStrategyStep.StoryData): GoalExecution[StoryState, StoryStrategyStep.StoryData, StoryStrategyStep.StoryData] = {
    //                        goalMap(n).satisfy(state, input)
    //                    }
    //                }, parameterList)
    //            }
    //        }
    //
    //    def narrateKeyword = "narrate" | "n"
    //
    //    def narrateStatement = narrateKeyword ~ template ~ endStatement ^^ {
    //        case narrate ~ t ~ end => StoryStrategyStep.narrate(t)
    //    }
    //
    //    def findKeyword = "find" | "f"
    //
    //    def whereKeyword = "where" | "w"
    //
    //    def predicate = isNotPredicate | relationshipDoesNotContainPredicate | relationshipContainsPredicate | containsPredicate | isPredicate | isAttributePredicate
    //
    //    def isNotPredicate = identifier ~ "isnt" ~ identifier ^^ {
    //        case entity1 ~ isnt ~ entity2 => StoryStatePredicate.isNot(entity1, entity2)
    //    }
    //
    //    def isPredicate = identifier ~ "is" ~ identifier ^^ {
    //        case entity1 ~ is ~ entity2 => StoryStatePredicate.is(entity1, entity2)
    //    }
    //
    //    def isAttributePredicate = identifier ~ identifier ~ "is" ~ identifier ^^ {
    //        case entity ~ attribute ~ is ~ symbol => StoryStatePredicate.is(entity, attribute, symbol)
    //    }
    //
    //    def relationshipDoesNotContainPredicate = "relationship" ~ identifier ~ identifier ~ identifier ~ "does" ~ "not" ~ "contain" ~ identifier ^^ {
    //        case r ~ e1 ~ e2 ~ a ~ d ~ n ~ c ~ s => StoryStatePredicate.relationshipDoesNotContain(e1, e2, a, s)
    //    }
    //
    //    def containsPredicate = identifier ~ identifier ~ "contains" ~ identifier ~ identifier ^^ {
    //        case e1 ~ a1 ~ c ~ e2 ~ a2 => StoryStatePredicate.contains(e1, a1, e2, a2)
    //    }
    //
    //    def relationshipContainsPredicate = "relationship" ~ identifier ~ identifier ~ identifier ~ "contains" ~ identifier ^^ {
    //        case relationship ~ e1 ~ e2 ~ a ~ c ~ s => StoryStatePredicate.relationshipContains(e1, e2, a, s)
    //    }
    //
    //    def findStatement = findKeyword ~ identifier ~ identifier ~ whereKeyword ~ rep(predicate) ~ endStatement ^^ {
    //        case find ~ e1 ~ e2 ~ where ~ predicateList ~ end => StoryStrategyStep.findCharacterCharacter(e1, e2, predicateList)
    //    }
    //
    //    def findStatement1 = findKeyword ~ identifier ~ whereKeyword ~ rep(predicate) ~ endStatement ^^ {
    //        case find ~ e1 ~ where ~ predicateList ~ end => StoryStrategyStep.findCharacter(e1, predicateList)
    //    }
    //
    //    def noneKeyword = "none"
    //
    //    def noneStatement2 = noneKeyword ~ identifier ~ identifier ~ whereKeyword ~ rep(predicate) ~ endStatement ^^ {
    //        case none ~ e1 ~ e2 ~ where ~ predicateList ~ end => StoryStrategyStep.noCharacterCharacter(e1, e2, predicateList)
    //    }

    def statementSet = stateDeclaration

    val identifier = """[\p{Alnum}-]+""".r

    val symbol = """"[\p{Alnum}\p{Punct} &&[^"]]*"""".r

    def beginBlock = "{"

    def endBlock = "}"

    // state

    def stateKeyword = "state" | "s"

    def stateDeclaration = stateKeyword ~ beginBlock ~ rep(attributeDeclaration) ~ rep(characterDeclaration) ~ rep(relationshipUnidirectionalDeclaration | relationshipBidirectionalDeclaration) ~ endBlock ^^ {
        case s ~ bb ~ attributeList ~ characterList ~ relationshipList ~ eb => {

            def fUpsert[E](e: E): (Option[E]) => Option[E] = (oe) => oe match {
                case Some(e) => oe
                case None => Some(e)
            }

            val fUpdate: Option[Set[Symbol]] => Option[Set[Symbol]] => Option[Set[Symbol]] = symbolSetUpdate => {
                symbolSet => {
                    symbolSetUpdate
                }
            }
            val fInsert: Option[Set[Symbol]] => Option[Set[Symbol]] => Option[Set[Symbol]] = symbolSetInsert => {
                symbolSetInitial => {
                    for (i <- symbolSetInitial.orElse(Some(Set[Symbol]()));
                         n <- symbolSetInsert.orElse(Some(Set[Symbol]()))) yield i ++ n
                }
            }

            val fRemove: Option[Set[Symbol]] => (Option[Set[Symbol]]) => Option[Set[Symbol]] = symbolSetRemove => {
                symbolSetInitial => {
                    for (i <- symbolSetInitial.orElse(Some(Set[Symbol]()));
                         r <- symbolSetRemove.orElse(Some(Set[Symbol]()))) yield i -- r
                }
            }

            val map = mutable.Map[Symbol, Int]()

            val storyStateWithAttributeMap = attributeList.foldLeft(StoryState())((storyState, attribute) => {
                storyState.applyLens(StoryState.focusAttributeMap).composeTraversal(at(attribute.name)).set(Some(attribute))
            })

            val storyStateWithCharacterMap = characterList.foldLeft(storyStateWithAttributeMap)((storyState, tuple: (Symbol, List[ModifyStatement with Product with Serializable])) => {
                map += (tuple._1 -> characterIndex.next())

                tuple._2.foldLeft(storyState.applyLens(StoryState.focusCharacterMap).composeTraversal(at(map(tuple._1))).modify(fUpsert(Character())))((storyState, statement) => {
                    statement match {
                        case UpdateStatement(field, value) => field.modifyCharacter(storyState, map(tuple._1), map, fUpdate(value.valueCharacter(storyState, map(tuple._1), map).get))
                        case InsertStatement(field, value) => field.modifyCharacter(storyState, map(tuple._1), map, fInsert(value.valueCharacter(storyState, map(tuple._1), map).get))
                        case RemoveStatement(field, value) => field.modifyCharacter(storyState, map(tuple._1), map, fRemove(value.valueCharacter(storyState, map(tuple._1), map).get))
                    }
                })
            })

            val storyStateWithRelationshipMap = relationshipList.foldLeft(storyStateWithCharacterMap)((storyState, relationshipDeclaration) => {
                relationshipDeclaration match {
                    case RelationshipUnidirectionalDeclaration(el, er, modifyStatementList) => {
                        modifyStatementList.foldLeft(storyState.applyLens(StoryState.focusRelationshipUnidirectionalMap).composeTraversal(at(map(el), map(er))).modify(fUpsert(CharacterUnidirectionalRelationship((map(el), map(er))))))((storyState, statement) => statement match {
                            case UpdateStatement(field, value) => field.modifyUnidirectionalRelationship(storyState, (map(el), map(er)), map, fUpdate(value.valueUnidirectionalRelationship(storyState, (map(el), map(er)), map).get))
                            case InsertStatement(field, value) => field.modifyUnidirectionalRelationship(storyState, (map(el), map(er)), map, fInsert(value.valueUnidirectionalRelationship(storyState, (map(el), map(er)), map).get))
                            case RemoveStatement(field, value) => field.modifyUnidirectionalRelationship(storyState, (map(el), map(er)), map, fRemove(value.valueUnidirectionalRelationship(storyState, (map(el), map(er)), map).get))
                        })
                    }
                    case RelationshipBidirectionalDeclaration(elo, ero, modifyStatementList) => {
                        val tuple = if (map(elo) < map(ero)) {
                            (map(elo), map(ero))
                        } else {
                            (map(ero), map(elo))
                        }

                        modifyStatementList.foldLeft(storyState.applyLens(StoryState.focusRelationshipBidirectionalMap).composeTraversal(at(tuple)).modify(fUpsert(CharacterBidirectionalRelationship(tuple))))((storyState, statement) => statement match {
                            case UpdateStatement(field, value) => field.modifyBidirectionalRelationship(storyState, tuple, map, fUpdate(value.valueUnidirectionalRelationship(storyState, tuple, map).get))
                            case InsertStatement(field, value) => field.modifyBidirectionalRelationship(storyState, tuple, map, fInsert(value.valueUnidirectionalRelationship(storyState, tuple, map).get))
                            case RemoveStatement(field, value) => field.modifyBidirectionalRelationship(storyState, tuple, map, fRemove(value.valueUnidirectionalRelationship(storyState, tuple, map).get))
                        })
                    }

                }
            })

            storyStateWithRelationshipMap
        }
    }

    // attribute

    def attributeDeclaration = attributeOneSymbolDeclaration | attributeOneUnorderedDeclaration | attributeOneOrderedDeclaration | attributeSubsetSymbolDeclaration | attributeSubsetUnorderedDeclaration | attributeSubsetOrderedDeclaration

    def attributeKeyword = "attribute" | "a"

    def attributeOneSymbol = "one symbol" | "os"

    def attributeOneSymbolDeclaration = attributeKeyword ~ identifier ~ attributeOneSymbol ^^ {
        case a ~ name ~ ss => AttributeOneSymbol(Symbol(name))
    }

    def attributeOneUnorderedKeyword = "one of unordered set" | "ou"

    def attributeOneUnorderedDeclaration = attributeKeyword ~ identifier ~ attributeOneUnorderedKeyword ~ beginBlock ~ rep1(attributeValue) ~ endBlock ^^ {
        case a ~ name ~ ou ~ bb ~ validSequence ~ eb => AttributeOneUnordered(Symbol(name), validSequence)
    }

    def attributeOneOrderedKeyword = "one of ordered set" | "oo"

    def attributeOneOrderedDeclaration = attributeKeyword ~ identifier ~ attributeOneOrderedKeyword ~ beginBlock ~ rep1(attributeValue) ~ endBlock ^^ {
        case a ~ name ~ oo ~ bb ~ validSequence ~ eb => AttributeOneOrdered(Symbol(name), validSequence)
    }

    def attributeSubsetSymbol = "subset of symbols" | "ss"

    def attributeSubsetSymbolDeclaration = attributeKeyword ~ identifier ~ attributeSubsetSymbol ^^ {
        case a ~ name ~ ss => AttributeSubsetSymbol(Symbol(name))
    }

    def attributeSubsetUnorderedKeyword = "subset of unordered set" | "su"

    def attributeSubsetUnorderedDeclaration = attributeKeyword ~ identifier ~ attributeSubsetUnorderedKeyword ~ beginBlock ~ rep1(attributeValue) ~ endBlock ^^ {
        case a ~ name ~ su ~ bb ~ validSequence ~ eb => AttributeSubsetUnordered(Symbol(name), validSequence)
    }

    def attributeSubsetOrderedKeyword = "subset of ordered set" | "so"

    def attributeSubsetOrderedDeclaration = attributeKeyword ~ identifier ~ attributeSubsetOrderedKeyword ~ beginBlock ~ rep1(attributeValue) ~ endBlock ^^ {
        case a ~ name ~ so ~ bb ~ validSequence ~ eb => AttributeSubsetOrdered(Symbol(name), validSequence)
    }

    def attributeValue = symbol ^^ {
        case s => Symbol(s.substring(1, s.length - 1))
    }

    // character

    def characterKeyword = "character" | "c"

    def characterDeclaration = characterKeyword ~ identifier ~ beginBlock ~ rep(modifyStatement) ~ endBlock ^^ {
        case c ~ name ~ bb ~ modifyStatementList ~ eb => Symbol(name) -> modifyStatementList
    }

    // relationship

    def relationshipKeyword = "relationship" | "r"

    def relationshipUnidirectionalDeclaration = relationshipKeyword ~ identifier ~ "<->" ~ identifier ~ beginBlock ~ rep(modifyStatement) ~ endBlock ^^ {
        case r ~ el ~ c ~ er ~ bb ~ modifyStatementList ~ eb => RelationshipBidirectionalDeclaration(Symbol(el), Symbol(er), modifyStatementList)
    }

    def relationshipBidirectionalDeclaration = relationshipKeyword ~ identifier ~ "->" ~ identifier ~ beginBlock ~ rep(modifyStatement) ~ endBlock ^^ {
        case r ~ el ~ c ~ er ~ bb ~ modifyStatementList ~ eb => RelationshipUnidirectionalDeclaration(Symbol(el), Symbol(er), modifyStatementList)
    }

    // modify

    def modifyStatement = updateStatement | insertStatement | removeStatement

    def updateStatement = fieldClause ~ ":=" ~ valueClause ^^ {
        case f ~ o ~ v => UpdateStatement(f, v)
    }

    def insertStatement = fieldClause ~ "+=" ~ valueClause ^^ {
        case f ~ o ~ v => InsertStatement(f, v)
    }

    def removeStatement = fieldClause ~ "-=" ~ valueClause ^^ {
        case f ~ o ~ v => RemoveStatement(f, v)
    }

    // field clause

    def fieldClause = field1Clause | field2Clause | field3Clause

    def field1Clause = identifier ^^ {
        case a => FieldSpecification1(Symbol(a))
    }

    def field2Clause = identifier ~ "." ~ identifier ^^ {
        case e ~ d ~ a => FieldSpecification2(Symbol(e), Symbol(a))
    }

    def field3Clause = identifier ~ "<->" ~ identifier ~ "." ~ identifier ^^ {
        case el ~ c ~ er ~ d ~ a => FieldSpecification3(Symbol(el), Symbol(er), Symbol(a))
    }

    def field4Clause = identifier ~ "->" ~ identifier ~ "." ~ identifier ^^ {
        case el ~ c ~ er ~ d ~ a => FieldSpecification4(Symbol(el), Symbol(er), Symbol(a))
    }

    // value clause

    def valueClause = value0Clause | value2Clause | value1Clause | value3Clause | value4Clause

    def value0Clause = symbol ^^ {
        case s => ValueSpecification0(Symbol(s.substring(1, s.length - 1)))
    }

    def value1Clause = identifier ^^ {
        case a => ValueSpecification1(Symbol(a))
    }

    def value2Clause = identifier ~ "." ~ identifier ^^ {
        case e ~ d ~ a => ValueSpecification2(Symbol(e), Symbol(a))
    }

    def value3Clause = identifier ~ "<->" ~ identifier ~ "." ~ identifier ^^ {
        case el ~ c ~ er ~ d ~ a => ValueSpecification3(Symbol(el), Symbol(er), Symbol(a))
    }

    def value4Clause = identifier ~ "->" ~ identifier ~ "." ~ identifier ^^ {
        case el ~ c ~ er ~ d ~ a => ValueSpecification4(Symbol(el), Symbol(er), Symbol(a))
    }

    def parse(text: String): Option[StoryState] = {
        parseAll(statementSet, text) match {
            case Success(storyState, next) => Some(storyState)
            case NoSuccess(storyState, next) => {
                println(storyState, next)
                None
            }
        }
    }
}

object Test extends Parser with App {
    val story = io.Source.fromInputStream(getClass.getResourceAsStream("Sample.story")).mkString


    parse(story.stripMargin) match {
        case Some(storyState) => {
            System.out.println(storyState)
            //            val state0 = new StoryState(cast.map((c: Character) => (c.id -> c)).toMap, map)
            //
            //            val storyData = Map[String, Int]()
            //            var lasttext = ""
            //            var state = state0
            //
            //            for (i <- 1 to 100) {
            //                val ctx = marriage.satisfy(state, storyData)
            //                Fabula.fabula(ctx).foreach(eventExecution => eventExecution.successor.get._1.narration match {
            //                    case None => ;
            //                    case Some(s) => {
            //                        if (s == lasttext) {
            //                        } else {
            //                            System.out.println(s)
            //                            lasttext = s
            //                        }
            //                    }
            //                })
            //                state = ctx.successor().get._1
            //            }
        }
    }
}

//goal {
//strategy {
//find c1 c2 where
//c1 isnt c2
//c1 orientation contains c2 gender
//c2 orientation contains c1 gender
//relationship c1 c2 status does not contain marriage;
//subgoal alive c1;
//subgoal alive c2;
//subgoal single c1;
//subgoal single c2;
//c1 c2 status += marriage
//narrate "{{c1.first.iterator.next}} and {{c2.first.iterator.next}} marry.";
//}
//}
//
//goal alive "Alive" {
//strategy c "Alive - No Op" {
//find c1 where
//c1 is c
//c1 life is dead;
//c1 life := alive;
//narrate "{{c1.first.iterator.next}} returns to Melrose Place, very much alive.";
//}
//strategy c "Alive - Alive" {
//find c1 where
//c1 is c
//c1 life is alive;
//narrate "{{c1.first.iterator.next}} is alive.";
//}
//}
//
//goal single "Single" {
//strategy c "Single - Divorce" {
//find c1 c2 where
//c1 is c
//c1 isnt c2
//relationship c1 c2 status contains marriage;
//remove marriage from c1 c2 status;
//narrate "{{c1.first.iterator.next}} and {{c2.first.iterator.next}} divorce.";
//}
//strategy c "Single - Death" {
//find c1 c2 where
//c1 is c
//c1 isnt c2
//relationship c1 c2 status contains marriage;
//c2 life := dead;
//remove marriage from c1 c2 status;
//narrate "{{c2.first.iterator.next}} dies, leaving {{c1.first.iterator.next}} alone.";
//}
//strategy c "Single - Noop" {
//none c1 c2 where
//c1 is c
//c1 isnt c2
//relationship c1 c2 status contains marriage;
//narrate "{{c.first.iterator.next}} is single.";
//}
//}