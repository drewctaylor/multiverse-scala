package edu.gatech.dt87.scalaverse.story.dsl

import edu.gatech.dt87.scalaverse.planner.{Goal, GoalExecution, Strategy, StrategyStep}
import edu.gatech.dt87.scalaverse.story.character.Character
import edu.gatech.dt87.scalaverse.story.{Fabula, StoryEvent, StoryState}

import scala.util.parsing.combinator.RegexParsers

class Parser extends RegexParsers {

    val goalMap = scala.collection.mutable.Map[String, Goal[StoryState, StoryEvent.StoryData, StoryEvent.StoryData]]()

    def identifier = """\p{Alnum}+""".r

    def nameDoubleQuote = """"[\p{Alnum}\p{Punct} &&[^"]]*"""".r

    def template = """"[\p{Alnum}\p{Punct} &&[^"]]*"""".r

    def beginBlock = "{"

    def endBlock = "}"

    def endStatement = ";"

    def goalKeyword = "goal" | "g"

    def goalDeclarationWithoutIdentifier = goalKeyword ~ opt(nameDoubleQuote) ~ beginBlock ~ rep1(strategyDeclaration) ~ endBlock ^^ {
        case keyword ~ name ~ begin ~ strategyDeclarationList ~ end => name match {
            case None => Goal(strategyDeclarationList.toSeq: _*)
            case Some(_) => Goal(strategyDeclarationList.toSeq: _*)
        }
    }

    def goalDeclarationWithIdentifier = goalKeyword ~ identifier ~ opt(nameDoubleQuote) ~ beginBlock ~ rep1(strategyDeclaration) ~ endBlock ^^ {
        case keyword ~ g ~ name ~ begin ~ strategyDeclarationList ~ end => name match {
            case None => Goal(strategyDeclarationList.toSeq: _*)
            case Some(n) => {
                val goal = Goal(n, strategyDeclarationList.toSeq: _*)
                goalMap.update(g, goal)
                goalMap(g)
            }
        }
    }

    def strategyKeyword = "strategy" | "s"

    def strategyDeclaration = strategyKeyword ~ rep(identifier) ~ opt(nameDoubleQuote) ~ beginBlock ~ rep1(eventStatement) ~ endBlock ^^ {
        case strategy ~ identifierList ~ name ~ begin ~ eventList ~ end => name match {
            case None => Strategy((StoryEvent.formal(identifierList) +: eventList).reduce[StrategyStep[StoryState, StoryEvent.StoryData, StoryEvent.StoryData]]((sss, ss) => sss merge ss))
            case Some(_) => Strategy((StoryEvent.formal(identifierList) +: eventList).reduce[StrategyStep[StoryState, StoryEvent.StoryData, StoryEvent.StoryData]]((sss, ss) => sss merge ss))
        }
    }

    def eventStatement = insertRelationshipStatement | findStatement | findStatement1 | narrateStatement | subgoalStatement | updateStatement | removeRelationshipStatement | noneStatement2

    def subgoalKeyword = "subgoal"

    def subgoalStatement = subgoalKeyword ~ identifier ~ rep(identifier) ~ endStatement ^^ {
        case subgoal ~ n ~ parameterList ~ end => {
            StoryEvent.subgoal(new Goal[StoryState, StoryEvent.StoryData, StoryEvent.StoryData]() {
                def name: String = {
                    n
                }

                def strategySet: Set[Strategy[StoryState, StoryEvent.StoryData, StoryEvent.StoryData]] = {
                    goalMap(n).strategySet
                }

                def satisfy(state: StoryState, input: StoryEvent.StoryData): GoalExecution[StoryState, StoryEvent.StoryData, StoryEvent.StoryData] = {
                    goalMap(n).satisfy(state, input)
                }
            }, parameterList)
        }
    }

    def narrateKeyword = "narrate" | "n"

    def narrateStatement = narrateKeyword ~ template ~ endStatement ^^ {
        case narrate ~ t ~ end => StoryEvent.narrate(t)
    }

    def insertKeyword = "insert" | "i"

    def insertRelationshipStatement = insertKeyword ~ identifier ~ opt("into") ~ identifier ~ identifier ~ identifier ~ endStatement ^^ {
        case insert ~ s ~ into ~ e1 ~ e2 ~ a ~ end => StoryEvent.insertRelationship(e1, e2, a, s);
    }

    def removeKeyword = "remove" | "r"

    def removeRelationshipStatement = removeKeyword ~ identifier ~ opt("from") ~ identifier ~ identifier ~ identifier ~ endStatement ^^ {
        case remove ~ s ~ from ~ e1 ~ e2 ~ a ~ end => StoryEvent.removeRelationship(e1, e2, a, s);
    }

    def updateKeyword = "update" | "u"

    def updateStatement = updateKeyword ~ identifier ~ identifier ~ "to" ~ identifier ~ endStatement ^^ {
        case update ~ e ~ a ~ to ~ s ~ end => StoryEvent.update(e, a, s);
    }

    def findKeyword = "find" | "f"

    def whereKeyword = "where" | "w"

    def predicate = isNotPredicate | relationshipDoesNotContainPredicate | relationshipContainsPredicate | containsPredicate | isPredicate | isAttributePredicate

    def isNotPredicate = identifier ~ "isnt" ~ identifier ^^ {
        case entity1 ~ isnt ~ entity2 => StoryEvent.isNot(entity1, entity2)
    }

    def isPredicate = identifier ~ "is" ~ identifier ^^ {
        case entity1 ~ is ~ entity2 => StoryEvent.is(entity1, entity2)
    }

    def isAttributePredicate = identifier ~ identifier ~ "is" ~ identifier ^^ {
        case entity ~ attribute ~ is ~ symbol => StoryEvent.is(entity, attribute, symbol)
    }

    def relationshipDoesNotContainPredicate = "relationship" ~ identifier ~ identifier ~ identifier ~ "does" ~ "not" ~ "contain" ~ identifier ^^ {
        case r ~ e1 ~ e2 ~ a ~ d ~ n ~ c ~ s => StoryEvent.relationshipDoesNotContain(e1, e2, a, s)
    }

    def containsPredicate = identifier ~ identifier ~ "contains" ~ identifier ~ identifier ^^ {
        case e1 ~ a1 ~ c ~ e2 ~ a2 => StoryEvent.contains(e1, a1, e2, a2)
    }

    def relationshipContainsPredicate = "relationship" ~ identifier ~ identifier ~ identifier ~ "contains" ~ identifier ^^ {
        case relationship ~ e1 ~ e2 ~ a ~ c ~ s => StoryEvent.relationshipContains(e1, e2, a, s)
    }

    def findStatement = findKeyword ~ identifier ~ identifier ~ whereKeyword ~ rep(predicate) ~ endStatement ^^ {
        case find ~ e1 ~ e2 ~ where ~ predicateList ~ end => StoryEvent.findCharacterCharacter(e1, e2, predicateList)
    }

    def findStatement1 = findKeyword ~ identifier ~ whereKeyword ~ rep(predicate) ~ endStatement ^^ {
        case find ~ e1 ~ where ~ predicateList ~ end => StoryEvent.findCharacter(e1, predicateList)
    }

    def noneKeyword = "none"

    def noneStatement2 = noneKeyword ~ identifier ~ identifier ~ whereKeyword ~ rep(predicate) ~ endStatement ^^ {
        case none ~ e1 ~ e2 ~ where ~ predicateList ~ end => StoryEvent.noCharacterCharacter(e1, e2, predicateList)
    }

    def statement = goalDeclarationWithIdentifier | goalDeclarationWithoutIdentifier

    def statementSet = rep(statement)
}

object Test extends Parser with App {
    val stream = getClass.getResourceAsStream("Sample.story")
    System.out.println(stream)
    val story = io.Source.fromInputStream(getClass.getResourceAsStream("Sample.story")).mkString


    parseAll(statementSet, story.stripMargin) match {
        case Success(lup, _) => {
            val marriage = lup(0)

            val cast = List(
                new Character(1, Map("first" -> Set("Matt"), "gender" -> Set("male"), "orientation" -> Set("male"), "life" -> Set("alive"))),
                new Character(2, Map("first" -> Set("Alison"), "gender" -> Set("female"), "orientation" -> Set("male"), "life" -> Set("alive"))),
                new Character(3, Map("first" -> Set("Jake"), "gender" -> Set("male"), "orientation" -> Set("female", "male"), "life" -> Set("alive"))),
                new Character(4, Map("first" -> Set("Kimberly"), "gender" -> Set("female"), "orientation" -> Set("female", "male"), "life" -> Set("alive"))),
                new Character(5, Map("first" -> Set("Sydney"), "gender" -> Set("female"), "orientation" -> Set("female", "male"), "life" -> Set("alive"))),
                new Character(6, Map("first" -> Set("Jane"), "gender" -> Set("female"), "orientation" -> Set("male"), "life" -> Set("alive"))),
                new Character(7, Map("first" -> Set("Michael"), "gender" -> Set("male"), "orientation" -> Set("female"), "life" -> Set("alive"))),
                new Character(8, Map("first" -> Set("Peter"), "gender" -> Set("male"), "orientation" -> Set("female"), "life" -> Set("alive"))),
                new Character(9, Map("first" -> Set("Amanda"), "gender" -> Set("female"), "orientation" -> Set("male"), "life" -> Set("alive"))),
                new Character(10, Map("first" -> Set("Billy"), "gender" -> Set("male"), "orientation" -> Set("female"), "life" -> Set("alive"))))

            val map = (for {
                i <- 1 to 10
                j <- i to 10
            } yield (i, j) -> Map("status" -> Set[String]())).toMap

            val state0 = new StoryState(cast.map((c: Character) => (c.id -> c)).toMap, map)

            val storyData = Map[String, Int]()
            var lasttext = ""
            var state = state0

            for(i <- 1 to 100) {
                val ctx = marriage.satisfy(state, storyData)
                Fabula.fabula(ctx).foreach(eventExecution => eventExecution.successor.get._1.narration match {
                    case None => ;
                    case Some(s) => {
                        if (s == lasttext) {
                        } else {
                            System.out.println(s)
                        }
                    }
                })
                state = ctx.successor().get._1
            }
        }
        case x => println(x)
    }
}