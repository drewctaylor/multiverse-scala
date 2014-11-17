package edu.gatech.dt87.multiverse.story.dsl

import edu.gatech.dt87.multiverse.planner._
import edu.gatech.dt87.multiverse.story.StoryStrategyStep.StorySymbolMap
import edu.gatech.dt87.multiverse.story.{StoryState, StoryStrategyStep}

import scala.collection.mutable
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

class Parser extends RegexParsers with PackratParsers {

    lazy val declarationStory = declarationState ~ rep(declarationGoal) ^^ {
        case s ~ gl => DeclarationStory(s, gl)
    }

    lazy val declarationGoal: PackratParser[DeclarationGoal] = "goal" ~ identifier ~ "(" ~ opt(declarationParameterList) ~ ")" ~ opt(expressionString) ~ blockGoal ^^ {
        case g ~ i ~ pl ~ None ~ pr ~ s ~ b => DeclarationGoal(i, s, List(), b)
        case g ~ i ~ pl ~ Some(l) ~ pr ~ s ~ b => DeclarationGoal(i, s, l, b)
    }

    lazy val blockGoal: PackratParser[BlockGoal] = "{" ~ rep(declarationStrategy) ~ "}" ^^ {
        case bl ~ l ~ br => BlockGoal(l)
    }

    lazy val declarationStrategy: PackratParser[DeclarationStrategy] = "strategy" ~ opt(expressionString) ~ blockStrategy ^^ {
        case s ~ l ~ b => DeclarationStrategy(l, b)
    }

    lazy val blockStrategy: PackratParser[BlockStrategy] = "{" ~ rep(statementSatisfaction | statementQuery | statementAssignment | statementNarration | statementDeclarationData) ~ "}" ^^ {
        case bl ~ s ~ br => BlockStrategy(s)
    }

    lazy val statementDeclarationData: PackratParser[StatementDeclarationData] = (declarationEntity | declarationRelationship) ^^ {
        case d => StatementDeclarationData(d)
    }

    lazy val declarationParameterList: PackratParser[List[DeclarationParameter]] = declarationParameter ~ rep("," ~ declarationParameter) ^^ {
        case dl ~ dList => dList.foldLeft(List(dl))((declarationParameter, operationDeclarationParameter) => operationDeclarationParameter match {
            case op ~ dr => declarationParameter :+ dr
        })
    }

    lazy val declarationParameter: PackratParser[DeclarationParameter] = identifier ~ identifier ^^ {
        case e ~ i => DeclarationParameter(e, i)
    }

    lazy val declarationState: PackratParser[DeclarationState] = "state" ~ blockState ^^ {
        case s ~ b => DeclarationState(b)
    }

    lazy val blockState: PackratParser[BlockState] = "{" ~ rep(declarationAttribute | declarationEntity | declarationRelationship) ~ "}" ^^ {
        case bl ~ d ~ br => BlockState(d)
    }

    lazy val declarationAttribute: PackratParser[DeclarationAttribute] = declarationOneSymbol | declarationOneUnordered | declarationOneOrdered | declarationSubsetSymbol | declarationSubsetUnordered | declarationSubsetOrdered

    lazy val declarationOneSymbol: PackratParser[DeclarationAttribute] = "attribute" ~ identifier ~ "symbol" ^^ {
        case a ~ i ~ s1 => DeclarationAttributeOneSymbol(i)
    }

    lazy val declarationOneUnordered: PackratParser[DeclarationAttribute] = "attribute" ~ identifier ~ "one" ~ "of" ~ "unordered" ~ "set" ~ blockLiteral ^^ {
        case a ~ i ~ s1 ~ s2 ~ s3 ~ s4 ~ b => DeclarationAttributeOneUnordered(i, b)
    }

    lazy val declarationOneOrdered: PackratParser[DeclarationAttribute] = "attribute" ~ identifier ~ "one" ~ "of" ~ "ordered" ~ "set" ~ blockLiteral ^^ {
        case a ~ i ~ s1 ~ s2 ~ s3 ~ s4 ~ b => DeclarationAttributeOneOrdered(i, b)
    }

    lazy val declarationSubsetSymbol: PackratParser[DeclarationAttribute] = "attribute" ~ identifier ~ "set" ~ "of" ~ "symbols" ^^ {
        case a ~ i ~ s1 ~ s2 ~ s3 => DeclarationAttributeSubsetSymbol(i)
    }

    lazy val declarationSubsetUnordered: PackratParser[DeclarationAttribute] = "attribute" ~ identifier ~ "subset" ~ "of" ~ "unordered" ~ "set" ~ blockLiteral ^^ {
        case a ~ i ~ s1 ~ s2 ~ s3 ~ s4 ~ b => DeclarationAttributeSubsetUnordered(i, b)
    }

    lazy val declarationSubsetOrdered: PackratParser[DeclarationAttribute] = "attribute" ~ identifier ~ "subset" ~ "of" ~ "ordered" ~ "set" ~ blockLiteral ^^ {
        case a ~ i ~ s1 ~ s2 ~ s3 ~ s4 ~ b => DeclarationAttributeSubsetOrdered(i, b)
    }

    lazy val declarationRelationship: PackratParser[DeclarationRelationship] = expressionRelationship ~ blockAssignment ^^ {
        case i ~ b => DeclarationRelationship(i, b)
    }

    lazy val declarationEntity: PackratParser[DeclarationEntity] = identifier ~ expressionEntity ~ blockAssignment ^^ {
        case k ~ i ~ b => DeclarationEntity(k, i, b)
    }

    lazy val blockLiteral: PackratParser[BlockLiteral] = "{" ~ expressionSymbol ~ rep("," ~ expressionSymbol) ~ "}" ^^ {
        case bl ~ el ~ eList ~ br => BlockLiteral(eList.foldLeft(List(el))((symbol, operationSymbol) => operationSymbol match {
            case op ~ er => symbol :+ er
        }))
    }

    lazy val blockAssignment: PackratParser[BlockAssignment] = "{" ~ rep(statementAssignment) ~ "}" ^^ {
        case bl ~ el ~ br => BlockAssignment(el)
    }

    lazy val statementSatisfaction: PackratParser[StatementSatisfaction] = identifier ~ "(" ~ opt(parameterList) ~ ")" ^^ {
        case i ~ pl ~ Some(p) ~ pr => StatementSatisfaction(i, p)
        case i ~ pl ~ None ~ pr => StatementSatisfaction(i, List())
    }

    lazy val parameterList: PackratParser[List[Symbol]] = identifier ~ rep("," ~ identifier) ^^ {
        case pl ~ pList => pList.foldLeft(List[Symbol](pl))((parameterList, parameter) => parameter match {
            case op ~ pr => parameterList :+ pr
        })
    }

    lazy val statementNarration: PackratParser[StatementNarration] = expressionString ^^ {
        case e => StatementNarration(e)
    }

    lazy val statementQuery: PackratParser[StatementQuery] = opt(declarationParameterList) ~ operatorQuery ~ expression ^^ {
        case Some(l) ~ op ~ e => StatementQuery(l, e, op)
        case None ~ op ~ e => StatementQuery(List(), e, op)
    }

    lazy val operatorQuery: PackratParser[OperatorQuery] = (OperatorQueryAll.symbol | OperatorQueryNone.symbol) ^^ {
        case OperatorQueryAll.symbol => OperatorQueryAll
        case OperatorQueryNone.symbol => OperatorQueryNone
    }

    lazy val statementAssignment: PackratParser[StatementAssignment] = expressionAttribute ~ operatorAssignment ~ expression ^^ {
        case el ~ op ~ er => StatementAssignment(el, er, op)
    }

    lazy val operatorAssignment: PackratParser[OperatorAssignment] = (OperatorAssignmentUpdate.symbol | OperatorAssignmentInsert.symbol | OperatorAssignmentRemove.symbol) ^^ {
        case OperatorAssignmentUpdate.symbol => OperatorAssignmentUpdate
        case OperatorAssignmentInsert.symbol => OperatorAssignmentInsert
        case OperatorAssignmentRemove.symbol => OperatorAssignmentRemove
    }

    lazy val expression: PackratParser[Expression] = expressionConditionalAnd ~ rep(OperatorOr.symbol ~ expressionConditionalAnd) ^^ {
        case el ~ eList => eList.foldLeft(el)((expression, operationExpression) => operationExpression match {
            case op ~ er => ExpressionBinary(expression, er, OperatorOr)
        })
    }

    lazy val expressionConditionalAnd: PackratParser[Expression] = expressionEquality ~ rep(OperatorAnd.symbol ~ expressionEquality) ^^ {
        case el ~ eList => eList.foldLeft(el)((expression, operationExpression) => operationExpression match {
            case op ~ er => ExpressionBinary(expression, er, OperatorAnd)
        })
    }

    lazy val expressionEquality: PackratParser[Expression] = expressionRelational ~ rep(operatorEquality ~ expressionRelational) ^^ {
        case el ~ eList => eList.foldLeft(el)((expression, operationExpression) => operationExpression match {
            case op ~ er => ExpressionBinary(expression, er, op)
        })
    }

    lazy val operatorEquality: PackratParser[OperatorBinary] = (OperatorEqual.symbol | OperatorNotEqual.symbol) ^^ {
        case OperatorEqual.symbol => OperatorEqual
        case OperatorNotEqual.symbol => OperatorNotEqual
    }

    lazy val expressionRelational: PackratParser[Expression] = expressionAdditive ~ rep(operatorRelational ~ expressionAdditive) ^^ {
        case el ~ eList => eList.foldLeft(el)((expression, operationExpression) => operationExpression match {
            case op ~ er => ExpressionBinary(expression, er, op)
        })
    }

    lazy val operatorRelational: PackratParser[OperatorBinary] = (OperatorSubset.symbol | OperatorSuperset.symbol | OperatorLessThan.symbol | OperatorGreaterThan.symbol | OperatorLessThanOrEqual.symbol | OperatorGreaterThanOrEqual.symbol) ^^ {
        case OperatorSubset.symbol => OperatorSubset
        case OperatorSuperset.symbol => OperatorSuperset
        case OperatorLessThan.symbol => OperatorLessThan
        case OperatorGreaterThan.symbol => OperatorGreaterThan
        case OperatorLessThanOrEqual.symbol => OperatorLessThanOrEqual
        case OperatorGreaterThanOrEqual.symbol => OperatorGreaterThanOrEqual
    }

    lazy val expressionAdditive: PackratParser[Expression] = expressionUnary ~ rep(operatorAdditive ~ expressionUnary) ^^ {
        case el ~ eList => eList.foldLeft(el)((expression, operationExpression) => operationExpression match {
            case op ~ er => ExpressionBinary(expression, er, op)
        })
    }

    lazy val operatorAdditive: PackratParser[OperatorBinary] = (OperatorUnion.symbol | OperatorDifference.symbol | OperatorIntersection.symbol) ^^ {
        case OperatorUnion.symbol => OperatorUnion
        case OperatorDifference.symbol => OperatorDifference
        case OperatorIntersection.symbol => OperatorIntersection
    }

    lazy val expressionUnary: PackratParser[Expression] = opt(OperatorNot.symbol) ~ expressionPostfix ^^ {
        case None ~ e => e
        case Some(op) ~ e => ExpressionUnary(e, OperatorNot)
    }

    lazy val expressionPostfix: PackratParser[Expression] = expressionAtom ~ opt(operatorPostfix) ^^ {
        case e ~ None => e
        case e ~ Some(op) => ExpressionUnary(e, op)
    }

    lazy val operatorPostfix: PackratParser[OperatorUnary] = (OperatorIncrement.symbol | OperatorDecrement.symbol | OperatorIncrementToMaximum.symbol | OperatorDecrementToMinimum.symbol) ^^ {
        case OperatorIncrement.symbol => OperatorIncrement
        case OperatorDecrement.symbol => OperatorDecrement
        case OperatorIncrementToMaximum.symbol => OperatorIncrementToMaximum
        case OperatorDecrementToMinimum.symbol => OperatorDecrementToMinimum
    }

    lazy val expressionAtom: PackratParser[Expression] = expressionNumber | expressionBoolean | expressionSymbol | expressionAttribute | expressionEnclosed

    lazy val expressionEnclosed: PackratParser[Expression] = "(" ~ expression ~ ")" ^^ {
        case pl ~ e ~ pr => e
    }

    lazy val expressionAttribute: PackratParser[ExpressionAttribute] = opt((expressionEntity | expressionRelationshipEnclosed) ~ ".") ~ identifier ^^ {
        case Some((ao: ExpressionAttributeOwner) ~ d) ~ a => ExpressionAttribute(Some(ao), a)
        case None ~ a => ExpressionAttribute(None, a)
    }

    lazy val expressionEntity: PackratParser[ExpressionEntity] = identifier ^^ {
        case e => ExpressionEntity(e)
    }

    lazy val expressionRelationshipEnclosed: PackratParser[ExpressionAttributeOwner] = "(" ~ expressionRelationship ~ ")" ^^ {
        case pl ~ r ~ pr => r
    }

    lazy val expressionRelationship: PackratParser[ExpressionRelationship] = expressionRelationshipUnidirectional | expressionRelationshipBidirectional

    lazy val expressionRelationshipUnidirectional: PackratParser[ExpressionRelationshipUnidirectional] = identifier ~ "->" ~ identifier ^^ {
        case el ~ ur ~ er => ExpressionRelationshipUnidirectional(el, er)
    }

    lazy val expressionRelationshipBidirectional: PackratParser[ExpressionRelationshipBidirectional] = identifier ~ "<->" ~ identifier ^^ {
        case el ~ br ~ er => ExpressionRelationshipBidirectional(el, er)
    }

    lazy val expressionString: PackratParser[ExpressionLiteralString] = "\"" ~ literalStringRegex ~ "\"" ^^ {
        case ql ~ l ~ qr => ExpressionLiteralString(l)
    }

    lazy val expressionNumber: PackratParser[ExpressionLiteralNumber] = literalNumberRegex ^^ {
        case n => ExpressionLiteralNumber(Symbol(n))
    }

    lazy val expressionBoolean: PackratParser[ExpressionLiteralBoolean] = ("true" | "false") ^^ {
        case b => ExpressionLiteralBoolean(Symbol(b))
    }

    lazy val expressionSymbol: PackratParser[ExpressionLiteralSymbol] = "\"" ~ literalSymbolRegex ~ "\"" ^^ {
        case ql ~ l ~ qr => ExpressionLiteralSymbol(Symbol(l))
    }

    lazy val identifier: PackratParser[Symbol] = identifierRegex ^^ {
        case i => Symbol(i)
    }

    lazy val literalBoolean = "true" | "false"

    lazy val literalStringRegex = """[^"]*""".r

    lazy val literalNumberRegex = """[\p{Digit}]+""".r

    lazy val literalSymbolRegex = """[\p{Alnum}][\p{Alnum} \-,]*""".r

    lazy val identifierRegex = """\p{Alpha}\p{Alnum}*""".r
}

object Parser {

    def parse(source: String): Option[(StoryState, Set[Goal[StoryState, StoryStrategyStep.StorySymbolMap, StoryStrategyStep.StorySymbolMap]])] = {
        val parser = new Parser()

        parser.parseAll(parser.declarationStory, source) match {
            case parser.Success(DeclarationStory(s, gl), next) => {
                val eventList = s.block.declarationList.foldLeft(List[Event[StoryState, StoryStrategyStep.StorySymbolMap, StoryStrategyStep.StorySymbolMap]]())((el, d) => d match {
                    case DeclarationAttributeOneSymbol(a) => el
                    case DeclarationAttributeOneUnordered(a, bl) => el
                    case DeclarationAttributeOneOrdered(a, bl) => el
                    case DeclarationAttributeSubsetSymbol(a) => el
                    case DeclarationAttributeSubsetUnordered(a, bl) => el
                    case DeclarationAttributeSubsetOrdered(a, bl) => el
                    case DeclarationEntity(k, ExpressionEntity(e), ba) => {
                        el ++ List(StoryStrategyStep.eventDeclare(DeclarationEntity(k, ExpressionEntity(e), ba))) ++ ba.assignmentList.map({
                            case StatementAssignment(ExpressionAttribute(None, a), right, op) => StoryStrategyStep.eventAssignment(StatementAssignment(ExpressionAttribute(Some(ExpressionEntity(e)), a), right, op))
                            case sa => StoryStrategyStep.eventAssignment(sa)
                        })
                    }
                    case DeclarationRelationship(ExpressionRelationshipBidirectional(l, r), ba) => {
                        el ++ ba.assignmentList.map({
                            case StatementAssignment(ExpressionAttribute(None, a), right, op) => StoryStrategyStep.eventAssignment(StatementAssignment(ExpressionAttribute(Some(ExpressionRelationshipBidirectional(l, r)), a), right, op))
                            case sa => StoryStrategyStep.eventAssignment(sa)
                        })
                    }
                    case DeclarationRelationship(ExpressionRelationshipUnidirectional(l, r), ba) => {
                        el ++ ba.assignmentList.map({
                            case StatementAssignment(ExpressionAttribute(None, a), right, op) => StoryStrategyStep.eventAssignment(StatementAssignment(ExpressionAttribute(Some(ExpressionRelationshipUnidirectional(l, r)), a), right, op))
                            case sa => StoryStrategyStep.eventAssignment(sa)
                        })
                    }
                })

                val step = eventList.reduce((a: StrategyStep[StoryState, StoryStrategyStep.StorySymbolMap, StoryStrategyStep.StorySymbolMap], b: StrategyStep[StoryState, StoryStrategyStep.StorySymbolMap, StoryStrategyStep.StorySymbolMap]) => a merge b)
                val strategy: Strategy[StoryState, StorySymbolMap, StorySymbolMap] = Strategy(step)
                val goalexecution = Goal(strategy).satisfy(StoryState(), Map())
                val storyState = goalexecution.successor().get._1
                val storySymbolMap = Map[Symbol, (Symbol, Int)]()

                var map = mutable.Map[Symbol, Goal[StoryState, StoryStrategyStep.StorySymbolMap, StoryStrategyStep.StorySymbolMap]]()
                val goalForGoalSymbol = (gs: Symbol) => map(gs)
                val declarationFor = (gs: Symbol) => gl.filter(_.identifier == gs).head
                val goalForGoalName = (gn: String) => gl.filter((g) => (g.label.map(_.string) getOrElse g.identifier.name) == gn).head
                val goalList = gl.map(g => Goal(g.label.map(_.string) getOrElse g.identifier.name, g.block.strategyList.map(s => {
                    val eventList = s.block.statementList.map({
                        case s: StatementQuery => StoryStrategyStep.eventQuery(s)
                        case s: StatementSatisfaction => StoryStrategyStep.eventSatisfaction(s, goalForGoalSymbol, declarationFor)
                        case s: StatementNarration => StoryStrategyStep.eventNarration(s)
                        case s: StatementAssignment => StoryStrategyStep.eventAssignment(s)
                        case StatementDeclarationData(DeclarationEntity(k, ExpressionEntity(e), ba)) => (List(StoryStrategyStep.eventDeclare(DeclarationEntity(k, ExpressionEntity(e), ba))) ++ ba.assignmentList.map({
                            case StatementAssignment(ExpressionAttribute(None, a), right, op) => StoryStrategyStep.eventAssignment(StatementAssignment(ExpressionAttribute(Some(ExpressionEntity(e)), a), right, op))
                            case sa => StoryStrategyStep.eventAssignment(sa)
                        })).reduce((a: StrategyStep[StoryState, StoryStrategyStep.StorySymbolMap, StoryStrategyStep.StorySymbolMap], b: StrategyStep[StoryState, StoryStrategyStep.StorySymbolMap, StoryStrategyStep.StorySymbolMap]) => {
                            a merge b
                        })
                        case StatementDeclarationData(DeclarationRelationship(ExpressionRelationshipBidirectional(l, r), ba)) => ba.assignmentList.map({
                            case StatementAssignment(ExpressionAttribute(None, a), right, op) => StoryStrategyStep.eventAssignment(StatementAssignment(ExpressionAttribute(Some(ExpressionRelationshipBidirectional(l, r)), a), right, op))
                            case sa => StoryStrategyStep.eventAssignment(sa)
                        }).reduce((a: StrategyStep[StoryState, StoryStrategyStep.StorySymbolMap, StoryStrategyStep.StorySymbolMap], b: StrategyStep[StoryState, StoryStrategyStep.StorySymbolMap, StoryStrategyStep.StorySymbolMap]) => {
                            a merge b
                        })
                        case StatementDeclarationData(DeclarationRelationship(ExpressionRelationshipUnidirectional(l, r), ba)) => ba.assignmentList.map({
                            case StatementAssignment(ExpressionAttribute(None, a), right, op) => StoryStrategyStep.eventAssignment(StatementAssignment(ExpressionAttribute(Some(ExpressionRelationshipUnidirectional(l, r)), a), right, op))
                            case sa => StoryStrategyStep.eventAssignment(sa)
                        }).reduce((a: StrategyStep[StoryState, StoryStrategyStep.StorySymbolMap, StoryStrategyStep.StorySymbolMap], b: StrategyStep[StoryState, StoryStrategyStep.StorySymbolMap, StoryStrategyStep.StorySymbolMap]) => {
                            a merge b
                        })
                    }).reduce((a: StrategyStep[StoryState, StoryStrategyStep.StorySymbolMap, StoryStrategyStep.StorySymbolMap], b: StrategyStep[StoryState, StoryStrategyStep.StorySymbolMap, StoryStrategyStep.StorySymbolMap]) => {
                        a merge b
                    })

                    if(s.label.isDefined)
                        Strategy(s.label.get.string, eventList)
                    else
                        Strategy(eventList)
                }).toSeq: _*))

                goalList.map((g) => goalForGoalName(g.name).identifier -> g).map(map += _)

                val top = goalList.filter((g) => declarationFor(goalForGoalName(g.name).identifier).parameterList.isEmpty)

                Some(storyState, top.toSet)
            }
            case o => {
                println(o)
                None
            }
        }

    }
}