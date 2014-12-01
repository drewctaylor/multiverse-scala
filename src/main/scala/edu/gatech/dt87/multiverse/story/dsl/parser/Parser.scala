package edu.gatech.dt87.multiverse.story.dsl.parser

import edu.gatech.dt87.multiverse.planner.{Event, Goal, Strategy, StrategyStep}
import edu.gatech.dt87.multiverse.story.StateStrategyStep.SymbolMap
import edu.gatech.dt87.multiverse.story.dsl.lexer._
import edu.gatech.dt87.multiverse.story.{State, StateStrategyStep}

import scala.collection.mutable
import scala.util.parsing.combinator.syntactical.TokenParsers

class Parser extends TokenParsers with LexerTokens {
    type Tokens = Lexer
    val lexical: Tokens = new Lexer()

    lazy val declarationStory = declarationState ~ rep(declarationGoal) ^^ {
        case s ~ gl => DeclarationStory(s, gl)
    }

    lazy val declarationGoal: Parser[DeclarationGoal] = lexical.TokenKeywordGoal ~ expressionIdentifier ~ lexical.TokenKeywordParenthesesLeft ~ opt(declarationParameterList) ~ lexical.TokenKeywordParenthesesRight ~ opt(expressionString) ~ blockGoal ^^ {
        case _ ~ i ~ _ ~ None ~ _ ~ s ~ b => DeclarationGoal(i, s, List(), b)
        case _ ~ i ~ _ ~ Some(l) ~ _ ~ s ~ b => DeclarationGoal(i, s, l, b)
    }

    lazy val blockGoal: Parser[BlockGoal] = lexical.TokenKeywordBraceLeft ~ rep(declarationStrategy) ~ lexical.TokenKeywordBraceRight ^^ {
        case _ ~ l ~ _ => BlockGoal(l)
    }

    lazy val declarationStrategy: Parser[DeclarationStrategy] = lexical.TokenKeywordStrategy ~ opt(expressionString) ~ blockStrategy ^^ {
        case _ ~ l ~ b => DeclarationStrategy(l, b)
    }

    lazy val blockStrategy: Parser[BlockStrategy] = lexical.TokenKeywordBraceLeft ~ rep(statementSubgoal | statementQuery | statementAssignment | statementNarration | statementDeclarationData) ~ lexical.TokenKeywordBraceRight ^^ {
        case _ ~ s ~ _ => BlockStrategy(s)
    }

    lazy val statementDeclarationData: Parser[StatementDeclarationData] = (declarationEntity | declarationRelationship) ^^ {
        case d => StatementDeclarationData(d)
    }

    lazy val declarationParameterList: Parser[List[DeclarationParameter]] = declarationParameter ~ rep(lexical.TokenKeywordComma ~ declarationParameter) ^^ {
        case dl ~ dList => dList.foldLeft(List(dl))((declarationParameter, operationDeclarationParameter) => operationDeclarationParameter match {
            case _ ~ dr => declarationParameter :+ dr
        })
    }

    lazy val declarationParameter: Parser[DeclarationParameter] = expressionIdentifier ~ expressionIdentifier ^^ {
        case e ~ i => DeclarationParameter(e, i)
    }

    lazy val declarationState: Parser[DeclarationState] = lexical.TokenKeywordState ~ blockState ^^ {
        case _ ~ b => DeclarationState(b)
    }

    lazy val blockState: Parser[BlockState] = lexical.TokenKeywordBraceLeft ~ rep(declarationEntity | declarationRelationship) ~ lexical.TokenKeywordBraceRight ^^ {
        case _ ~ d ~ _ => BlockState(d)
    }

    lazy val declarationRelationship: Parser[DeclarationRelationship] = expressionRelationship ~ blockAssignment ^^ {
        case i ~ b => DeclarationRelationship(i, b)
    }

    lazy val declarationEntity: Parser[DeclarationEntity] = expressionIdentifier ~ expressionIdentifier ~ blockAssignment ^^ {
        case k ~ i ~ b => DeclarationEntity(k, ExpressionEntity(i), b)
    }

    lazy val blockLiteral: Parser[BlockLiteral] = lexical.TokenKeywordBraceLeft ~ expressionString ~ rep(lexical.TokenKeywordComma ~ expressionString) ~ lexical.TokenKeywordBraceRight ^^ {
        case _ ~ el ~ eList ~ _ => BlockLiteral(eList.foldLeft(List(el))((symbol, operationSymbol) => operationSymbol match {
            case _ ~ er => symbol :+ er
        }))
    }

    lazy val blockAssignment: Parser[BlockAssignment] = lexical.TokenKeywordBraceLeft ~ rep(statementAssignmentContextual) ~ lexical.TokenKeywordBraceRight ^^ {
        case _ ~ el ~ _ => BlockAssignment(el)
    }

    lazy val statementSubgoal: Parser[StatementSubgoal] = expressionIdentifier ~ lexical.TokenKeywordParenthesesLeft ~ opt(parameterList) ~ lexical.TokenKeywordParenthesesRight ^^ {
        case i ~ _ ~ Some(p) ~ _ => StatementSubgoal(i, p)
        case i ~ _ ~ None ~ _ => StatementSubgoal(i, List())
    }

    lazy val parameterList: Parser[List[ExpressionIdentifier]] = expressionIdentifier ~ rep(lexical.TokenKeywordComma ~ expressionIdentifier) ^^ {
        case pl ~ pList => pList.foldLeft(List[ExpressionIdentifier](pl))((parameterList, parameter) => parameter match {
            case _ ~ pr => parameterList :+ pr
        })
    }

    lazy val statementNarration: Parser[StatementNarration] = expressionString ^^ {
        case e => StatementNarration(e)
    }

    lazy val statementQuery = opt(declarationParameterList) ~ operatorQuery ~ expression ^^ {
        case Some(el) ~ (op: lexical.TokenOperatorQuery) ~ er => StatementQuery(el, er, operatorQueryMap(op))
        case None ~ (op: lexical.TokenOperatorQuery) ~ er => StatementQuery(List(), er, operatorQueryMap(op))
    }

    lazy val operatorQuery = lexical.TokenOperatorQueryAll | lexical.TokenOperatorQueryNone

    lazy val statementAssignment = expressionIdentifierSequence ~ operatorAssignment ~ expression ^^ {
        case el ~ (op: lexical.TokenOperatorAssignment) ~ er => StatementAssignment(el, er, operatorAssignmentMap(op))
    }

    lazy val statementAssignmentContextual = expressionIdentifier ~ operatorAssignment ~ expression ^^ {
        case el ~ (op: lexical.TokenOperatorAssignment) ~ er => StatementAssignment(ExpressionIdentifierSequence(None, Seq(el)), er, operatorAssignmentMap(op))
    }

    lazy val operatorAssignment = lexical.TokenOperatorAssignmentUpdate | lexical.TokenOperatorAssignmentInsert | lexical.TokenOperatorAssignmentRemove

    lazy val expression = expressionConditionalAnd ~ rep(lexical.TokenOperatorOr ~ expressionConditionalAnd) ^^ {
        case el ~ eList => eList.foldLeft(el)((expression, operationExpression) => operationExpression match {
            case (op: lexical.TokenOperatorBinary) ~ er => ExpressionBinary(expression, er, operatorBinaryMap(op))
        })
    }

    lazy val expressionConditionalAnd = expressionEquality ~ rep(lexical.TokenOperatorAnd ~ expressionEquality) ^^ {
        case el ~ eList => eList.foldLeft(el)((expression, operationExpression) => operationExpression match {
            case (op: lexical.TokenOperatorBinary) ~ er => ExpressionBinary(expression, er, operatorBinaryMap(op))
        })
    }

    lazy val expressionEquality = expressionRelational ~ rep(operatorEquality ~ expressionRelational) ^^ {
        case el ~ eList => eList.foldLeft(el)((expression, operationExpression) => operationExpression match {
            case (op: lexical.TokenOperatorBinary) ~ er => ExpressionBinary(expression, er, operatorBinaryMap(op))
        })
    }

    lazy val operatorEquality = lexical.TokenOperatorEqual | lexical.TokenOperatorNotEqual

    lazy val expressionRelational = expressionAdditive ~ rep(operatorRelational ~ expressionAdditive) ^^ {
        case el ~ eList => eList.foldLeft(el)((expression, operationExpression) => operationExpression match {
            case (op: lexical.TokenOperatorBinary) ~ er => ExpressionBinary(expression, er, operatorBinaryMap(op))
        })
    }

    lazy val operatorRelational = lexical.TokenOperatorSubset | lexical.TokenOperatorSuperset | lexical.TokenOperatorLessThan | lexical.TokenOperatorGreaterThan | lexical.TokenOperatorLessThanOrEqual | lexical.TokenOperatorGreaterThanOrEqual

    lazy val expressionAdditive = expressionMultiplicative ~ rep(operatorAdditive ~ expressionMultiplicative) ^^ {
        case el ~ eList => eList.foldLeft(el)((expression, operationExpression) => operationExpression match {
            case (op: lexical.TokenOperatorBinary) ~ er => ExpressionBinary(expression, er, operatorBinaryMap(op))
        })
    }

    lazy val operatorAdditive = lexical.TokenOperatorUnion | lexical.TokenOperatorDifference | lexical.TokenOperatorIntersection | lexical.TokenOperatorAddition | lexical.TokenOperatorSubtraction

    lazy val expressionMultiplicative = expressionUnary ~ rep(operatorAdditive ~ expressionUnary) ^^ {
        case el ~ eList => eList.foldLeft(el)((expression, operationExpression) => operationExpression match {
            case (op: lexical.TokenOperatorBinary) ~ er => ExpressionBinary(expression, er, operatorBinaryMap(op))
        })
    }

    lazy val operatorMultiplicative = lexical.TokenOperatorMultiplication | lexical.TokenOperatorDivision

    lazy val expressionUnary = opt(operatorUnary) ~ expressionPostfix ^^ {
        case None ~ e => e
        case Some(op: lexical.TokenOperatorUnary) ~ e => ExpressionUnary(e, operatorUnaryMap(op))
    }

    lazy val operatorUnary = lexical.TokenOperatorNot | lexical.TokenOperatorCardinality

    lazy val expressionPostfix = expressionAtom ~ opt(operatorPostfix) ^^ {
        case e ~ None => e
        case e ~ Some(op: lexical.TokenOperatorUnary) => ExpressionUnary(e, operatorUnaryMap(op))
    }

    lazy val operatorPostfix = lexical.TokenOperatorDecrement | lexical.TokenOperatorDecrementToMinimum | lexical.TokenOperatorIncrementToMaximum | lexical.TokenOperatorIncrement

    lazy val expressionAtom: Parser[Expression] = expressionLiteral | expressionIdentifierSequence | expressionEnclosed

    lazy val expressionEnclosed: Parser[Expression] = lexical.TokenKeywordParenthesesLeft ~ expression ~ lexical.TokenKeywordParenthesesRight ^^ {
        case _ ~ e ~ _ => e
    }

    lazy val expressionIdentifier: Parser[ExpressionIdentifier] = elem("identifier", _.isInstanceOf[TokenIdentifier]) ^^ {
        case lexical.TokenIdentifier(i) => ExpressionIdentifier(Symbol(i))
    }

    lazy val expressionIdentifierSequence = opt(expressionRelationshipEnclosed ~ lexical.TokenKeywordScope) ~ expressionIdentifier ~ rep(lexical.TokenKeywordScope ~ expressionIdentifier) ^^ {
        case Some(r ~ _) ~ i ~ iList => iList.foldLeft(ExpressionIdentifierSequence(Some(r), Seq(i)))((expressionIdentifierSequence, operationExpression) => operationExpression match {
            case _ ~ ir => ExpressionIdentifierSequence(expressionIdentifierSequence.owner, expressionIdentifierSequence.identifierSeq :+ ir)
        })
        case None ~ i ~ iList => iList.foldLeft(ExpressionIdentifierSequence(Some(ExpressionEntity(i)), Seq()))((expressionIdentifierSequence, operationExpression) => operationExpression match {
            case _ ~ ir => ExpressionIdentifierSequence(expressionIdentifierSequence.owner, expressionIdentifierSequence.identifierSeq :+ ir)
        })
    }

    lazy val expressionRelationshipEnclosed: Parser[ExpressionRelationship] = lexical.TokenKeywordParenthesesLeft ~ expressionRelationship ~ lexical.TokenKeywordParenthesesRight ^^ {
        case _ ~ r ~ _ => r
    }

    lazy val expressionRelationship: Parser[ExpressionRelationship] = expressionRelationshipBidirectional | expressionRelationshipUnidirectional

    lazy val expressionRelationshipUnidirectional: Parser[ExpressionRelationshipUnidirectional] = expressionIdentifierSequence ~ lexical.TokenKeywordRelationshipUnidirectional ~ expressionIdentifierSequence ^^ {
        case left ~ _ ~ right => ExpressionRelationshipUnidirectional(left, right)
    }

    lazy val expressionRelationshipBidirectional: Parser[ExpressionRelationshipBidirectional] = expressionIdentifierSequence ~ lexical.TokenKeywordRelationshipBidirectional ~ expressionIdentifierSequence ^^ {
        case left ~ _ ~ right => ExpressionRelationshipBidirectional(left, right)
    }

    lazy val expressionLiteral: Parser[Expression] = expressionBoolean | expressionEmpty | expressionNumber | expressionString | failure("literal expected")

    lazy val expressionBoolean: Parser[ExpressionLiteralBoolean] = expressionBooleanTrue | expressionBooleanFalse | failure("boolean expected")

    lazy val expressionBooleanTrue: Parser[ExpressionLiteralBoolean] = lexical.TokenLiteralBooleanTrue ^^^ ExpressionLiteralBoolean(value = true)

    lazy val expressionBooleanFalse: Parser[ExpressionLiteralBoolean] = lexical.TokenLiteralBooleanFalse ^^^ ExpressionLiteralBoolean(value = false)

    lazy val expressionEmpty: Parser[ExpressionLiteralEmpty.type] = lexical.TokenLiteralEmpty ^^^ ExpressionLiteralEmpty

    lazy val expressionNumber: Parser[ExpressionLiteralNumber] = elem("number", _.isInstanceOf[TokenLiteralNumber]) ^^ {
        case lexical.TokenLiteralNumber(n) => ExpressionLiteralNumber(BigDecimal(n))
    }

    lazy val expressionString: Parser[ExpressionLiteralString] = elem("string", _.isInstanceOf[TokenLiteralString]) ^^ {
        case lexical.TokenLiteralString(s) => ExpressionLiteralString(s)
    }

    val operatorUnaryMap: Map[Elem, OperatorUnary] = Map(
        lexical.TokenOperatorCardinality -> OperatorCardinality,
        lexical.TokenOperatorDecrement -> OperatorDecrement,
        lexical.TokenOperatorDecrementToMinimum -> OperatorDecrementToMinimum,
        lexical.TokenOperatorIncrement -> OperatorIncrement,
        lexical.TokenOperatorIncrementToMaximum -> OperatorIncrementToMaximum,
        lexical.TokenOperatorNot -> OperatorNot
    )

    val operatorBinaryMap: Map[Elem, OperatorBinary] = Map(
        lexical.TokenOperatorAddition -> OperatorAddition,
        lexical.TokenOperatorAnd -> OperatorAnd,
        lexical.TokenOperatorDifference -> OperatorDifference,
        lexical.TokenOperatorDivision -> OperatorDivision,
        lexical.TokenOperatorEqual -> OperatorEqual,
        lexical.TokenOperatorGreaterThan -> OperatorGreaterThan,
        lexical.TokenOperatorGreaterThanOrEqual -> OperatorGreaterThanOrEqual,
        lexical.TokenOperatorIntersection -> OperatorIntersection,
        lexical.TokenOperatorLessThan -> OperatorLessThan,
        lexical.TokenOperatorLessThanOrEqual -> OperatorLessThanOrEqual,
        lexical.TokenOperatorMultiplication -> OperatorMultiplication,
        lexical.TokenOperatorNotEqual -> OperatorNotEqual,
        lexical.TokenOperatorOr -> OperatorOr,
        lexical.TokenOperatorSubset -> OperatorSubset,
        lexical.TokenOperatorSubtraction -> OperatorSubtraction,
        lexical.TokenOperatorSuperset -> OperatorSuperset,
        lexical.TokenOperatorUnion -> OperatorUnion
    )

    val operatorAssignmentMap: Map[Elem, OperatorAssignment] = Map(
        lexical.TokenOperatorAssignmentInsert -> OperatorAssignmentInsert,
        lexical.TokenOperatorAssignmentRemove -> OperatorAssignmentRemove,
        lexical.TokenOperatorAssignmentUpdate -> OperatorAssignmentUpdate
    )

    val operatorQueryMap: Map[Elem, OperatorQuery] = Map(
        lexical.TokenOperatorQueryAll -> OperatorQueryAll,
        lexical.TokenOperatorQueryNone -> OperatorQueryNone
    )
}
