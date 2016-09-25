package edu.gatech.dt87.multiverse.language.parser

import edu.gatech.dt87.multiverse.language.`type`.{Female, Male, Neuter}
import edu.gatech.dt87.multiverse.language.lexer._

import scala.util.parsing.combinator.syntactical.TokenParsers

object Parser extends TokenParsers with LexerTokens {
    type Tokens = Lexer
    val lexical: Tokens = new Lexer()

    /**
     * A parser that matches a compilation unit: an optional story statement, followed by an optional state declaration, followed by a sequence of goal declarations.
     */
    lazy val unit = opt(lexical.TokenKeywordStory ~ literalString ~ opt(literalNumber)) ~ opt(stateDeclaration) ~ rep(goalDeclaration) ^^ {
        case Some(_ ~ t ~ n) ~ s ~ gl => DeclarationStory(Some(t), n, s, gl)
        case None ~ s ~ gl => DeclarationStory(None, None, s, gl)
    }

    /**
     * A parser that matches a state declaration.
     */
    lazy val stateDeclaration: Parser[DeclarationState] = lexical.TokenKeywordState ~ stateBlock ^^ {
        case _ ~ sl => DeclarationState(sl)
    }

    /**
     * A parser that matches a state block.
     */
    lazy val stateBlock: Parser[List[StatementAssignmentQualified]] = lexical.TokenKeywordBraceLeft ~ rep(statementAssignmentQualifiedList) ~ lexical.TokenKeywordBraceRight ^^ {
        case _ ~ sll ~ _ => sll.flatten
    }

    /**
     * A parser that matches an argument list.
     */
    lazy val argumentList: Parser[List[IdentifierAttributeQualified]] = identifierQualified ~ rep(lexical.TokenKeywordComma ~ identifierQualified) ^^ {
        case al ~ aList => aList.foldLeft(List(al))((parameterList, parameter) => parameter match {
            case _ ~ ar => parameterList :+ ar
        })
    }

    /**
     * A parser that matches a parameter list.
     */
    lazy val parameterList: Parser[List[(Symbol, Symbol)]] = identifierPart ~ identifierPart ~ rep(lexical.TokenKeywordComma ~ identifierPart ~ identifierPart) ^^ {
        case k ~ i ~ pList => pList.foldLeft(List((k, i)))((parameterList, parameter) => parameter match {
            case _ ~ kl ~ il => parameterList :+(kl, il)
        })
    }

    /**
     * A parser that matches a goal declaration.
     */
    lazy val goalDeclaration: Parser[DeclarationGoal] = lexical.TokenKeywordGoal ~ identifierPart ~ lexical.TokenKeywordParenthesesLeft ~ opt(parameterList) ~ lexical.TokenKeywordParenthesesRight ~ opt(literalString) ~ goalBlock ^^ {
        case _ ~ i ~ _ ~ None ~ _ ~ l ~ ss => DeclarationGoal(i, l, List(), ss)
        case _ ~ i ~ _ ~ Some(pl) ~ _ ~ l ~ ss => DeclarationGoal(i, l, pl, ss)
    }

    /**
     * A parser that matches a goal block.
     */
    lazy val goalBlock: Parser[Set[DeclarationStrategy]] = lexical.TokenKeywordBraceLeft ~ rep(strategyDeclaration) ~ lexical.TokenKeywordBraceRight ^^ {
        case _ ~ sl ~ _ => sl.toSet
    }

    /**
     * A parser that matches a strategy declaration.
     */
    lazy val strategyDeclaration: Parser[DeclarationStrategy] = lexical.TokenKeywordStrategy ~ opt(literalString) ~ strategyBlock ^^ {
        case _ ~ l ~ sl => DeclarationStrategy(l, sl)
    }

    /**
     * A parser that matches a strategy block.
     */
    lazy val strategyBlock: Parser[List[Statement]] = lexical.TokenKeywordBraceLeft ~ rep(statementSubgoal | statementQuery | statementAssignmentQualifiedList | statementNarration) ~ lexical.TokenKeywordBraceRight ^^ {
        case _ ~ sll ~ _ => sll.flatten
    }

    /**
     * A parser that matches a subgoal statement.
     */
    lazy val statementSubgoal: Parser[List[StatementSubgoal]] = identifierPart ~ lexical.TokenKeywordParenthesesLeft ~ opt(argumentList) ~ lexical.TokenKeywordParenthesesRight ^^ {
        case i ~ _ ~ Some(p) ~ _ => List(StatementSubgoal(i, p))
        case i ~ _ ~ None ~ _ => List(StatementSubgoal(i, List()))
    }

    /**
     * A parser that matches a narration statement.
     */
    lazy val statementNarration: Parser[List[StatementNarration]] = literalString ^^ {
        case e => List(StatementNarration(e))
    }

    /**
     * A parser that matches a query statement.
     */
    lazy val statementQuery: Parser[List[StatementQuery]] = opt(parameterList) ~ lexical.TokenOperatorQueryQuestionEqual ~ expression ^^ {
        case Some(pl) ~ _ ~ e => List(StatementQuery(pl, None, Some(e)))
        case None ~ _ ~ e => List(StatementQuery(List(), None, Some(e)))

    } | opt(parameterList) ~ lexical.TokenOperatorQueryQuestion ~ expression ~ opt(lexical.TokenOperatorQueryEqual ~ expression) ^^ {
        case Some(pl) ~ _ ~ ea ~ Some(_ ~ ee) => List(StatementQuery(pl, Some(ea), Some(ee)))
        case None ~ _ ~ ea ~ Some(_ ~ ee) => List(StatementQuery(List(), Some(ea), Some(ee)))
        case Some(pl) ~ _ ~ ea ~ None => List(StatementQuery(pl, Some(ea), None))
        case None ~ _ ~ ea ~ None => List(StatementQuery(List(), Some(ea), None))
    }

    /**
     * A parser that matches a block of qualified assignment statements.
     */
    lazy val statementAssignmentQualifiedList: Parser[List[StatementAssignmentQualified]] = statementAssignmentEntity | statementAssignmentRelationship | statementAssignmentQualified

    /**
     * A parser that matches a qualified assignment statement.
     */
    lazy val statementAssignmentQualified: Parser[List[StatementAssignmentQualified]] = identifierQualified ~ operatorAssignment ~ expression ^^ {
        case i ~ (op: lexical.TokenOperatorAssignment) ~ e => List(StatementAssignmentQualified(i, e, operatorAssignmentMap(op)))
    }

    /**
     * A parser that matches a relationship declaration.
     */
    lazy val statementAssignmentRelationship: Parser[List[StatementAssignmentQualified]] = identifierRelationship ~ statementAssignmentUnqualifiedBlock ^^ {
        case i ~ sl => sl.map(s => StatementAssignmentQualified(IdentifierAttributeQualified(i, s.left.symbolSequence), s.right, s.op))
    }

    /**
     * A parser that matches an entity declaration.
     */
    lazy val statementAssignmentEntity: Parser[List[StatementAssignmentQualified]] = identifierPart ~ identifierPart ~ statementAssignmentUnqualifiedBlock ^^ {
        case k ~ i ~ sl => sl.map(s => StatementAssignmentQualified(IdentifierAttributeQualified(IdentifierEntity(Some(k), i), s.left.symbolSequence), s.right, s.op))
    }

    /**
     * A parser that matches a block of unqualified assignment statements.
     */
    lazy val statementAssignmentUnqualifiedBlock: Parser[List[StatementAssignmentUnqualified]] = lexical.TokenKeywordBraceLeft ~ rep(statementAssignmentUnqualified) ~ lexical.TokenKeywordBraceRight ^^ {
        case _ ~ sl ~ _ => sl
    }

    /**
     * A parser that matches an unqualified assignment statement.
     */
    lazy val statementAssignmentUnqualified: Parser[StatementAssignmentUnqualified] = identifierUnqualified ~ operatorAssignment ~ expression ^^ {
        case i ~ (op: lexical.TokenOperatorAssignment) ~ e => StatementAssignmentUnqualified(i, e, operatorAssignmentMap(op))
    }

    /**
     * A parser that matches an assignment operator.
     */
    lazy val operatorAssignment: Parser[Elem] = lexical.TokenOperatorAssignmentUpdate | lexical.TokenOperatorAssignmentInsert | lexical.TokenOperatorAssignmentRemove

    /**
     * A parser that matches a boolean or expression.
     */
    lazy val expression: Parser[Expression] = expressionConditionalAnd ~ rep(lexical.TokenOperatorOr ~ expressionConditionalAnd) ^^ {
        case el ~ eList => eList.foldLeft(el)((expression, operationExpression) => operationExpression match {
            case (op: lexical.TokenOperatorBinary) ~ er => ExpressionBinary(expression, er, operatorBinaryMap(op))
        })
    }

    /**
     * A parser that matches a boolean and expression.
     */
    lazy val expressionConditionalAnd: Parser[Expression] = expressionEquality ~ rep(lexical.TokenOperatorAnd ~ expressionEquality) ^^ {
        case el ~ eList => eList.foldLeft(el)((expression, operationExpression) => operationExpression match {
            case (op: lexical.TokenOperatorBinary) ~ er => ExpressionBinary(expression, er, operatorBinaryMap(op))
        })
    }

    /**
     * A parser that matches an equality expression.
     */
    lazy val expressionEquality: Parser[Expression] = expressionRelational ~ rep(operatorEquality ~ expressionRelational) ^^ {
        case el ~ eList => eList.foldLeft(el)((expression, operationExpression) => operationExpression match {
            case (op: lexical.TokenOperatorBinary) ~ er => ExpressionBinary(expression, er, operatorBinaryMap(op))
        })
    }

    /**
     * A parser that matches an equality operator.
     */
    lazy val operatorEquality: Parser[Elem] = lexical.TokenOperatorEqual | lexical.TokenOperatorNotEqual

    /**
     * A parser that matches a relational expression.
     */
    lazy val expressionRelational: Parser[Expression] = expressionSet ~ rep(operatorRelational ~ expressionSet) ^^ {
        case el ~ eList => eList.foldLeft(el)((expression, operationExpression) => operationExpression match {
            case (op: lexical.TokenOperatorBinary) ~ er => ExpressionBinary(expression, er, operatorBinaryMap(op))
        })
    }

    /**
     * A parser that matches a relational operator.
     */
    lazy val operatorRelational: Parser[Elem] = lexical.TokenOperatorLessThan | lexical.TokenOperatorGreaterThan | lexical.TokenOperatorLessThanOrEqual | lexical.TokenOperatorGreaterThanOrEqual | lexical.TokenOperatorSubset | lexical.TokenOperatorSuperset

    /**
     * A parser that matches a set expression.
     */
    lazy val expressionSet: Parser[Expression] = expressionAdditive ~ rep(operatorSet ~ expressionAdditive) ^^ {
        case el ~ eList => eList.foldLeft(el)((expression, operationExpression) => operationExpression match {
            case (op: lexical.TokenOperatorBinary) ~ er => ExpressionBinary(expression, er, operatorBinaryMap(op))
        })
    }

    /**
     * A parser that matches a set operator.
     */
    lazy val operatorSet: Parser[Elem] = lexical.TokenOperatorUnion | lexical.TokenOperatorDifference | lexical.TokenOperatorIntersection

    /**
     * A parser that matches an additive expression.
     */
    lazy val expressionAdditive: Parser[Expression] = expressionMultiplicative ~ rep(operatorAdditive ~ expressionMultiplicative) ^^ {
        case el ~ eList => eList.foldLeft(el)((expression, operationExpression) => operationExpression match {
            case (op: lexical.TokenOperatorBinary) ~ er => ExpressionBinary(expression, er, operatorBinaryMap(op))
        })
    }

    /**
     * A parser that matches an addition or subtraction operator.
     */
    lazy val operatorAdditive: Parser[Elem] = lexical.TokenOperatorAddition | lexical.TokenOperatorSubtraction

    /**
     * A parser that matches a multiplication or division expression.
     */
    lazy val expressionMultiplicative: Parser[Expression] = expressionUnary ~ rep(operatorMultiplicative ~ expressionUnary) ^^ {
        case el ~ eList => eList.foldLeft(el)((expression, operationExpression) => operationExpression match {
            case (op: lexical.TokenOperatorBinary) ~ er => ExpressionBinary(expression, er, operatorBinaryMap(op))
        })
    }

    /**
     * A parser that matches a multiplication or division operator.
     */
    lazy val operatorMultiplicative: Parser[Elem] = lexical.TokenOperatorMultiplication | lexical.TokenOperatorDivision

    /**
     * A parser that matches a unary expression.
     */
    lazy val expressionUnary: Parser[Expression] = opt(operatorUnary) ~ expressionPostfix ^^ {
        case None ~ e => e
        case Some(op: lexical.TokenOperatorUnary) ~ e => ExpressionUnary(e, operatorUnaryMap(op))
    }

    /**
     * A parser that matches a unary operator.
     */
    lazy val operatorUnary: Parser[Elem] = lexical.TokenOperatorNot | lexical.TokenOperatorCardinality

    /**
     * A parser that matches a postfix expression.
     */
    lazy val expressionPostfix: Parser[Expression] = expressionAtom ~ opt(operatorPostfix) ^^ {
        case e ~ None => e
        case e ~ Some(op: lexical.TokenOperatorUnary) => ExpressionUnary(e, operatorUnaryMap(op))
    }

    /**
     * A parser that matches a postfix operator.
     */
    lazy val operatorPostfix: Parser[Elem] = lexical.TokenOperatorDecrement | lexical.TokenOperatorDecrementToMinimum | lexical.TokenOperatorIncrementToMaximum | lexical.TokenOperatorIncrement

    /**
     * A parser that matches an atom.
     */
    lazy val expressionAtom: Parser[Expression] = literal | identifierQualified | expressionEnclosed

    /**
     * A parser that matches an enclosed expression.
     */
    lazy val expressionEnclosed: Parser[Expression] = lexical.TokenKeywordParenthesesLeft ~ expression ~ lexical.TokenKeywordParenthesesRight ^^ {
        case _ ~ e ~ _ => e
    }

    /**
     * A parser that matches an enclosed relationship identifier.
     */
    lazy val identifierRelationshipEnclosed: Parser[IdentifierRelationship] = lexical.TokenKeywordParenthesesLeft ~ identifierRelationship ~ lexical.TokenKeywordParenthesesRight ^^ {
        case _ ~ r ~ _ => r
    }

    /**
     * A parser that matches a relationship identifier.
     */
    lazy val identifierRelationship: Parser[IdentifierRelationship] = identifierRelationshipBidirectional | identifierRelationshipUnidirectional

    /**
     * A parser that matches a unidirectional relationship identifier.
     */
    lazy val identifierRelationshipUnidirectional: Parser[IdentifierRelationshipUnidirectional] = identifierQualified ~ lexical.TokenKeywordRelationshipUnidirectional ~ identifierQualified ^^ {
        case left ~ _ ~ right => IdentifierRelationshipUnidirectional(left, right)
    }

    /**
     * A parser that matches an bidirectional relationship identifier.
     */
    lazy val identifierRelationshipBidirectional: Parser[IdentifierRelationshipBidirectional] = identifierQualified ~ lexical.TokenKeywordRelationshipBidirectional ~ identifierQualified ^^ {
        case left ~ _ ~ right => IdentifierRelationshipBidirectional(left, right)
    }

    /**
     * A parser that matches a qualified identifier.
     */
    lazy val identifierQualified: Parser[IdentifierAttributeQualified] = identifierRelationshipEnclosed ~ rep1(lexical.TokenKeywordScope ~ identifierPart) ^^ {
        case iL ~ iList => iList.foldLeft(IdentifierAttributeQualified(iL, Seq()))((identifierAttributeQualified, operatorIdentifierPart) => operatorIdentifierPart match {
            case _ ~ ir => IdentifierAttributeQualified(identifierAttributeQualified.owner, identifierAttributeQualified.symbolSequence :+ ir)
        })
    } | identifierPart ~ rep(lexical.TokenKeywordScope ~ identifierPart) ^^ {
        case iL ~ iList => iList.foldLeft(IdentifierAttributeQualified(IdentifierEntity(None, iL), Seq()))((identifierAttributeQualified, operatorIdentifierPart) => operatorIdentifierPart match {
            case _ ~ ir => IdentifierAttributeQualified(identifierAttributeQualified.owner, identifierAttributeQualified.symbolSequence :+ ir)
        })
    }

    /**
     * A parser that matches an unqualified identifier.
     */
    lazy val identifierUnqualified: Parser[IdentifierAttributeUnqualified] = identifierPart ~ rep(lexical.TokenKeywordScope ~ identifierPart) ^^ {
        case iL ~ iList => iList.foldLeft(IdentifierAttributeUnqualified(Seq(iL)))((identifierAttributeUnqualified, operatorIdentifierPart) => operatorIdentifierPart match {
            case _ ~ ir => IdentifierAttributeUnqualified(identifierAttributeUnqualified.symbolSequence :+ ir)
        })
    }

    /**
     * A parser that matches an identifier part.
     */
    lazy val identifierPart: Parser[Symbol] = elem("identifier", _.isInstanceOf[TokenIdentifier]) ^^ {
        case lexical.TokenIdentifier(i) => Symbol(i)
    }

    /**
     * A parser that matches a literal: a boolean, empty set, number, or string literal.
     */
    lazy val literal: Parser[Literal] = literalBoolean | literalEmpty | literalGender | literalNumber | literalString | failure("literal expected")

    /**
     * A parser that matches a boolean literal.
     */
    lazy val literalBoolean: Parser[LiteralBoolean] = literalBooleanFalse | literalBooleanTrue | failure("boolean expected")

    /**
      * A parser that matches the boolean literal false.
      */
    lazy val literalBooleanFalse: Parser[LiteralBoolean] = lexical.TokenLiteralBooleanTrue ^^^ LiteralBoolean(value = true)

    /**
      * A parser that matches the boolean literal true.
      */
    lazy val literalBooleanTrue: Parser[LiteralBoolean] = lexical.TokenLiteralBooleanFalse ^^^ LiteralBoolean(value = false)

    /**
      * A parser that matches a gender literal.
      */
    lazy val literalGender: Parser[LiteralGender] = literalGenderMale | literalGenderFemale | literalGenderNeuter | failure("gender expected")

    /**
      * A parser that matches the gender literal feminine.
      */
    lazy val literalGenderFemale: Parser[LiteralGender] = lexical.TokenLiteralGenderMale ^^^ LiteralGender(value = Male)

    /**
      * A parser that matches the gender literal masculine.
      */
    lazy val literalGenderMale: Parser[LiteralGender] = lexical.TokenLiteralGenderFemale ^^^ LiteralGender(value = Female)

    /**
      * A parser that matches the gender literal neuter.
      */
    lazy val literalGenderNeuter: Parser[LiteralGender] = lexical.TokenLiteralGenderFemale ^^^ LiteralGender(value = Neuter)

    /**
     * A parser that matches the empty set literal.
     */
    lazy val literalEmpty: Parser[LiteralEmpty.type] = lexical.TokenLiteralEmpty ^^^ LiteralEmpty

    /**
     * A parser that matches a number literal.
     */
    lazy val literalNumber: Parser[LiteralNumber] = elem("number", _.isInstanceOf[TokenLiteralNumber]) ^^ {
        case lexical.TokenLiteralNumber(n) => LiteralNumber(BigDecimal(n))
    }

    /**
     * A parser that matches a string literal.
     */
    lazy val literalString: Parser[LiteralString] = elem("string", _.isInstanceOf[TokenLiteralString]) ^^ {
        case lexical.TokenLiteralString(s) => LiteralString(s)
    }

    /**
     * A map from a token that represents an assignment operator to the assignment operator itself.
     */
    val operatorAssignmentMap: Map[Elem, OperatorAssignment] = Map(
        lexical.TokenOperatorAssignmentInsert -> OperatorAssignmentInsert,
        lexical.TokenOperatorAssignmentRemove -> OperatorAssignmentRemove,
        lexical.TokenOperatorAssignmentUpdate -> OperatorAssignmentUpdate
    )
    /**
     * A map from a token that represents a binary operator to the binary operator itself.
     */
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

    /**
     * A map from a token that represents a unary operator to the unary operator itself.
     */
    val operatorUnaryMap: Map[Elem, OperatorUnary] = Map(
        lexical.TokenOperatorCardinality -> OperatorCardinality,
        lexical.TokenOperatorDecrement -> OperatorDecrement,
        lexical.TokenOperatorDecrementToMinimum -> OperatorDecrementToMinimum,
        lexical.TokenOperatorIncrement -> OperatorIncrement,
        lexical.TokenOperatorIncrementToMaximum -> OperatorIncrementToMaximum,
        lexical.TokenOperatorNot -> OperatorNot
    )

}
