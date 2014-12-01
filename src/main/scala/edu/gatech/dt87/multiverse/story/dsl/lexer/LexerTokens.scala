package edu.gatech.dt87.multiverse.story.dsl.lexer

import scala.util.parsing.combinator.token.Tokens

trait LexerTokens extends Tokens {

    sealed abstract class TokenAbstract extends Token

    sealed trait TokenOperatorAssignment

    sealed trait TokenOperatorQuery

    sealed trait TokenOperatorBinary

    sealed trait TokenOperatorUnary

    class TokenReserved(cs: String*) extends TokenAbstract {
        val invalid = cs.filter(!cs.filter(chars =>
            chars.forall(!_.isWhitespace) && (
                chars.forall(_.isLetter) ||
                    chars.forall(!_.isLetterOrDigit))
        ).contains(_))

        if (invalid.size > 0) throw new RuntimeException( s"""A reserved token must not contain whitespace and must consist of only letters or only punctuation: "${invalid.mkString("\", \"")}".""")

        def charsSeq = cs

        def chars = cs.head
    }

    val tokenReservedSet: Set[TokenReserved] = Set(
        TokenOperatorAnd,
        TokenOperatorOr,
        TokenOperatorNot,
        TokenOperatorAddition,
        TokenOperatorSubtraction,
        TokenOperatorMultiplication,
        TokenOperatorDivision,
        TokenOperatorIncrement,
        TokenOperatorIncrementToMaximum,
        TokenOperatorDecrement,
        TokenOperatorDecrementToMinimum,
        TokenOperatorLessThan,
        TokenOperatorLessThanOrEqual,
        TokenOperatorGreaterThan,
        TokenOperatorGreaterThanOrEqual,
        TokenOperatorCardinality,
        TokenOperatorSubset,
        TokenOperatorSuperset,
        TokenOperatorIntersection,
        TokenOperatorUnion,
        TokenOperatorDifference,
        TokenOperatorEqual,
        TokenOperatorNotEqual,
        TokenOperatorAssignmentInsert,
        TokenOperatorAssignmentRemove,
        TokenOperatorAssignmentUpdate,
        TokenOperatorQueryAll,
        TokenOperatorQueryNone,
        TokenKeywordStory,
        TokenKeywordState,
        TokenKeywordEntity,
        TokenKeywordGoal,
        TokenKeywordParenthesesLeft,
        TokenKeywordParenthesesRight,
        TokenKeywordBraceLeft,
        TokenKeywordBraceRight,
        TokenKeywordComma,
        TokenKeywordRelationshipBidirectional,
        TokenKeywordRelationshipUnidirectional,
        TokenKeywordScope,
        TokenKeywordStrategy,
        TokenLiteralBooleanTrue,
        TokenLiteralBooleanFalse,
        TokenLiteralEmpty)

    val tokenReservedMap: Map[String, TokenReserved] = tokenReservedSet.foldLeft(Map[String, TokenReserved]())((map, reserved) => {
        reserved.charsSeq.foldLeft(map)((m, c) => {
            m + (c -> reserved)
        })
    })

    val tokenReservedOperatorMap = tokenReservedMap.filterKeys(_.forall(!_.isLetterOrDigit))

    val tokenReservedIdentifierMap = tokenReservedMap.filterKeys(_.forall(_.isLetter))

    case object TokenOperatorAnd extends TokenReserved("and", "&&") with TokenOperatorBinary

    case object TokenOperatorOr extends TokenReserved("or", "||") with TokenOperatorBinary

    case object TokenOperatorNot extends TokenReserved("not", "!") with TokenOperatorUnary

    case object TokenOperatorAddition extends TokenReserved("+") with TokenOperatorBinary

    case object TokenOperatorSubtraction extends TokenReserved("-") with TokenOperatorBinary

    case object TokenOperatorMultiplication extends TokenReserved("*") with TokenOperatorBinary

    case object TokenOperatorDivision extends TokenReserved("/") with TokenOperatorBinary

    case object TokenOperatorIncrement extends TokenReserved("increment", "++") with TokenOperatorUnary

    case object TokenOperatorIncrementToMaximum extends TokenReserved("max", "maximum", "+++") with TokenOperatorUnary

    case object TokenOperatorDecrement extends TokenReserved("decrement", "--") with TokenOperatorUnary

    case object TokenOperatorDecrementToMinimum extends TokenReserved("min", "minimum", "---") with TokenOperatorUnary

    case object TokenOperatorLessThan extends TokenReserved("<") with TokenOperatorBinary

    case object TokenOperatorLessThanOrEqual extends TokenReserved("<=") with TokenOperatorBinary

    case object TokenOperatorGreaterThan extends TokenReserved(">") with TokenOperatorBinary

    case object TokenOperatorGreaterThanOrEqual extends TokenReserved(">=") with TokenOperatorBinary

    case object TokenOperatorCardinality extends TokenReserved("#", "cardinality") with TokenOperatorUnary

    case object TokenOperatorSubset extends TokenReserved("subset") with TokenOperatorBinary

    case object TokenOperatorSuperset extends TokenReserved("superset") with TokenOperatorBinary

    case object TokenOperatorIntersection extends TokenReserved("intersection") with TokenOperatorBinary

    case object TokenOperatorUnion extends TokenReserved("union") with TokenOperatorBinary

    case object TokenOperatorDifference extends TokenReserved("difference") with TokenOperatorBinary

    case object TokenOperatorEqual extends TokenReserved("==") with TokenOperatorBinary

    case object TokenOperatorNotEqual extends TokenReserved("!=") with TokenOperatorBinary

    case object TokenOperatorAssignmentUpdate extends TokenReserved(":=") with TokenOperatorAssignment

    case object TokenOperatorAssignmentInsert extends TokenReserved("+=") with TokenOperatorAssignment

    case object TokenOperatorAssignmentRemove extends TokenReserved("-=") with TokenOperatorAssignment

    case object TokenOperatorQueryAll extends TokenReserved("?=") with TokenOperatorQuery

    case object TokenOperatorQueryNone extends TokenReserved("!?=") with TokenOperatorQuery

    case object TokenKeywordBraceLeft extends TokenReserved("{")

    case object TokenKeywordBraceRight extends TokenReserved("}")

    case object TokenKeywordParenthesesLeft extends TokenReserved("(")

    case object TokenKeywordParenthesesRight extends TokenReserved(")")

    case object TokenKeywordRelationshipBidirectional extends TokenReserved("<->")

    case object TokenKeywordRelationshipUnidirectional extends TokenReserved("->")

    case object TokenKeywordScope extends TokenReserved(".")

    case object TokenKeywordComma extends TokenReserved(",")

    case object TokenKeywordStory extends TokenReserved("story")

    case object TokenKeywordEntity extends TokenReserved("entity")

    case object TokenKeywordState extends TokenReserved("state")

    case object TokenKeywordGoal extends TokenReserved("goal")

    case object TokenKeywordStrategy extends TokenReserved("strategy")

    case object TokenLiteralBooleanTrue extends TokenReserved("true")

    case object TokenLiteralBooleanFalse extends TokenReserved("false")

    case object TokenLiteralEmpty extends TokenReserved("empty")

    case class TokenLiteralString(chars: String) extends TokenAbstract

    case class TokenLiteralNumber(chars: String) extends TokenAbstract

    case class TokenIdentifier(chars: String) extends TokenAbstract

}