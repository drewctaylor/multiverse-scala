package edu.gatech.dt87.multiverse.story.dsl.parser

/**
 * An expression is an operator expression, a literal expression, an identifier expression, an identifier
 * sequence expression, or an attribute owner expression.
 */
sealed trait Expression

/**
 * An identifier expression consists of a symbol for an identifier.
 *
 * @param identifier a symbol for an an identifier
 */
case class ExpressionIdentifier(identifier: Symbol) extends Expression

/**
 * An identifier expression sequence consists of an optional attribute owner followed by a sequence of identifiers
 *
 * @param owner an optional attribute owner
 * @param identifierSeq a sequence of identifiers
 */
case class ExpressionIdentifierSequence(owner: Option[ExpressionAttributeOwner], identifierSeq: Seq[ExpressionIdentifier]) extends Expression

/**
 * An attribute owner expression is either a relationship expression or an entity expression.
 */
sealed trait ExpressionAttributeOwner extends Expression

/**
 * An entity expression consist of an identifier for an entity.
 *
 * @param entity an identifier for an entity
 */
case class ExpressionEntity(entity: ExpressionIdentifier) extends ExpressionAttributeOwner

/**
 * A relationship expression is either a unidirectional or bidirectional relationship expression.
 */
sealed trait ExpressionRelationship extends ExpressionAttributeOwner {
    def left: ExpressionIdentifierSequence

    def right: ExpressionIdentifierSequence
}

/**
 * A bidirectional relationship expression consists of the identifier sequence for a left-entity and the identifier sequence for a right-entity.
 *
 * @param left the symbol for a left-entity
 * @param right the symbol for a right-entity
 */
case class ExpressionRelationshipBidirectional(left: ExpressionIdentifierSequence, right: ExpressionIdentifierSequence) extends ExpressionRelationship

/**
 * A unidirectional relationship expression consists of the identifier sequence for a left-entity and the identifier sequence for a right-entity.
 *
 * @param left the symbol for a left-entity
 * @param right the symbol for a right-entity
 */
case class ExpressionRelationshipUnidirectional(left: ExpressionIdentifierSequence, right: ExpressionIdentifierSequence) extends ExpressionRelationship

/**
 * A literal expression: a literal string, a literal number, a literal boolean, or a literal empty set.
 */
sealed trait ExpressionLiteral extends Expression

/**
 * A literal string expression consists of a string.
 *
 * @param value the string
 */
case class ExpressionLiteralString(value: String) extends ExpressionLiteral

/**
 * A literal boolean expression consists of either true or false.
 *
 * @param value either true or false
 */
case class ExpressionLiteralBoolean(value: Boolean) extends ExpressionLiteral

/**
 * A literal number expression consists of a decimal number.
 *
 * @param value the number
 */
case class ExpressionLiteralNumber(value: BigDecimal) extends ExpressionLiteral

/**
 * A literal empty set consists of an empty set.
 */
object ExpressionLiteralEmpty extends ExpressionLiteral

/**
 * An operator expression: a unary expression or a binary expression.
 */
sealed trait ExpressionOperator extends Expression

/**
 * A unary expression consists of an expression and a unary operator.
 *
 * @param expression the expression
 * @param operator the unary operator
 */
case class ExpressionUnary(expression: Expression, operator: OperatorUnary) extends ExpressionOperator

/**
 * A binary expression consists of a left-expression, a right-expression, and a binary operator.
 *
 * @param left the left expression
 * @param right the right expression
 * @param operator the binary operator
 */
case class ExpressionBinary(left: Expression, right: Expression, operator: OperatorBinary) extends ExpressionOperator
