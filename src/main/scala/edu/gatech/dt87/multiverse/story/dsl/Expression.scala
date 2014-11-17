package edu.gatech.dt87.multiverse.story.dsl

/**
 * An expression is a unary expression, a binary expression, a literal string expression, a literal symbol expression,
 * or an attribute expression.
 */
sealed trait Expression

/**
 * A unary expression consists of an expression and a unary operator.
 *
 * @param expression the expression
 * @param operator the unary operator
 */
case class ExpressionUnary(expression: Expression, operator: OperatorUnary) extends Expression

/**
 * A binary expression consists of a left-expression, a right-expression, and a binary operator.
 *
 * @param left the left expression
 * @param right the right expression
 * @param operator the binary operator
 */
case class ExpressionBinary(left: Expression, right: Expression, operator: OperatorBinary) extends Expression

/**
 * A literal string expression consists of a string.
 *
 * @param string the string
 */
case class ExpressionLiteralString(string: String) extends Expression

/**
 * A literal symbol expression consists of a symbol.
 *
 * @param symbol the symbol
 */
case class ExpressionLiteralSymbol(symbol: Symbol) extends Expression

/**
 * A literal boolean expression consists of either the symbol "true" or the symbol "false".
 *
 * @param symbol the symbol
 */
case class ExpressionLiteralBoolean(symbol: Symbol) extends Expression

/**
 * A literal number expression consists of either a symbol that is wholly numeric.
 *
 * @param symbol the symbol
 */
case class ExpressionLiteralNumber(symbol: Symbol) extends Expression

/**
 * An attribute expression consists of an optional attribute owner and a symbol for an attribute.
 *
 * @param owner an optional attribute owner
 * @param attribute a symbol for an attribute
 */
case class ExpressionAttribute(owner: Option[ExpressionAttributeOwner], attribute: Symbol) extends Expression

/**
 * An attribute owner expression is either a relationship expression or an entity expression.
 */
sealed trait ExpressionAttributeOwner

/**
 * A relationship expression is either a unidirectional or bidirectional relationship expression.
 */
sealed trait ExpressionRelationship extends ExpressionAttributeOwner

/**
 * A bidirectional relationship expression consists of the symbol for a left-entity and the symbol for a right-entity.
 *
 * @param left the symbol for a left-entity
 * @param right the symbol for a right-entity
 */
case class ExpressionRelationshipBidirectional(left: Symbol, right: Symbol) extends ExpressionRelationship

/**
 * A unidirectional relationship expression consists of the symbol for a left-entity and the symbol for a right-entity.
 *
 * @param left the symbol for a left-entity
 * @param right the symbol for a right-entity
 */
case class ExpressionRelationshipUnidirectional(left: Symbol, right: Symbol) extends ExpressionRelationship

/**
 * An entity expression consists of the symbol for the entity.
 *
 * @param entity the symbol for the entity.
 */
case class ExpressionEntity(entity: Symbol) extends ExpressionAttributeOwner
