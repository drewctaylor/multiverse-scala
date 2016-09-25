package edu.gatech.dt87.multiverse.language.parser

/**
 * An expression is an operator expression, a literal expression, or an identifier expression.
 */
sealed trait Expression

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
case class ExpressionUnary(expression: Expression, operator: OperatorUnary) extends ExpressionOperator {
    override def toString: String = s"($operator ${expression.toString})"
}


/**
 * A binary expression consists of a left-expression, a right-expression, and a binary operator.
 *
 * @param left the left expression
 * @param right the right expression
 * @param operator the binary operator
 */
case class ExpressionBinary(left: Expression, right: Expression, operator: OperatorBinary) extends ExpressionOperator {
    override def toString: String = s"(${left.toString} $operator ${right.toString})"
}

/**
 * An identifier is an attribute or an attribute owner.
 */
sealed trait Identifier extends Expression

/**
 * An attribute owner identifier is either an entity or a relationship.
 */
sealed trait IdentifierAttributeOwner extends Identifier

/**
 * An entity identifier consists of a symbol.
 *
 * @param symbol the symbol for the entity identifier
 */
case class IdentifierEntity(kind: Option[Symbol] = None, symbol: Symbol) extends IdentifierAttributeOwner {
    override def toString: String = symbol.name
}

/**
 * A relationship identifier is either a unidirectional or bidirectional relationship identifier.
 */
sealed trait IdentifierRelationship extends IdentifierAttributeOwner {
    /**
     * @return the left-identifier
     */
    def left: IdentifierAttributeQualified

    /**
     * @return the right-identifier
     */
    def right: IdentifierAttributeQualified
}

/**
 * A bidirectional relationship identifier consists of a left owner identifier and a right owner identifier.
 *
 * @param left the left-identifier
 * @param right the right-identifier
 */
case class IdentifierRelationshipBidirectional(left: IdentifierAttributeQualified, right: IdentifierAttributeQualified) extends IdentifierRelationship {
    override def toString: String = s"(${left.toString}<->${right.toString})"
}

/**
 * A unidirectional relationship identifier consists of a left-identifier and a right-identifier.
 *
 * @param left the left-identifier
 * @param right the right-identifier
 */
case class IdentifierRelationshipUnidirectional(left: IdentifierAttributeQualified, right: IdentifierAttributeQualified) extends IdentifierRelationship {
    override def toString: String = s"(${left.toString}->${right.toString})"
}

/**
 * An attribute identifier is either a qualified or unqualified attribute identifier.
 */
sealed trait IdentifierAttribute extends Identifier {
    /**
     * @return a sequence of symbols identifying an attribute.
     */
    def symbolSequence: Seq[Symbol]
}

/**
 * An qualified attribute identifier consists of an attribute owner identifier and a sequence of symbols identifier an attribute.
 *
 * @param owner the attribute owner identifier
 * @param symbolSequence the sequence of symbols identifying an attribute
 */
case class IdentifierAttributeQualified(owner: IdentifierAttributeOwner, symbolSequence: Seq[Symbol]) extends IdentifierAttribute {
    override def toString: String = s"${owner.toString}${symbolSequence.foldLeft("")((string, symbol) => string + "." + symbol.name)}"
}

/**
 * An unqualified attribute identifier consists of a sequence of symbols.
 *
 * @param symbolSequence the sequence of symbols identifying an attribute
 */
case class IdentifierAttributeUnqualified(symbolSequence: Seq[Symbol]) extends IdentifierAttribute {
    override def toString: String = s"${symbolSequence.foldLeft("")((string, symbol) => string + "." + symbol.name)}"
}

/**
 * A literal: an entity, string, boolean, number, or empty set literal.
 */
sealed trait Literal extends Expression

/**
 * A boolean literal consists of either true or false.
 *
 * @param value either true or false
 */
case class LiteralBoolean(value: Boolean) extends Literal

/**
 * The empty set literal.
 */
case object LiteralEmpty extends Literal

/**
 * A number literal consists of a decimal number.
 *
 * @param value the number
 */
case class LiteralNumber(value: BigDecimal) extends Literal

/**
 * A string literal consists of a string.
 *
 * @param value the string
 */
case class LiteralString(value: String) extends Literal
