package edu.gatech.dt87.multiverse.story.dsl.parser

sealed trait Statement

/**
 * A story statement consists of a story title and an optional random seed.
 */
case class StatementStory(title: LiteralString, seed: Option[LiteralNumber]) extends Statement

/**
 * An assignment statement is either a qualified assignment statement or an unqualified assignment statement.
 */
sealed trait StatementAssignment extends Statement {
    /**
     * @return right the expression
     */
    def right: Expression

    /**
     * @return op the operator
     */
    def op: OperatorAssignment
}

/**
 * An qualified assignment statement consists of a qualified attribute identifier, an expression, and an assignment operator.
 *
 * @param left the qualified attribute identifier
 * @param right the expression
 * @param op the operator
 */
case class StatementAssignmentQualified(left: IdentifierAttributeQualified, right: Expression, op: OperatorAssignment) extends StatementAssignment

/**
 * An unqualified assignment statement consists of an unqualified attribute identifier, an expression, and an assignment operator.
 *
 * @param left the unqualified attribute identifier
 * @param right the expression
 * @param op the operator
 */
case class StatementAssignmentUnqualified(left: IdentifierAttributeUnqualified, right: Expression, op: OperatorAssignment) extends StatementAssignment

/**
 * A narration statement consists of a literal string expression.
 *
 * @param string a literal string expression
 */
case class StatementNarration(string: LiteralString) extends Statement

/**
 * A query statement consists of a list of parameter declarations, an all expression (an expression which must be true
 * for all combinations of parameters), and an exists expression (an expression which must be true for at least one
 * combination of parameters).
 *
 * @param left a list of parameter declarations
 * @param all an all expression
 * @param exists an exists expression
 */
case class StatementQuery(left: List[(Symbol, Symbol)], all: Option[Expression], exists: Option[Expression]) extends Statement

/**
 * A subgoal statement consists of a goal and a list of parameters.
 *
 * @param goal the goal
 * @param parameterList the list of parameters
 */
case class StatementSubgoal(goal: Symbol, parameterList: List[IdentifierAttributeQualified]) extends Statement

