package edu.gatech.dt87.multiverse.story.dsl.parser

sealed trait Statement

/**
* An assignment statement consists of an attribute expression, an expression, and an operator.
*
* @param left the attribute expression
* @param right the expression
* @param op the operator
*/
case class StatementAssignment(left: ExpressionIdentifierSequence, right: Expression, op: OperatorAssignment) extends Statement

/**
* A narration statement consists of a literal string expression.
*
* @param string a literal string expression
*/
case class StatementNarration(string: ExpressionLiteralString) extends Statement

/**
* A data declaration statement consist of a data declaration.
*
* @param declaration a data declaration
*/
case class StatementDeclarationData(declaration : DeclarationData) extends Statement

/**
* A query statement consists of a list of parameter declarations, an expression, and an operator.
*
* @param left a list of parameter declarations
* @param right an expression
* @param op an operator
*/
case class StatementQuery(left: List[DeclarationParameter], right: Expression, op: OperatorQuery) extends Statement

/**
* A subgoal statement consists of a goal and a list of parameters.
*
* @param goal the goal
* @param parameterList the list of parameters
*/
case class StatementSubgoal(goal: ExpressionIdentifier, parameterList: List[ExpressionIdentifier]) extends Statement
