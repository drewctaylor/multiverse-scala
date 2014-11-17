package edu.gatech.dt87.multiverse.story.dsl

/**
 * A block is a sequence of statements or declarations.
 */
sealed trait Block

/**
* An assignment block is a sequence of assignment statements.
*
* @param assignmentList a sequence of assignment statements.
*/
case class BlockAssignment(assignmentList: List[StatementAssignment]) extends Block

/**
 * A literal block is a sequence of symbol literals.
 *
 * @param literalList a sequence of symbol literals.
 */
case class BlockLiteral(literalList: List[ExpressionLiteralSymbol]) extends Block

/**
 * A state block is a sequence of state component declarations: attribute, entity, and relationship declarations.
 *
 * @param declarationList a sequence of state component declarations
 */
case class BlockState(declarationList: List[DeclarationStateComponent]) extends Block

/**
 * A strategy block is a sequence of statements: assignment, data declaration, narration, query, and satisfaction statements.
 *
 * @param statementList a sequence of statements
 */
case class BlockStrategy(statementList: List[Statement]) extends Block

/**
 * A goal block is a set of strategy declarations.
 *
 * @param strategyList a set of strategy declarations.
 */
case class BlockGoal(strategyList: List[DeclarationStrategy]) extends Block
