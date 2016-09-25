package edu.gatech.dt87.multiverse.language.parser

/**
 * A declaration: either a story, a state, a goal, or a strategy declaration.
 */
sealed trait Declaration

/**
 * A story declaration consists of a state declaration and a set of goal declarations.
 * @param title the title of the story
 * @param seed the random seed for the story
 * @param state a state declaration
 * @param goalList a set of goal declarations.
 */
case class DeclarationStory(title: Option[LiteralString], seed: Option[LiteralNumber], state: Option[DeclarationState], goalList: List[DeclarationGoal]) extends Declaration

/**
 * A goal declaration consists of a goal identifier, a goal label, a parameter list, and a strategy set.
 *
 * @param identifier a goal identifier
 * @param label a goal label
 * @param parameterList a parameter list
 * @param strategySet a strategy set
 */
case class DeclarationGoal(identifier: Symbol, label: Option[LiteralString], parameterList: List[(Symbol, Symbol)], strategySet: Set[DeclarationStrategy]) extends Declaration

/**
 * A strategy declaration consists of a strategy label and a statement list.
 *
 * @param label the strategy label
 * @param statementList the statement list
 */
case class DeclarationStrategy(label: Option[LiteralString], statementList: List[Statement]) extends Declaration

/**
 * A state declaration consists of an assignment list.
 *
 * @param assignmentList the assignment  list
 */
case class DeclarationState(assignmentList: List[StatementAssignmentQualified]) extends Declaration