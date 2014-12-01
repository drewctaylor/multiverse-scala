package edu.gatech.dt87.multiverse.story.dsl.parser

/**
 * A declaration: either a story, a goal, a strategy, a parameter, or a state component.
 */
sealed trait Declaration

case class DeclarationEntityKind(kind: ExpressionIdentifier, block: BlockAssignment) extends Declaration

/**
 * A story declaration consists of a state declaration and a set of goal declarations.
 * @param state a state declaration
 * @param goalList a set of goal declarations.
 */
case class DeclarationStory(state: DeclarationState, goalList: List[DeclarationGoal]) extends Declaration

/**
 * A goal declaration consists of a goal identifier, a parameter list, and a goal block.
 *
 * @param identifier a goal identifier
 * @param label a goal label
 * @param parameterList a parameter list
 * @param block a goal block
 */
case class DeclarationGoal(identifier: ExpressionIdentifier, label: Option[ExpressionLiteralString], parameterList: List[DeclarationParameter], block: BlockGoal) extends Declaration

/**
 * A strategy declaration consists of a strategy block.
 *
 * @param block the strategy block
 */
case class DeclarationStrategy(label: Option[ExpressionLiteralString], block: BlockStrategy) extends Declaration

/**
 * A parameter declaration consists of the kind of the entity and the entity identifier.
 *
 * @param kind the kind of the entity
 * @param parameter the entity identifier
 */
case class DeclarationParameter(kind: ExpressionIdentifier, parameter: ExpressionIdentifier) extends Declaration

/**
 * A state declaration is a block of data and attribute declarations.
 *
 * @param block a block of data and attribute declarations
 */
case class DeclarationState(block: BlockState) extends Declaration

/**
 * A state component declaration: either a data declaration of an attribute declaration.
 */
sealed trait DeclarationStateComponent extends Declaration

/**
 * A data declaration: either an entity declaration or a relationship declaration.
 */
sealed trait DeclarationData extends DeclarationStateComponent

/**
 * An entity declaration consists of an entity and the attributes associated with the entity.
 *
 * @param kind the kind of the entity
 * @param entity the entity
 * @param block the attributes associated with the entity
 */
case class DeclarationEntity(kind: ExpressionIdentifier, entity: ExpressionEntity, block: BlockAssignment) extends DeclarationData

/**
 * A relationship declaration consists of a relationship and the attributes associated with the relationship .
 *
 * @param relationship the relationship
 * @param block the attributes associated with the relationship
 */
case class DeclarationRelationship(relationship: ExpressionRelationship, block: BlockAssignment) extends DeclarationData

///**
// * An attribute declaration.
// */
//sealed trait DeclarationAttribute extends DeclarationStateComponent
//
///**
// * A declaration of an attribute that is one of the set of all symbols.
// *
// * @param attribute the attribute name.
// */
//case class DeclarationAttributeOneSymbol(attribute: Symbol) extends DeclarationAttribute
//
///**
// * A declaration of an attribute that is one of an unordered set of symbols.
// *
// * @param attribute the attribute name
// * @param block the unordered set of symbols
// */
//case class DeclarationAttributeOneUnordered(attribute: Symbol, block: BlockLiteral) extends DeclarationAttribute
//
///**
// * A declaration of an attribute that is one of an ordered set of symbols.
// *
// * @param attribute the attribute name
// * @param block the ordered set of symbols
// */
//case class DeclarationAttributeOneOrdered(attribute: Symbol, block: BlockLiteral) extends DeclarationAttribute
//
///**
// * A declaration for an attribute that is a subset of the set of all symbols.
// *
// * @param attribute the attribute name
// */
//case class DeclarationAttributeSubsetSymbol(attribute: Symbol) extends DeclarationAttribute
//
///**
// * A declaration for an attribute that is a subset of an unordered set of symbols.
// *
// * @param attribute the attribute name
// * @param block the unordered set of symbols
// */
//case class DeclarationAttributeSubsetUnordered(attribute: Symbol, block: BlockLiteral) extends DeclarationAttribute
//
///**
// * A declaration for an attribute that is a subset of an ordered set of symbols.
// *
// * @param attribute the attribute name
// * @param block the ordered set of symbols
// */
//case class DeclarationAttributeSubsetOrdered(attribute: Symbol, block: BlockLiteral) extends DeclarationAttribute
//
