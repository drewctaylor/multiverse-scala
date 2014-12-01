package edu.gatech.dt87.multiverse.story.dsl.parser

sealed trait Operator

sealed trait OperatorTerminal extends Operator

sealed trait OperatorAssignment extends OperatorTerminal

case object OperatorAssignmentUpdate extends OperatorAssignment

case object OperatorAssignmentInsert extends OperatorAssignment

case object OperatorAssignmentRemove extends OperatorAssignment

sealed trait OperatorQuery extends OperatorTerminal

case object OperatorQueryAll extends OperatorQuery

case object OperatorQueryNone extends OperatorQuery

sealed trait OperatorNonterminal extends Operator

sealed trait OperatorUnary extends OperatorNonterminal

case object OperatorDecrement extends OperatorUnary

case object OperatorDecrementToMinimum extends OperatorUnary

case object OperatorIncrement extends OperatorUnary

case object OperatorIncrementToMaximum extends OperatorUnary

case object OperatorNot extends OperatorUnary

case object OperatorCardinality extends OperatorUnary

sealed trait OperatorBinary extends OperatorNonterminal

case object OperatorOr extends OperatorBinary

case object OperatorAnd extends OperatorBinary

case object OperatorAddition extends OperatorBinary

case object OperatorSubtraction extends OperatorBinary

case object OperatorMultiplication extends OperatorBinary

case object OperatorDivision extends OperatorBinary

case object OperatorLessThan extends OperatorBinary

case object OperatorLessThanOrEqual extends OperatorBinary

case object OperatorGreaterThan extends OperatorBinary

case object OperatorGreaterThanOrEqual extends OperatorBinary

case object OperatorSubset extends OperatorBinary

case object OperatorSuperset extends OperatorBinary

case object OperatorUnion extends OperatorBinary

case object OperatorDifference extends OperatorBinary

case object OperatorIntersection extends OperatorBinary

case object OperatorEqual extends OperatorBinary

case object OperatorNotEqual extends OperatorBinary
