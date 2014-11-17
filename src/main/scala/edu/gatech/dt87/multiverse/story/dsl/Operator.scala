package edu.gatech.dt87.multiverse.story.dsl

sealed trait Operator {
    val symbol: String
}

sealed trait OperatorTerminal extends Operator

sealed abstract class OperatorAssignment(s: String) extends OperatorTerminal {
    val symbol = s
}

case object OperatorAssignmentUpdate extends OperatorAssignment(":=")

case object OperatorAssignmentInsert extends OperatorAssignment("+=")

case object OperatorAssignmentRemove extends OperatorAssignment("-=")

sealed abstract class OperatorQuery(s: String) extends OperatorTerminal {
    val symbol = s
}

case object OperatorQueryAll extends OperatorQuery("?=")

case object OperatorQueryNone extends OperatorQuery("!?=")

sealed trait OperatorNonterminal extends Operator

sealed abstract class OperatorUnary(s: String) extends OperatorNonterminal {
    val symbol = s
}

case object OperatorDecrement extends OperatorUnary("--")

case object OperatorDecrementToMinimum extends OperatorUnary("---")

case object OperatorIncrement extends OperatorUnary("++")

case object OperatorIncrementToMaximum extends OperatorUnary("+++")

case object OperatorNot extends OperatorUnary("!")

sealed abstract class OperatorBinary(s: String) extends OperatorNonterminal {
    val symbol = s
}

case object OperatorOr extends OperatorBinary("||")

case object OperatorAnd extends OperatorBinary("&&")

case object OperatorEqual extends OperatorBinary("==")

case object OperatorNotEqual extends OperatorBinary("!=")

case object OperatorSubset extends OperatorBinary("[")

case object OperatorSuperset extends OperatorBinary("]")

case object OperatorLessThan extends OperatorBinary("<")

case object OperatorLessThanOrEqual extends OperatorBinary("<=")

case object OperatorGreaterThan extends OperatorBinary(">")

case object OperatorGreaterThanOrEqual extends OperatorBinary(">=")

case object OperatorUnion extends OperatorBinary("+")

case object OperatorDifference extends OperatorBinary("-")

case object OperatorIntersection extends OperatorBinary("^")