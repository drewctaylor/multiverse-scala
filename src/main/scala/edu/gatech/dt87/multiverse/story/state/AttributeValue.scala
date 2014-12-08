package edu.gatech.dt87.multiverse.story.state

import edu.gatech.dt87.multiverse.story.StateStrategyStep
import edu.gatech.dt87.multiverse.story.dsl.parser._

import scala.collection.immutable.Set

sealed trait AttributeValue

case class AttributeValueBoolean(boolean: Boolean) extends AttributeValue

case class AttributeValueEntity(entity: (Symbol, Int)) extends AttributeValue

case class AttributeValueNumber(number: BigDecimal) extends AttributeValue

case class AttributeValueSymbol(symbol: Symbol) extends AttributeValue

//case class AttributeValueSymbolOrdered(symbol: Symbol, symbolSequence: Seq[Symbol]) extends AttributeValue

object AttributeValueOperation {

    def bbb(op: (Boolean, Boolean) => Boolean)(right: Option[Set[AttributeValue]])(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = (left.map(_.toSeq), right.map(_.toSeq)) match {
        case (Some(Seq(AttributeValueBoolean(l))), Some(Seq(AttributeValueBoolean(r)))) => Some(Set(AttributeValueBoolean(op(l, r))))
        case _ => None
    }

    def nnb(op: (BigDecimal, BigDecimal) => Boolean)(right: Option[Set[AttributeValue]])(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = (left.map(_.toSeq), right.map(_.toSeq)) match {
        case (Some(Seq(AttributeValueNumber(l))), Some(Seq(AttributeValueNumber(r)))) => Some(Set(AttributeValueBoolean(op(l, r))))
        case _ => None
    }

    def nnn(op: (BigDecimal, BigDecimal) => BigDecimal)(right: Option[Set[AttributeValue]])(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = (left.map(_.toSeq), right.map(_.toSeq)) match {
        case (Some(Seq(AttributeValueNumber(l))), Some(Seq(AttributeValueNumber(r)))) => Some(Set(AttributeValueNumber(op(l, r))))
        case _ => None
    }

    def ssb(op: (Set[AttributeValue], Set[AttributeValue]) => Boolean)(right: Option[Set[AttributeValue]])(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = {
        for (l <- left; r <- right) yield Set(AttributeValueBoolean(op(l, r)))
    }

    def sss(op: (Set[AttributeValue], Set[AttributeValue]) => Set[AttributeValue])(right: Option[Set[AttributeValue]])(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = {
        for (l <- left; r <- right) yield op(l, r)
    }

    def and(right: Option[Set[AttributeValue]])(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = {
        bbb((l, r) => l && r)(right)(left)
    }

    def or(right: Option[Set[AttributeValue]])(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = {
        bbb((l, r) => l || r)(right)(left)
    }

    def not(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = left.map(_.toSeq) match {
        case Some(Seq(AttributeValueBoolean(l))) => Some(Set(AttributeValueBoolean(!l)))
        case _ => None
    }

    def greaterThan(right: Option[Set[AttributeValue]])(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = {
        nnb((l, r) => l > r)(right)(left)
    }

    def greaterThanOrEqual(right: Option[Set[AttributeValue]])(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = {
        nnb((l, r) => l >= r)(right)(left)
    }

    def lessThan(right: Option[Set[AttributeValue]])(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = {
        nnb((l, r) => l < r)(right)(left)
    }

    def lessThanOrEqual(right: Option[Set[AttributeValue]])(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = {
        nnb((l, r) => l <= r)(right)(left)
    }

    def add(right: Option[Set[AttributeValue]])(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = {
        nnn((l, r) => l + r)(right)(left)
    }

    def subtract(right: Option[Set[AttributeValue]])(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = {
        nnn((l, r) => l - r)(right)(left)
    }

    def multiply(right: Option[Set[AttributeValue]])(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = {
        nnn((l, r) => l * r)(right)(left)
    }

    def divide(right: Option[Set[AttributeValue]])(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = {
        nnn((l, r) => l / r)(right)(left)
    }

    def decrement(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = left.map(_.toSeq) match {
        case Some(Seq(AttributeValueNumber(l))) => Some(Set(AttributeValueNumber(l - 1)))
//        case Some(Seq(AttributeValueSymbolOrdered(s, ss))) => Some(Set(AttributeValueSymbolOrdered(ss((ss.indexOf(s) - 1) max 0), ss)))
        case _ => None
    }

    def increment(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = {
        left.map(_.toSeq) match {
            case Some(Seq(AttributeValueNumber(l))) => Some(Set(AttributeValueNumber(l + 1)))
//            case Some(Seq(AttributeValueSymbolOrdered(s, ss))) => Some(Set(AttributeValueSymbolOrdered(ss((ss.indexOf(s) + 1) min (ss.size - 1)), ss)))
            case _ => None
        }
    }

    def decrementToMinimum(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = left.map(_.toSeq) match {
//        case Some(Seq(AttributeValueSymbolOrdered(s, ss))) => Some(Set(AttributeValueSymbolOrdered(ss.head, ss)))
        case _ => None
    }

    def incrementToMaximum(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = left.map(_.toSeq) match {
//        case Some(Seq(AttributeValueSymbolOrdered(s, ss))) => Some(Set(AttributeValueSymbolOrdered(ss.last, ss)))
        case _ => None
    }

    def equal(right: Option[Set[AttributeValue]])(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = {
        ssb((l, r) => l == r)(right)(left)
    }

    def notEqual(right: Option[Set[AttributeValue]])(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = {
        ssb((l, r) => l != r)(right)(left)
    }

    def cardinality(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = {
        Some(Set(AttributeValueNumber(left.size)))
    }

    def subset(right: Option[Set[AttributeValue]])(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = {
        ssb((l, r) => l.subsetOf(r))(right)(left)
    }

    def superset(right: Option[Set[AttributeValue]])(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = {
        ssb((l, r) => r.subsetOf(l))(right)(left)
    }

    def intersection(right: Option[Set[AttributeValue]])(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = {
        sss((l, r) => l & r)(right)(left)
    }

    def union(right: Option[Set[AttributeValue]])(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = {
        sss((l, r) => l ++ r)(right)(left)
    }

    def difference(right: Option[Set[AttributeValue]])(left: Option[Set[AttributeValue]]): Option[Set[AttributeValue]] = {
        sss((l, r) => l -- r)(right)(left)
    }

    def evaluate(symbolSet: Option[Set[AttributeValue]], op: OperatorUnary): Option[Set[AttributeValue]] = {
        op match {
            case OperatorCardinality => cardinality(symbolSet)
            case OperatorDecrement => decrement(symbolSet)
            case OperatorDecrementToMinimum => decrementToMinimum(symbolSet)
            case OperatorIncrement => increment(symbolSet)
            case OperatorIncrementToMaximum => incrementToMaximum(symbolSet)
            case OperatorNot => not(symbolSet)
        }
    }

    def evaluate(left: Option[Set[AttributeValue]], right: Option[Set[AttributeValue]], op: OperatorBinary): Option[Set[AttributeValue]] = {
        op match {
            case OperatorAddition => add(right)(left)
            case OperatorAnd => and(right)(left)
            case OperatorDifference => difference(right)(left)
            case OperatorDivision => divide(right)(left)
            case OperatorEqual => equal(right)(left)
            case OperatorGreaterThan => greaterThan(right)(left)
            case OperatorGreaterThanOrEqual => greaterThanOrEqual(right)(left)
            case OperatorIntersection => intersection(right)(left)
            case OperatorLessThan => lessThan(right)(left)
            case OperatorLessThanOrEqual => lessThanOrEqual(right)(left)
            case OperatorMultiplication => multiply(right)(left)
            case OperatorNotEqual => notEqual(right)(left)
            case OperatorOr => or(right)(left)
            case OperatorSubset => subset(right)(left)
            case OperatorSubtraction => subtract(right)(left)
            case OperatorSuperset => superset(right)(left)
            case OperatorUnion => union(right)(left)
        }
    }

    /**
     * Evaluate an expression as follows:
     * 1) If a unary expression,
     * Evaluate the left expression, then perform the unary operation on the result.
     *
     * 2) If a binary expression,
     * if the and operation
     * evaluate the left expression
     * if the value of the left expression is true,
     * evaluate the right expression
     * otherwise if the value of the left expression is false,
     * return the value of the left expression
     * otherwise (the value of the left expression is not boolean, and the operation is invalid)
     * return None
     * if the or operation
     * evaluate the left expression
     * if the value of the left expression is false,
     * evaluate the right expression
     * otherwise if the value of the left expression is true,
     * return the value of the left expression
     * otherwise (the value of the left expression is not boolean, and the operation is invalid)
     * return None
     * otherwise
     * evaluate the left expression, the right expression, then perform the binary operation on the result
     *
     * 3) If a boolean literal,
     * return an attribute value for that boolean
     *
     * 4) If an empty set literal,
     * return an attribute value for that empty set
     *
     * 5) If a number literal,
     * return an attribute value for that number
     *
     * 6) If a string literal,
     * return an attribute value for that string
     *
     * 7) If an identifier,
     * If the identifier resolves to an attribute, the value of the attribute
     * If the identifier resolves to an entity, the value of the entity
     * If the identifier resolves to a relationship, None (this is an error).
     * Otherwise None.
     *
     * @param state the state
     * @param symbolMap the symbol map
     * @param expression the expression
     * @return the result of the expression, if any
     */
    def evaluateExpression(state: State, symbolMap: StateStrategyStep.SymbolMap, expression: Expression): Option[Set[AttributeValue]] = {
        expression match {
            case ExpressionUnary(left, op) =>
                AttributeValueOperation.evaluate(evaluateExpression(state, symbolMap, left), op)

            case ExpressionBinary(left, right, op) =>
                op match {
                    case OperatorAnd =>
                        val leftEvaluate = evaluateExpression(state, symbolMap, left)

                        leftEvaluate.map(_.toSeq) match {
                            case Some(Seq(AttributeValueBoolean(true))) =>
                                evaluateExpression(state, symbolMap, right)

                            case _ =>
                                Some(Set(AttributeValueBoolean(boolean = false)))
                        }

                    case OperatorOr =>
                        val leftEvaluate = evaluateExpression(state, symbolMap, left)

                        leftEvaluate.map(_.toSeq) match {
                            case Some(Seq(AttributeValueBoolean(false))) =>
                                AttributeValueOperation.evaluate(leftEvaluate, evaluateExpression(state, symbolMap, right), op)

                            case _ =>
                                Some(Set(AttributeValueBoolean(boolean = true)))
                        }

                    case _ =>
                        AttributeValueOperation.evaluate(evaluateExpression(state, symbolMap, left), evaluateExpression(state, symbolMap, right), op)
                }

            case LiteralBoolean(value) =>
                Some(Set(AttributeValueBoolean(value)))

            case LiteralEmpty =>
                Some(Set())

            case LiteralNumber(value) =>
                Some(Set(AttributeValueNumber(value)))

            case LiteralString(value) =>
                Some(Set(AttributeValueSymbol(Symbol(value))))

            case i: IdentifierAttributeQualified =>
                StateLocation.resolve(state, symbolMap, i) match {
                    case Some(StateLocationAttribute(StateLocationEntity(entity), attribute)) =>
                        val entitySetOption = state.entitySetMap.get(entity._1) // should be Some
                        val entityOption = entitySetOption.map(_.entityMap.get(entity._2)).flatten // should be Some
                        entityOption.map(_.attributeMap.get(attribute)).flatten orElse Some(Set())

                    case Some(StateLocationAttribute(StateLocationRelationshipBidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute)) =>
                        val entityOption = state.relationshipBidirectionalMap.get((left, right)) // should be Some
                        entityOption.map(_.attributeMap.get(attribute)).flatten orElse Some(Set())

                    case Some(StateLocationAttribute(StateLocationRelationshipUnidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute)) =>
                        val entityOption = state.relationshipUnidirectionalMap.get((left, right)) // should be Some
                        entityOption.map(_.attributeMap.get(attribute)).flatten orElse Some(Set())

                    case Some(StateLocationEntity(entity)) =>
                        Some(Set(AttributeValueEntity(entity)))

                    case Some(StateLocationRelationshipBidirectional(StateLocationEntity(left), StateLocationEntity(right))) =>
                        println(s"The expression ${i.toString} references a bidirectional relationship.")
                        None

                    case Some(StateLocationRelationshipUnidirectional(StateLocationEntity(left), StateLocationEntity(right))) =>
                        println(s"The expression ${i.toString} references a unidirectional relationship.")
                        None

                    case None =>
                        None
                }

            case i: IdentifierAttributeUnqualified =>
                println(s"An unqualified identifier must not appear here.")
                None

            case i: IdentifierEntity =>
                println(s"An entity identifier must not appear here.")
                None

            case i: IdentifierRelationshipBidirectional =>
                println(s"A bidirectional relationship identifier must not appear here.")
                None

            case i: IdentifierRelationshipUnidirectional =>
                println(s"A unidirectional relationship identifier must not appear here.")
                None
        }
    }

}