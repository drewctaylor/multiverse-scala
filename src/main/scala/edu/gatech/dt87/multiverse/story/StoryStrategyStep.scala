package edu.gatech.dt87.multiverse.story

import com.gilt.handlebars.scala.Handlebars
import com.gilt.handlebars.scala.binding.dynamic._
import edu.gatech.dt87.multiverse.planner._
import edu.gatech.dt87.multiverse.predicate.Predicate
import edu.gatech.dt87.multiverse.story.dsl._
import monocle.function._
import monocle.std._
import monocle.syntax._

import scala.collection.immutable.Set

object StoryStrategyStep {
    type StorySymbolMap = Map[Symbol, (Symbol, Int)]

    def upsertRelationshipUnidirectional(storyState: StoryState, storySymbolMap: StorySymbolMap, left: Symbol, right: Symbol): StoryState = {
        storyState.applyLens(StoryState.focusRelationshipUnidirectionalMap).composeTraversal(at((storySymbolMap(left), storySymbolMap(right)))).modify({
            case None => Some(StoryEntity())
            case Some(e) => Some(e)
        })
    }

    def upsertRelationshipBidirectional(storyState: StoryState, storySymbolMap: StorySymbolMap, left: Symbol, right: Symbol): StoryState = {
        storyState.applyLens(StoryState.focusRelationshipBidirectionalMap).composeTraversal(at((storySymbolMap(left), storySymbolMap(right)))).modify({
            case None => Some(StoryEntity())
            case Some(e) => Some(e)
        })
    }

    def updateEntityAttribute(storyState: StoryState, storySymbolMap: StorySymbolMap, entity: Symbol, attribute: Symbol, symbolSet: Option[Set[Symbol]]): StoryState = {
        storyState.applyLens(StoryState.focusEntitySetMap)
            .composeTraversal(index(storySymbolMap(entity)._1))
            .composeTraversal(StoryEntitySet.focusEntityMap)
            .composeTraversal(index(storySymbolMap(entity)._2))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .set(symbolSet)
    }

    def insertEntityAttribute(storyState: StoryState, storySymbolMap: StorySymbolMap, entity: Symbol, attribute: Symbol, symbolSet: Option[Set[Symbol]]): StoryState = {
        storyState.applyLens(StoryState.focusEntitySetMap)
            .composeTraversal(index(storySymbolMap(entity)._1))
            .composeTraversal(StoryEntitySet.focusEntityMap)
            .composeTraversal(index(storySymbolMap(entity)._2))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .modify(symbolSetOld => for (sso <- symbolSetOld; ss <- symbolSet) yield sso ++ ss)
    }

    def removeEntityAttribute(storyState: StoryState, storySymbolMap: StorySymbolMap, entity: Symbol, attribute: Symbol, symbolSet: Option[Set[Symbol]]): StoryState = {
        storyState.applyLens(StoryState.focusEntitySetMap)
            .composeTraversal(index(storySymbolMap(entity)._1))
            .composeTraversal(StoryEntitySet.focusEntityMap)
            .composeTraversal(index(storySymbolMap(entity)._2))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .modify(symbolSetOld => for (sso <- symbolSetOld; ss <- symbolSet) yield sso -- ss)
    }

    def updateRelationshipUnidirectionalAttribute(storyState: StoryState, storySymbolMap: StorySymbolMap, left: Symbol, right: Symbol, attribute: Symbol, symbolSet: Option[Set[Symbol]]): StoryState = {
        upsertRelationshipUnidirectional(storyState, storySymbolMap, left, right)
            .applyLens(StoryState.focusRelationshipUnidirectionalMap)
            .composeTraversal(index((storySymbolMap(left), storySymbolMap(right))))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .set(symbolSet)
    }

    def insertRelationshipUnidirectionalAttribute(storyState: StoryState, storySymbolMap: StorySymbolMap, left: Symbol, right: Symbol, attribute: Symbol, symbolSet: Option[Set[Symbol]]): StoryState = {
        upsertRelationshipUnidirectional(storyState, storySymbolMap, left, right)
            .applyLens(StoryState.focusRelationshipUnidirectionalMap)
            .composeTraversal(index((storySymbolMap(left), storySymbolMap(right))))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .modify(symbolSetOld => for (sso <- symbolSetOld; ss <- symbolSet) yield sso ++ ss)
    }

    def removeRelationshipUnidirectionalAttribute(storyState: StoryState, storySymbolMap: StorySymbolMap, left: Symbol, right: Symbol, attribute: Symbol, symbolSet: Option[Set[Symbol]]): StoryState = {
        upsertRelationshipUnidirectional(storyState, storySymbolMap, left, right)
            .applyLens(StoryState.focusRelationshipUnidirectionalMap)
            .composeTraversal(index((storySymbolMap(left), storySymbolMap(right))))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .modify(symbolSetOld => for (sso <- symbolSetOld; ss <- symbolSet) yield sso -- ss)
    }

    def evaluateRelationshipUnidirectionalAttribute(storyState: StoryState, storySymbolMap: StorySymbolMap, left: Symbol, right: Symbol, attribute: Symbol): Option[Set[Symbol]] = {
        val ss1 = upsertRelationshipUnidirectional(storyState, storySymbolMap, left, right)
            .applyLens(StoryState.focusRelationshipUnidirectionalMap)
            .composeTraversal(index((storySymbolMap(left), storySymbolMap(right))))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .modify({
            case None => Some(Set[Symbol]())
            case Some(s) => Some(s)
        })

        ss1.applyLens(StoryState.focusRelationshipUnidirectionalMap)
            .composeTraversal(index((storySymbolMap(left), storySymbolMap(right))))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .headOption getOrElse None
    }

    def updateRelationshipBidirectionalAttribute(storyState: StoryState, storySymbolMap: StorySymbolMap, left: Symbol, right: Symbol, attribute: Symbol, symbolSet: Option[Set[Symbol]]): StoryState = {
        val l = if (storySymbolMap(left)._1.name > storySymbolMap(right)._1.name) right else if (storySymbolMap(left)._1.name == storySymbolMap(right)._1.name && storySymbolMap(left)._2 > storySymbolMap(right)._2) right else left
        val r = if (storySymbolMap(left)._1.name > storySymbolMap(right)._1.name) left else if (storySymbolMap(left)._1.name == storySymbolMap(right)._1.name && storySymbolMap(left)._2 > storySymbolMap(right)._2) left else right

        upsertRelationshipBidirectional(storyState, storySymbolMap, l, r)
            .applyLens(StoryState.focusRelationshipBidirectionalMap)
            .composeTraversal(index((storySymbolMap(l), storySymbolMap(r))))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .set(symbolSet)
    }

    def insertRelationshipBidirectionalAttribute(storyState: StoryState, storySymbolMap: StorySymbolMap, left: Symbol, right: Symbol, attribute: Symbol, symbolSet: Option[Set[Symbol]]): StoryState = {
        val l = if (storySymbolMap(left)._1.name > storySymbolMap(right)._1.name) right else if (storySymbolMap(left)._1.name == storySymbolMap(right)._1.name && storySymbolMap(left)._2 > storySymbolMap(right)._2) right else left
        val r = if (storySymbolMap(left)._1.name > storySymbolMap(right)._1.name) left else if (storySymbolMap(left)._1.name == storySymbolMap(right)._1.name && storySymbolMap(left)._2 > storySymbolMap(right)._2) left else right

        upsertRelationshipBidirectional(storyState, storySymbolMap, l, r)
            .applyLens(StoryState.focusRelationshipBidirectionalMap)
            .composeTraversal(index((storySymbolMap(l), storySymbolMap(r))))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .modify(symbolSetOld => for (sso <- symbolSetOld; ss <- symbolSet) yield sso ++ ss)
    }

    def removeRelationshipBidirectionalAttribute(storyState: StoryState, storySymbolMap: StorySymbolMap, left: Symbol, right: Symbol, attribute: Symbol, symbolSet: Option[Set[Symbol]]): StoryState = {
        val l = if (storySymbolMap(left)._1.name > storySymbolMap(right)._1.name) right else if (storySymbolMap(left)._1.name == storySymbolMap(right)._1.name && storySymbolMap(left)._2 > storySymbolMap(right)._2) right else left
        val r = if (storySymbolMap(left)._1.name > storySymbolMap(right)._1.name) left else if (storySymbolMap(left)._1.name == storySymbolMap(right)._1.name && storySymbolMap(left)._2 > storySymbolMap(right)._2) left else right

        upsertRelationshipBidirectional(storyState, storySymbolMap, l, r)
            .applyLens(StoryState.focusRelationshipBidirectionalMap)
            .composeTraversal(index((storySymbolMap(l), storySymbolMap(r))))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .modify(symbolSetOld => for (sso <- symbolSetOld; ss <- symbolSet) yield sso -- ss)
    }

    def evaluateRelationshipBidirectionalAttribute(storyState: StoryState, storySymbolMap: StorySymbolMap, left: Symbol, right: Symbol, attribute: Symbol): Option[Set[Symbol]] = {
        val l = if (storySymbolMap(left)._1.name > storySymbolMap(right)._1.name) right else if (storySymbolMap(left)._1.name == storySymbolMap(right)._1.name && storySymbolMap(left)._2 > storySymbolMap(right)._2) right else left
        val r = if (storySymbolMap(left)._1.name > storySymbolMap(right)._1.name) left else if (storySymbolMap(left)._1.name == storySymbolMap(right)._1.name && storySymbolMap(left)._2 > storySymbolMap(right)._2) left else right

        val ss1 = upsertRelationshipBidirectional(storyState, storySymbolMap, l, r)
            .applyLens(StoryState.focusRelationshipBidirectionalMap)
            .composeTraversal(index((storySymbolMap(l), storySymbolMap(r))))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .modify({
            case None => Some(Set[Symbol]())
            case Some(s) => Some(s)
        })

        ss1.applyLens(StoryState.focusRelationshipBidirectionalMap)
            .composeTraversal(index((storySymbolMap(l), storySymbolMap(r))))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .headOption getOrElse None
    }

    def evaluateEntityAttribute(storyState: StoryState, storySymbolMap: StorySymbolMap, entity: Symbol, attribute: Symbol): Option[Set[Symbol]] = {
        storyState.applyLens(StoryState.focusEntitySetMap)
            .composeTraversal(index(storySymbolMap(entity)._1))
            .composeTraversal(StoryEntitySet.focusEntityMap)
            .composeTraversal(index(storySymbolMap(entity)._2))
            .composeTraversal(StoryEntity.focusAttributeMap)
            .composeTraversal(at(attribute))
            .headOption getOrElse Some(Set())
    }

    def evaluateBoolean(symbolSet: Option[Set[Symbol]]): Option[Boolean] = {
        symbolSet match {
            case Some(set) => set.toSeq match {
                case Seq(Symbol("true")) => Some(true)
                case Seq(Symbol("false")) => Some(false)
                case _ => None
            }
            case None => None
        }
    }

    def evaluateSymbol(symbolSet: Option[Set[Symbol]]): Option[Symbol] = {
        symbolSet match {
            case Some(set) => set.toSeq match {
                case Seq(Symbol(s)) => Some(Symbol(s))
                case _ => None
            }
            case None => None
        }
    }

    // to implement
    def evaluateDecrement(symbol: Option[Set[Symbol]]): Option[Set[Symbol]] = None

    def evaluateDecrementToMinimum(symbol: Option[Set[Symbol]]): Option[Set[Symbol]] = None

    def evaluateIncrement(symbol: Option[Set[Symbol]]): Option[Set[Symbol]] = None

    def evaluateIncrementToMaximum(symbol: Option[Set[Symbol]]): Option[Set[Symbol]] = None

    def evaluateGreaterThan(left: Option[Set[Symbol]], right: Option[Set[Symbol]]): Option[Set[Symbol]] = None

    def evaluateGreaterThanOrEqual(left: Option[Set[Symbol]], right: Option[Set[Symbol]]): Option[Set[Symbol]] = None

    def evaluateLessThan(left: Option[Set[Symbol]], right: Option[Set[Symbol]]): Option[Set[Symbol]] = None

    def evaluateLessThanOrEqual(left: Option[Set[Symbol]], right: Option[Set[Symbol]]): Option[Set[Symbol]] = None

    def evaluateNot(left: Option[Set[Symbol]]): Option[Set[Symbol]] = {
        for (b <- evaluateBoolean(left)) yield Set(Symbol((!b).toString))
    }

    def evaluateOperatorUnary(symbolSet: Option[Set[Symbol]], op: OperatorUnary): Option[Set[Symbol]] = {
        op match {
            case OperatorDecrement => evaluateDecrement(symbolSet)
            case OperatorDecrementToMinimum => evaluateDecrementToMinimum(symbolSet)
            case OperatorIncrement => evaluateIncrement(symbolSet)
            case OperatorIncrementToMaximum => evaluateIncrementToMaximum(symbolSet)
            case OperatorNot => evaluateNot(symbolSet)
        }
    }

    def evaluateAnd(left: Option[Set[Symbol]], right: Option[Set[Symbol]]): Option[Set[Symbol]] = {
        for (l <- evaluateBoolean(left); r <- evaluateBoolean(right)) yield Set(Symbol((l && r).toString))
    }

    def evaluateOr(left: Option[Set[Symbol]], right: Option[Set[Symbol]]): Option[Set[Symbol]] = {
        for (l <- evaluateBoolean(left); r <- evaluateBoolean(right)) yield Set(Symbol((l || r).toString))
    }

    def evaluateDifference(left: Option[Set[Symbol]], right: Option[Set[Symbol]]): Option[Set[Symbol]] = {
        for (l <- left; r <- right) yield l -- r
    }

    def evaluateIntersection(left: Option[Set[Symbol]], right: Option[Set[Symbol]]): Option[Set[Symbol]] = {
        for (l <- left; r <- right) yield l & r
    }

    def evaluateUnion(left: Option[Set[Symbol]], right: Option[Set[Symbol]]): Option[Set[Symbol]] = {
        for (l <- left; r <- right) yield l ++ r
    }

    def evaluateSubset(left: Option[Set[Symbol]], right: Option[Set[Symbol]]): Option[Set[Symbol]] = {
        for (l <- left; r <- right) yield Set(Symbol(l.subsetOf(r).toString))
    }

    def evaluateSuperset(left: Option[Set[Symbol]], right: Option[Set[Symbol]]): Option[Set[Symbol]] = {
        for (l <- left; r <- right) yield Set(Symbol(r.subsetOf(l).toString))
    }

    def evaluateEqual(left: Option[Set[Symbol]], right: Option[Set[Symbol]]): Option[Set[Symbol]] = {
        for (l <- left; r <- right) yield Set(Symbol((l == r).toString))
    }

    def evaluateNotEqual(left: Option[Set[Symbol]], right: Option[Set[Symbol]]): Option[Set[Symbol]] = {
        for (l <- left; r <- right) yield Set(Symbol((l != r).toString))
    }

    def evaluateOperator(left: Option[Set[Symbol]], right: Option[Set[Symbol]], op: OperatorBinary): Option[Set[Symbol]] = {
        op match {
            case OperatorAnd => evaluateAnd(left, right)
            case OperatorDifference => evaluateDifference(left, right)
            case OperatorEqual => evaluateEqual(left, right)
            case OperatorNotEqual => evaluateNotEqual(left, right)
            case OperatorGreaterThan => evaluateGreaterThan(left, right)
            case OperatorGreaterThanOrEqual => evaluateGreaterThanOrEqual(left, right)
            case OperatorIntersection => evaluateIntersection(left, right)
            case OperatorLessThan => evaluateLessThan(left, right)
            case OperatorLessThanOrEqual => evaluateLessThanOrEqual(left, right)
            case OperatorOr => evaluateOr(left, right)
            case OperatorSubset => evaluateSubset(left, right)
            case OperatorSuperset => evaluateSuperset(left, right)
            case OperatorUnion => evaluateUnion(left, right)
        }
    }

    def evaluateEntityEqual(storyState: StoryState, storySymbolMap: StorySymbolMap, left: Symbol, right: Symbol): Option[Set[Symbol]] = {
        Some(Set(Symbol((storySymbolMap(left) == storySymbolMap(right)).toString)))
    }

    def evaluateEntityNotEqual(storyState: StoryState, storySymbolMap: StorySymbolMap, left: Symbol, right: Symbol): Option[Set[Symbol]] = {
        Some(Set(Symbol((storySymbolMap(left) != storySymbolMap(right)).toString)))
    }

    def evaluateExpression(storyState: StoryState, storySymbolMap: StorySymbolMap, expression: Expression): Option[Set[Symbol]] = {
        expression match {
            case ExpressionBinary(ExpressionAttribute(None, el), ExpressionAttribute(None, er), OperatorEqual) => evaluateEntityEqual(storyState, storySymbolMap, el, er)
            case ExpressionBinary(ExpressionAttribute(None, el), ExpressionAttribute(None, er), OperatorNotEqual) => evaluateEntityNotEqual(storyState, storySymbolMap, el, er)
            case ExpressionUnary(left, op) => evaluateOperatorUnary(evaluateExpression(storyState, storySymbolMap, left), op)
            case ExpressionBinary(left, right, op) => evaluateOperator(evaluateExpression(storyState, storySymbolMap, left), evaluateExpression(storyState, storySymbolMap, right), op)
            case ExpressionLiteralString(string) => Some(Set(Symbol(string)))
            case ExpressionLiteralSymbol(symbol) => Some(Set(symbol))
            case ExpressionLiteralBoolean(symbol) => Some(Set(symbol))
            case ExpressionLiteralNumber(symbol) => Some(Set(symbol))
            case ExpressionAttribute(Some(ExpressionEntity(e)), a) => evaluateEntityAttribute(storyState, storySymbolMap, e, a)
            case ExpressionAttribute(Some(ExpressionRelationshipBidirectional(l, r)), a) => evaluateRelationshipBidirectionalAttribute(storyState, storySymbolMap, l, r, a)
            case ExpressionAttribute(Some(ExpressionRelationshipUnidirectional(l, r)), a) => evaluateRelationshipUnidirectionalAttribute(storyState, storySymbolMap, l, r, a)
            case _ => None
        }
    }

    def eventAssignment(statementAssignment: StatementAssignment): Event[StoryState, StorySymbolMap, StorySymbolMap] = {
        statementAssignment match {
            case StatementAssignment(ExpressionAttribute(Some(ExpressionEntity(e)), a), right, OperatorAssignmentUpdate) => Event((s, x) => Some(updateEntityAttribute(s, x, e, a, evaluateExpression(s, x, right)), x))
            case StatementAssignment(ExpressionAttribute(Some(ExpressionEntity(e)), a), right, OperatorAssignmentInsert) => Event((s, x) => Some(insertEntityAttribute(s, x, e, a, evaluateExpression(s, x, right)), x))
            case StatementAssignment(ExpressionAttribute(Some(ExpressionEntity(e)), a), right, OperatorAssignmentRemove) => Event((s, x) => Some(removeEntityAttribute(s, x, e, a, evaluateExpression(s, x, right)), x))
            case StatementAssignment(ExpressionAttribute(Some(ExpressionRelationshipBidirectional(l, r)), a), right, OperatorAssignmentUpdate) => Event((s, x) => Some(updateRelationshipBidirectionalAttribute(s, x, l, r, a, evaluateExpression(s, x, right)), x))
            case StatementAssignment(ExpressionAttribute(Some(ExpressionRelationshipBidirectional(l, r)), a), right, OperatorAssignmentInsert) => Event((s, x) => Some(insertRelationshipBidirectionalAttribute(s, x, l, r, a, evaluateExpression(s, x, right)), x))
            case StatementAssignment(ExpressionAttribute(Some(ExpressionRelationshipBidirectional(l, r)), a), right, OperatorAssignmentRemove) => Event((s, x) => Some(removeRelationshipBidirectionalAttribute(s, x, l, r, a, evaluateExpression(s, x, right)), x))
            case StatementAssignment(ExpressionAttribute(Some(ExpressionRelationshipUnidirectional(l, r)), a), right, OperatorAssignmentUpdate) => Event((s, x) => Some(updateRelationshipUnidirectionalAttribute(s, x, l, r, a, evaluateExpression(s, x, right)), x))
            case StatementAssignment(ExpressionAttribute(Some(ExpressionRelationshipUnidirectional(l, r)), a), right, OperatorAssignmentInsert) => Event((s, x) => Some(insertRelationshipUnidirectionalAttribute(s, x, l, r, a, evaluateExpression(s, x, right)), x))
            case StatementAssignment(ExpressionAttribute(Some(ExpressionRelationshipUnidirectional(l, r)), a), right, OperatorAssignmentRemove) => Event((s, x) => Some(removeRelationshipUnidirectionalAttribute(s, x, l, r, a, evaluateExpression(s, x, right)), x))
            case _ => Event((s, x) => None)
        }
    }

    val i = Iterator.from(0)

    def eventDeclare(declaration: DeclarationEntity): Event[StoryState, StorySymbolMap, StorySymbolMap] = {
        declaration match {
            case DeclarationEntity(k, ExpressionEntity(e), al) => Event((s, x) => {
                val in = i.next();

                val s1 = s.applyLens(StoryState.focusEntitySetMap)
                    .composeTraversal(at(k))
                    .modify({
                    case None => Some(StoryEntitySet())
                    case Some(es) => Some(es)

                }).applyLens(StoryState.focusEntitySetMap)
                    .composeTraversal(index(k))
                    .composeTraversal(StoryEntitySet.focusEntityMap)
                    .composeTraversal(at(in))
                    .modify({
                    case None => Some(StoryEntity())
                    case Some(en) => Some(en)
                })

                val x2 = x + (e -> (k -> in))

                Some(s1, x2)
            })
        }
    }


    def generatorFor(declarationParameter: DeclarationParameter): (StoryState) => List[Int] = {
        (s) => {
            s.entitySetMap.get(declarationParameter.kind).map(_.entityMap.keys.toList) getOrElse List[Int]()
        }
    }

    def eventQuery(statementQuery: StatementQuery): Event[StoryState, StorySymbolMap, StorySymbolMap] = {
        statementQuery match {
            case StatementQuery(Nil, right, OperatorQueryAll) => Event((s, x) => evaluateBoolean(evaluateExpression(s, x, right)) match {
                case Some(true) => Some(s, x)
                case _ => None
            })
            case StatementQuery(d1 :: Nil, right, OperatorQueryAll) => Event((s, x) => {
                val tuple = Predicate.given(generatorFor(d1)).thereExists((e1) => {

                    val y: StorySymbolMap = x +
                        (d1.parameter ->(d1.kind, e1))

                    evaluateBoolean(evaluateExpression(s, y, right)) getOrElse false
                })(s)

                tuple match {
                    case Some(t) => Some(s, x +
                        (d1.parameter ->(d1.kind, t)))
                    case None => None
                }

            })
            case StatementQuery(d1 :: d2 :: Nil, right, OperatorQueryAll) => Event((s, x) => {
                val tuple = Predicate.given(generatorFor(d1),
                    generatorFor(d2)).thereExists((e1, e2) => {

                    val y: StorySymbolMap = x +
                        (d1.parameter ->(d1.kind, e1)) +
                        (d2.parameter ->(d2.kind, e2))

                    evaluateBoolean(evaluateExpression(s, y, right)) getOrElse false
                })(s)

                tuple match {
                    case Some(t) => Some(s, x +
                        (d1.parameter ->(d1.kind, t._1)) +
                        (d2.parameter ->(d2.kind, t._2)))
                    case None => None
                }

            })
            case StatementQuery(d1 :: d2 :: d3 :: Nil, right, OperatorQueryAll) => Event((s, x) => {
                val tuple = Predicate.given(generatorFor(d1),
                    generatorFor(d2),
                    generatorFor(d3)).thereExists((e1, e2, e3) => {

                    val y: StorySymbolMap = x +
                        (d1.parameter ->(d1.kind, e1)) +
                        (d2.parameter ->(d2.kind, e2)) +
                        (d3.parameter ->(d3.kind, e3))

                    evaluateBoolean(evaluateExpression(s, y, right)) getOrElse false
                })(s)

                tuple match {
                    case Some(t) => Some(s, x +
                        (d1.parameter ->(d1.kind, t._1)) +
                        (d2.parameter ->(d2.kind, t._2)) +
                        (d3.parameter ->(d3.kind, t._3)))
                    case None => None
                }

            })
            case StatementQuery(d1 :: d2 :: d3 :: d4 :: Nil, right, OperatorQueryAll) => Event((s, x) => {
                val tuple = Predicate.given(generatorFor(d1),
                    generatorFor(d2),
                    generatorFor(d3),
                    generatorFor(d4)).thereExists((e1, e2, e3, e4) => {

                    val y: StorySymbolMap = x +
                        (d1.parameter ->(d1.kind, e1)) +
                        (d2.parameter ->(d2.kind, e2)) +
                        (d3.parameter ->(d3.kind, e3)) +
                        (d4.parameter ->(d4.kind, e4))

                    evaluateBoolean(evaluateExpression(s, y, right)) getOrElse false
                })(s)

                tuple match {
                    case Some(t) => Some(s, x +
                        (d1.parameter ->(d1.kind, t._1)) +
                        (d2.parameter ->(d2.kind, t._2)) +
                        (d3.parameter ->(d3.kind, t._3)) +
                        (d4.parameter ->(d4.kind, t._4)))
                    case None => None
                }

            })
            case StatementQuery(d1 :: d2 :: d3 :: d4 :: d5 :: Nil, right, OperatorQueryAll) => Event((s, x) => {
                val tuple = Predicate.given(generatorFor(d1),
                    generatorFor(d2),
                    generatorFor(d3),
                    generatorFor(d4),
                    generatorFor(d5)).thereExists((e1, e2, e3, e4, e5) => {

                    val y: StorySymbolMap = x +
                        (d1.parameter ->(d1.kind, e1)) +
                        (d2.parameter ->(d2.kind, e2)) +
                        (d3.parameter ->(d3.kind, e3)) +
                        (d4.parameter ->(d4.kind, e4)) +
                        (d5.parameter ->(d5.kind, e5))

                    evaluateBoolean(evaluateExpression(s, y, right)) getOrElse false
                })(s)

                tuple match {
                    case Some(t) => Some(s, x +
                        (d1.parameter ->(d1.kind, t._1)) +
                        (d2.parameter ->(d2.kind, t._2)) +
                        (d3.parameter ->(d3.kind, t._3)) +
                        (d4.parameter ->(d4.kind, t._4)) +
                        (d5.parameter ->(d5.kind, t._5)))
                    case None => None
                }

            })
            case StatementQuery(d1 :: d2 :: d3 :: d4 :: d5 :: d6 :: Nil, right, OperatorQueryAll) => Event((s, x) => {
                val tuple = Predicate.given(generatorFor(d1),
                    generatorFor(d2),
                    generatorFor(d3),
                    generatorFor(d4),
                    generatorFor(d5),
                    generatorFor(d6)).thereExists((e1, e2, e3, e4, e5, e6) => {

                    val y: StorySymbolMap = x +
                        (d1.parameter ->(d1.kind, e1)) +
                        (d2.parameter ->(d2.kind, e2)) +
                        (d3.parameter ->(d3.kind, e3)) +
                        (d4.parameter ->(d4.kind, e4)) +
                        (d5.parameter ->(d5.kind, e5)) +
                        (d6.parameter ->(d6.kind, e6))

                    evaluateBoolean(evaluateExpression(s, y, right)) getOrElse false
                })(s)

                tuple match {
                    case Some(t) => Some(s, x +
                        (d1.parameter ->(d1.kind, t._1)) +
                        (d2.parameter ->(d2.kind, t._2)) +
                        (d3.parameter ->(d3.kind, t._3)) +
                        (d4.parameter ->(d4.kind, t._4)) +
                        (d5.parameter ->(d5.kind, t._5)) +
                        (d6.parameter ->(d6.kind, t._6)))
                    case None => None
                }

            })
            case StatementQuery(Nil, right, OperatorQueryNone) => Event((s, x) => evaluateBoolean(evaluateExpression(s, x, right)) match {
                case Some(false) => Some(s, x)
                case _ => None
            })
            case StatementQuery(d1 :: Nil, right, OperatorQueryNone) => Event((s, x) => {
                val tuple = Predicate.given(generatorFor(d1)).thereExists((e1) => {

                    val y: StorySymbolMap = x +
                        (d1.parameter ->(d1.kind, e1))

                    evaluateBoolean(evaluateExpression(s, y, right)) getOrElse false
                })(s)

                tuple match {
                    case Some(t) => None
                    case None => Some(s, x)
                }

            })
            case StatementQuery(d1 :: d2 :: Nil, right, OperatorQueryNone) => Event((s, x) => {
                val tuple = Predicate.given(generatorFor(d1),
                    generatorFor(d2)).thereExists((e1, e2) => {

                    val y: StorySymbolMap = x +
                        (d1.parameter ->(d1.kind, e1)) +
                        (d2.parameter ->(d2.kind, e2))

                    evaluateBoolean(evaluateExpression(s, y, right)) getOrElse false
                })(s)

                tuple match {
                    case Some(t) => None
                    case None => Some(s, x)
                }

            })
            case StatementQuery(d1 :: d2 :: d3 :: Nil, right, OperatorQueryNone) => Event((s, x) => {
                val tuple = Predicate.given(generatorFor(d1),
                    generatorFor(d2),
                    generatorFor(d3)).thereExists((e1, e2, e3) => {

                    val y: StorySymbolMap = x +
                        (d1.parameter ->(d1.kind, e1)) +
                        (d2.parameter ->(d2.kind, e2)) +
                        (d3.parameter ->(d3.kind, e3))

                    evaluateBoolean(evaluateExpression(s, y, right)) getOrElse false
                })(s)

                tuple match {
                    case Some(t) => None
                    case None => Some(s, x)
                }

            })
            case StatementQuery(d1 :: d2 :: d3 :: d4 :: Nil, right, OperatorQueryNone) => Event((s, x) => {
                val tuple = Predicate.given(generatorFor(d1),
                    generatorFor(d2),
                    generatorFor(d3),
                    generatorFor(d4)).thereExists((e1, e2, e3, e4) => {

                    val y: StorySymbolMap = x +
                        (d1.parameter ->(d1.kind, e1)) +
                        (d2.parameter ->(d2.kind, e2)) +
                        (d3.parameter ->(d3.kind, e3)) +
                        (d4.parameter ->(d4.kind, e4))

                    evaluateBoolean(evaluateExpression(s, y, right)) getOrElse false
                })(s)

                tuple match {
                    case Some(t) => None
                    case None => Some(s, x)
                }

            })
            case StatementQuery(d1 :: d2 :: d3 :: d4 :: d5 :: Nil, right, OperatorQueryNone) => Event((s, x) => {
                val tuple = Predicate.given(generatorFor(d1),
                    generatorFor(d2),
                    generatorFor(d3),
                    generatorFor(d4),
                    generatorFor(d5)).thereExists((e1, e2, e3, e4, e5) => {

                    val y: StorySymbolMap = x +
                        (d1.parameter ->(d1.kind, e1)) +
                        (d2.parameter ->(d2.kind, e2)) +
                        (d3.parameter ->(d3.kind, e3)) +
                        (d4.parameter ->(d4.kind, e4)) +
                        (d5.parameter ->(d5.kind, e5))

                    evaluateBoolean(evaluateExpression(s, y, right)) getOrElse false
                })(s)

                tuple match {
                    case Some(t) => None
                    case None => Some(s, x)
                }

            })
            case StatementQuery(d1 :: d2 :: d3 :: d4 :: d5 :: d6 :: Nil, right, OperatorQueryNone) => Event((s, x) => {
                val tuple = Predicate.given(generatorFor(d1),
                    generatorFor(d2),
                    generatorFor(d3),
                    generatorFor(d4),
                    generatorFor(d5),
                    generatorFor(d6)).thereExists((e1, e2, e3, e4, e5, e6) => {

                    val y: StorySymbolMap = x +
                        (d1.parameter ->(d1.kind, e1)) +
                        (d2.parameter ->(d2.kind, e2)) +
                        (d3.parameter ->(d3.kind, e3)) +
                        (d4.parameter ->(d4.kind, e4)) +
                        (d5.parameter ->(d5.kind, e5)) +
                        (d6.parameter ->(d6.kind, e6))

                    evaluateBoolean(evaluateExpression(s, y, right)) getOrElse false
                })(s)

                tuple match {
                    case Some(t) => None
                    case None => Some(s, x)
                }

            })
        }
    }

    def eventSatisfaction(statementSatisfaction: StatementSatisfaction, goalFor: Symbol => Goal[StoryState, StorySymbolMap, StorySymbolMap], declarationFor: Symbol => DeclarationGoal): Subgoal[StoryState, StorySymbolMap, StorySymbolMap, StorySymbolMap, StorySymbolMap] = {
        val goal = new Goal[StoryState, StorySymbolMap, StorySymbolMap]() {
            def name: String = {
                goalFor(statementSatisfaction.goal).name
            }

            def strategySet: Set[Strategy[StoryState, StorySymbolMap, StorySymbolMap]] = {
                goalFor(statementSatisfaction.goal).strategySet
            }

            def satisfy(state: StoryState, input: StorySymbolMap): GoalExecution[StoryState, StorySymbolMap, StorySymbolMap] = {
                goalFor(statementSatisfaction.goal).satisfy(state, input)
            }

        }

        Subgoal(
            (s, x) => {
                Map(declarationFor(statementSatisfaction.goal).parameterList.map(_.parameter).zip(statementSatisfaction.parameterList.map(x)).toSeq: _*)
            },
            goal,
            (s, x, y1) => x)
    }

    def eventNarration(statementNarration: StatementNarration): Event[StoryState, StorySymbolMap, StorySymbolMap] = {
        Event((s, x) => {
            val turn: Map[String, Map[String, String]] = x.map((p) => p._1.name -> s.entitySetMap(p._2._1).entityMap(p._2._2).attributeMap.map((a) => a._1.name -> a._2.map(_.name).mkString(", ")))
            val t = Handlebars(statementNarration.string.string)

            val turnWithGender = turn.map((p) => {
                p._1 -> p._2.foldLeft(Map[String, String]())((accumulator, pair) => {
                    if (pair._1 == "gender") {
                        pair._2 match {
                            case "male" => accumulator + pair +
                                ("sub" -> "he") +
                                ("Sub" -> "He") +
                                ("obj" -> "him") +
                                ("Obj" -> "Him") +
                                ("pos" -> "his") +
                                ("Pos" -> "His") +
                                ("det" -> "his") +
                                ("Det" -> "His") +
                                ("ref" -> "himself") +
                                ("Ref" -> "Himself")
                            case "female" => accumulator + pair +
                                ("sub" -> "she") +
                                ("Sub" -> "She") +
                                ("obj" -> "her") +
                                ("Obj" -> "Her") +
                                ("pos" -> "hers") +
                                ("Pos" -> "Hers") +
                                ("det" -> "her") +
                                ("Det" -> "Her") +
                                ("ref" -> "herself") +
                                ("Ref" -> "Herself")
                            case "neuter" => accumulator + pair +
                                ("sub" -> "it") +
                                ("Sub" -> "It") +
                                ("obj" -> "it") +
                                ("Obj" -> "It") +
                                ("pos" -> "its") +
                                ("Pos" -> "Its") +
                                ("det" -> "its") +
                                ("Det" -> "Its") +
                                ("ref" -> "itself") +
                                ("Ref" -> "Itself")
                            case _ => accumulator + pair
                        }
                    } else {
                        accumulator + pair
                    }
                })
            })

            Some(s.applyLens(StoryState.focusNarration).modify(_ => Some(t(turnWithGender))), x)
        })

    }
}