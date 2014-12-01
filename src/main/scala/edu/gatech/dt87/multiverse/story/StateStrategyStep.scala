package edu.gatech.dt87.multiverse.story

import com.gilt.handlebars.scala.Handlebars
import com.gilt.handlebars.scala.binding.dynamic._
import edu.gatech.dt87.multiverse.planner._
import edu.gatech.dt87.multiverse.predicate.Predicate
import edu.gatech.dt87.multiverse.story.dsl.parser._
import monocle.function._
import monocle.std._
import monocle.syntax._

import scala.collection.immutable.Set

object StateStrategyStep {
    type SymbolMap = Map[Symbol, (Symbol, Int)]
    val idIterator = Iterator.from(0)

    def insertEntity(state: State, entity: (Symbol, Int)): State = {
        state.applyLens(State.focusEntitySetMap)
            .composeTraversal(at(entity._1))
            .modify({
            case None => Some(EntitySet())
            case Some(es) => Some(es)
        }).applyLens(State.focusEntitySetMap)
            .composeTraversal(index(entity._1))
            .composeTraversal(EntitySet.focusEntityMap)
            .composeTraversal(at(entity._2))
            .modify({
            case None => Some(Entity())
            case Some(en) => Some(en)
        })
    }

    def upsertRelationshipUnidirectional(state: State, left: (Symbol, Int), right: (Symbol, Int)): State = {
        state.applyLens(State.focusRelationshipUnidirectionalMap)
            .composeTraversal(at(left, right))
            .modify({
            case None => Some(Entity())
            case Some(e) => Some(e)
        })
    }

    def upsertRelationshipBidirectional(state: State, left: (Symbol, Int), right: (Symbol, Int)): State = {
        state.applyLens(State.focusRelationshipBidirectionalMap)
            .composeTraversal(at(left, right))
            .modify({
            case None => Some(Entity())
            case Some(e) => Some(e)
        })
    }

    def update(state: State, stateLocation: StateLocation, symbolSet: Option[Set[AttributeValue]]): Option[State] = {

        stateLocation match {
            case StateLocationAttribute(StateLocationEntity(entity), attribute) =>
                Some(state.applyLens(State.focusEntitySetMap)
                    .composeTraversal(index(entity._1))
                    .composeTraversal(EntitySet.focusEntityMap)
                    .composeTraversal(index(entity._2))
                    .composeTraversal(Entity.focusAttributeMap)
                    .composeTraversal(at(attribute))
                    .set(symbolSet))

            case StateLocationAttribute(StateLocationRelationshipBidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute) =>
                Some(upsertRelationshipBidirectional(state, left, right)
                    .applyLens(State.focusRelationshipBidirectionalMap)
                    .composeTraversal(index((left, right)))
                    .composeTraversal(Entity.focusAttributeMap)
                    .composeTraversal(at(attribute))
                    .set(symbolSet))

            case StateLocationAttribute(StateLocationRelationshipUnidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute) =>
                Some(upsertRelationshipUnidirectional(state, left, right)
                    .applyLens(State.focusRelationshipUnidirectionalMap)
                    .composeTraversal(index((left, right)))
                    .composeTraversal(Entity.focusAttributeMap)
                    .composeTraversal(at(attribute))
                    .set(symbolSet))

            case _ =>
                println("The system can only update an attribute; it cannot update an entity or a relationship.")
                None

        }
    }

    def insert(state: State, stateLocation: StateLocation, symbolSet: Option[Set[AttributeValue]]): Option[State] = {
        val union = AttributeValueOperation.union(symbolSet) _

        stateLocation match {
            case StateLocationAttribute(StateLocationEntity(entity), attribute) =>
                Some(state.applyLens(State.focusEntitySetMap)
                    .composeTraversal(index(entity._1))
                    .composeTraversal(EntitySet.focusEntityMap)
                    .composeTraversal(index(entity._2))
                    .composeTraversal(Entity.focusAttributeMap)
                    .composeTraversal(at(attribute))
                    .modify(union))

            case StateLocationAttribute(StateLocationRelationshipBidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute) =>
                Some(upsertRelationshipBidirectional(state, left, right)
                    .applyLens(State.focusRelationshipBidirectionalMap)
                    .composeTraversal(index((left, right)))
                    .composeTraversal(Entity.focusAttributeMap)
                    .composeTraversal(at(attribute))
                    .modify(union))

            case StateLocationAttribute(StateLocationRelationshipUnidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute) =>
                Some(upsertRelationshipUnidirectional(state, left, right)
                    .applyLens(State.focusRelationshipUnidirectionalMap)
                    .composeTraversal(index((left, right)))
                    .composeTraversal(Entity.focusAttributeMap)
                    .composeTraversal(at(attribute))
                    .modify(union))

            case _ =>
                println("The system can only insert into an attribute; it cannot insert to an entity or a relationship.")
                None
        }
    }

    def remove(state: State, stateLocation: StateLocation, symbolSet: Option[Set[AttributeValue]]): Option[State] = {
        val difference = AttributeValueOperation.difference(symbolSet) _

        stateLocation match {
            case StateLocationAttribute(StateLocationEntity(entity), attribute) =>
                Some(state.applyLens(State.focusEntitySetMap)
                    .composeTraversal(index(entity._1))
                    .composeTraversal(EntitySet.focusEntityMap)
                    .composeTraversal(index(entity._2))
                    .composeTraversal(Entity.focusAttributeMap)
                    .composeTraversal(at(attribute))
                    .modify(difference))

            case StateLocationAttribute(StateLocationRelationshipBidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute) =>
                Some(upsertRelationshipBidirectional(state, left, right)
                    .applyLens(State.focusRelationshipUnidirectionalMap)
                    .composeTraversal(index((left, right)))
                    .composeTraversal(Entity.focusAttributeMap)
                    .composeTraversal(at(attribute))
                    .modify(difference))

            case StateLocationAttribute(StateLocationRelationshipUnidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute) =>
                Some(upsertRelationshipUnidirectional(state, left, right)
                    .applyLens(State.focusRelationshipBidirectionalMap)
                    .composeTraversal(index((left, right)))
                    .composeTraversal(Entity.focusAttributeMap)
                    .composeTraversal(at(attribute))
                    .modify(difference))

            case _ =>
                println("The system can only remove from an attribute; it cannot remove from an entity or a relationship.")
                None
        }
    }

    def evaluateExpression(state: State, symbolMap: SymbolMap, expression: Expression): Option[Set[AttributeValue]] = {
        expression match {
            case ExpressionIdentifierSequence(owner, identifierSeq) =>
                StateLocation.resolve(state, symbolMap, ExpressionIdentifierSequence(owner, identifierSeq)) match {
                    case Some(StateLocationAttribute(StateLocationEntity(entity), attribute)) =>
                        val entitySetOption = state.entitySetMap.get(entity._1)
                        val entityOption = entitySetOption.map(_.entityMap.get(entity._2)).flatten
                        entityOption.map(_.attributeMap.get(attribute)).flatten orElse Some(Set())

                    case Some(StateLocationAttribute(StateLocationRelationshipBidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute)) =>
                        val entityOption = state.relationshipBidirectionalMap.get((left, right))
                        entityOption.map(_.attributeMap.get(attribute)).flatten orElse Some(Set())

                    case Some(StateLocationAttribute(StateLocationRelationshipUnidirectional(StateLocationEntity(left), StateLocationEntity(right)), attribute)) =>
                        val entityOption = state.relationshipUnidirectionalMap.get((left, right))
                        entityOption.map(_.attributeMap.get(attribute)).flatten orElse Some(Set())

                    case Some(StateLocationEntity(entity)) =>
                        Some(Set(AttributeValueEntity(entity)))

                    case Some(StateLocationRelationshipBidirectional(StateLocationEntity(left), StateLocationEntity(right))) =>
                        Some(Set(AttributeValueRelationshipBidirectional(left, right)))

                    case Some(StateLocationRelationshipUnidirectional(StateLocationEntity(left), StateLocationEntity(right))) =>
                        Some(Set(AttributeValueRelationshipUnidirectional(left, right)))

                    case _ =>
                        None
                }

            case ExpressionUnary(left, op) =>
                AttributeValueOperation.evaluate(evaluateExpression(state, symbolMap, left), op)

            case ExpressionBinary(left, right, op) =>
                AttributeValueOperation.evaluate(evaluateExpression(state, symbolMap, left), evaluateExpression(state, symbolMap, right), op)

            case ExpressionLiteralBoolean(value) =>
                Some(Set(AttributeValueBoolean(value)))

            case ExpressionLiteralEmpty =>
                Some(Set())

            case ExpressionLiteralNumber(value) =>
                Some(Set(AttributeValueNumber(value)))

            case ExpressionLiteralString(value) =>
                Some(Set(AttributeValueSymbol(Symbol(value))))

            case _ =>
                println("The system could not evaluate the expression.")
                None
        }
    }

    def eventAssignment(statementAssignment: StatementAssignment): Event[State, SymbolMap, SymbolMap] = {
        statementAssignment match {
            case StatementAssignment(left, right, OperatorAssignmentUpdate) =>
                Event((s, x) => StateLocation.resolve(s, x, left).map(update(s, _, evaluateExpression(s, x, right)).map((_, x))).flatten)

            case StatementAssignment(left, right, OperatorAssignmentInsert) =>
                Event((s, x) => StateLocation.resolve(s, x, left).map(insert(s, _, evaluateExpression(s, x, right)).map((_, x))).flatten)

            case StatementAssignment(left, right, OperatorAssignmentRemove) =>
                Event((s, x) => StateLocation.resolve(s, x, left).map(remove(s, _, evaluateExpression(s, x, right)).map((_, x))).flatten)
        }
    }

    def eventDeclare(declaration: DeclarationEntity): Event[State, SymbolMap, SymbolMap] = {
        declaration match {
            case DeclarationEntity(ExpressionIdentifier(kind), ExpressionEntity(ExpressionIdentifier(entity)), _) =>
                Event((s, x) => {
                    val id = idIterator.next()
                    Some((insertEntity(s, (kind, id)), x + (entity -> (kind -> id))))
                })
        }
    }

    def eventQuery(statementQuery: StatementQuery): Event[State, SymbolMap, SymbolMap] = {
        val generator: (Symbol) => (State) => List[Int] =
            (kind) => (state) => state.entitySetMap.get(kind).map(_.entityMap.keys.toList) getOrElse List[Int]()

        val predicate1: (State, SymbolMap, Seq[(Symbol, Symbol)], Expression) => (Int) => Boolean =
            (state, symbolMap, symbolSeq, expression) =>
                (id1) =>
                    evaluateExpression(state, (List(id1) zip symbolSeq).foldLeft(symbolMap)((m, t) => m + (t._2._2 -> (t._2._1 -> t._1))), expression).map(_.toSeq) match {
                        case Some(Seq(AttributeValueBoolean(value))) => value
                        case _ => false
                    }

        val predicate2: (State, SymbolMap, Seq[(Symbol, Symbol)], Expression) => (Int, Int) => Boolean =
            (state, symbolMap, symbolSeq, expression) =>
                (id1, id2) =>
                    evaluateExpression(state, (List(id1, id2) zip symbolSeq).foldLeft(symbolMap)((m, t) => m + (t._2._2 -> (t._2._1 -> t._1))), expression).map(_.toSeq) match {
                        case Some(Seq(AttributeValueBoolean(value))) => value
                        case _ => false
                    }

        val predicate3: (State, SymbolMap, Seq[(Symbol, Symbol)], Expression) => (Int, Int, Int) => Boolean =
            (state, symbolMap, symbolSeq, expression) =>
                (id1, id2, id3) =>
                    evaluateExpression(state, (List(id1, id2, id3) zip symbolSeq).foldLeft(symbolMap)((m, t) => m + (t._2._2 -> (t._2._1 -> t._1))), expression).map(_.toSeq) match {
                        case Some(Seq(AttributeValueBoolean(value))) => value
                        case _ => false
                    }

        val predicate4: (State, SymbolMap, Seq[(Symbol, Symbol)], Expression) => (Int, Int, Int, Int) => Boolean =
            (state, symbolMap, symbolSeq, expression) =>
                (id1, id2, id3, id4) =>
                    evaluateExpression(state, (List(id1, id2, id3, id4) zip symbolSeq).foldLeft(symbolMap)((m, t) => m + (t._2._2 -> (t._2._1 -> t._1))), expression).map(_.toSeq) match {
                        case Some(Seq(AttributeValueBoolean(value))) => value
                        case _ => false
                    }

        val predicate5: (State, SymbolMap, Seq[(Symbol, Symbol)], Expression) => (Int, Int, Int, Int, Int) => Boolean =
            (state, symbolMap, symbolSeq, expression) =>
                (id1, id2, id3, id4, id5) =>
                    evaluateExpression(state, (List(id1, id2, id3, id4, id5) zip symbolSeq).foldLeft(symbolMap)((m, t) => m + (t._2._2 -> (t._2._1 -> t._1))), expression).map(_.toSeq) match {
                        case Some(Seq(AttributeValueBoolean(value))) => value
                        case _ => false
                    }

        val predicate6: (State, SymbolMap, Seq[(Symbol, Symbol)], Expression) => (Int, Int, Int, Int, Int, Int) => Boolean =
            (state, symbolMap, symbolSeq, expression) =>
                (id1, id2, id3, id4, id5, id6) =>
                    evaluateExpression(state, (List(id1, id2, id3, id4, id5, id6) zip symbolSeq).foldLeft(symbolMap)((m, t) => m + (t._2._2 -> (t._2._1 -> t._1))), expression).map(_.toSeq) match {
                        case Some(Seq(AttributeValueBoolean(value))) => value
                        case _ => false
                    }

        val parameterList: (List[DeclarationParameter]) => Seq[(Symbol, Symbol)] =
            (declarationParameterList) =>
                declarationParameterList.foldLeft(Seq[(Symbol, Symbol)]())((symbolSeq, declarationParameter) =>
                    symbolSeq :+(declarationParameter.kind.identifier, declarationParameter.parameter.identifier))

        statementQuery match {
            case StatementQuery(Nil, right, OperatorQueryAll) =>
                Event((s, x) => evaluateExpression(s, x, right).map(_.toSeq) match {
                    case Some(Seq(AttributeValueBoolean(true))) => Some(s, x)
                    case _ => None
                })

            case StatementQuery(DeclarationParameter(ExpressionIdentifier(kind1), ExpressionIdentifier(entity1)) :: Nil, right, OperatorQueryAll) =>
                Event((s, x) => Predicate.given(generator(kind1)).thereExists(predicate1(s, x, Seq((kind1, entity1)), right))(s) match {
                    case Some(t) => Some(s, x + (entity1 ->(kind1, t)))
                    case None => None
                })

            case StatementQuery(DeclarationParameter(ExpressionIdentifier(kind1), ExpressionIdentifier(entity1)) :: DeclarationParameter(ExpressionIdentifier(kind2), ExpressionIdentifier(entity2)) :: Nil, right, OperatorQueryAll) =>
                Event((s, x) => Predicate.given(generator(kind1), generator(kind2)).thereExists(predicate2(s, x, Seq((kind1, entity1), (kind2, entity2)), right))(s) match {
                    case Some(t) => Some(s, x +
                        (entity1 ->(kind1, t._1)) +
                        (entity2 ->(kind2, t._2)))
                    case None => None
                })

            case StatementQuery(DeclarationParameter(ExpressionIdentifier(kind1), ExpressionIdentifier(entity1)) :: DeclarationParameter(ExpressionIdentifier(kind2), ExpressionIdentifier(entity2)) :: DeclarationParameter(ExpressionIdentifier(kind3), ExpressionIdentifier(entity3)) :: Nil, right, OperatorQueryAll) =>
                Event((s, x) => Predicate.given(generator(kind1), generator(kind2), generator(kind3)).thereExists(predicate3(s, x, Seq((kind1, entity1), (kind2, entity2), (kind3, entity3)), right))(s) match {
                    case Some(t) => Some(s, x +
                        (entity1 ->(kind1, t._1)) +
                        (entity2 ->(kind2, t._2)) +
                        (entity3 ->(kind3, t._3))
                    )
                    case None => None
                })

            case StatementQuery(DeclarationParameter(ExpressionIdentifier(kind1), ExpressionIdentifier(entity1)) :: DeclarationParameter(ExpressionIdentifier(kind2), ExpressionIdentifier(entity2)) :: DeclarationParameter(ExpressionIdentifier(kind3), ExpressionIdentifier(entity3)) :: DeclarationParameter(ExpressionIdentifier(kind4), ExpressionIdentifier(entity4)) :: Nil, right, OperatorQueryAll) =>
                Event((s, x) => Predicate.given(generator(kind1), generator(kind2), generator(kind3), generator(kind4)).thereExists(predicate4(s, x, Seq((kind1, entity1), (kind2, entity2), (kind3, entity3), (kind4, entity4)), right))(s) match {
                    case Some(t) => Some(s, x +
                        (entity1 ->(kind1, t._1)) +
                        (entity2 ->(kind2, t._2)) +
                        (entity3 ->(kind3, t._3)) +
                        (entity4 ->(kind4, t._4))
                    )
                    case None => None
                })

            case StatementQuery(DeclarationParameter(ExpressionIdentifier(kind1), ExpressionIdentifier(entity1)) :: DeclarationParameter(ExpressionIdentifier(kind2), ExpressionIdentifier(entity2)) :: DeclarationParameter(ExpressionIdentifier(kind3), ExpressionIdentifier(entity3)) :: DeclarationParameter(ExpressionIdentifier(kind4), ExpressionIdentifier(entity4)) :: DeclarationParameter(ExpressionIdentifier(kind5), ExpressionIdentifier(entity5)) :: Nil, right, OperatorQueryAll) =>
                Event((s, x) => Predicate.given(generator(kind1), generator(kind2), generator(kind3), generator(kind4), generator(kind5)).thereExists(predicate5(s, x, Seq((kind1, entity1), (kind2, entity2), (kind3, entity3), (kind4, entity4), (kind5, entity5)), right))(s) match {
                    case Some(t) => Some(s, x +
                        (entity1 ->(kind1, t._1)) +
                        (entity2 ->(kind2, t._2)) +
                        (entity3 ->(kind3, t._3)) +
                        (entity4 ->(kind4, t._4)) +
                        (entity5 ->(kind5, t._5))
                    )
                    case None => None
                })

            case StatementQuery(DeclarationParameter(ExpressionIdentifier(kind1), ExpressionIdentifier(entity1)) :: DeclarationParameter(ExpressionIdentifier(kind2), ExpressionIdentifier(entity2)) :: DeclarationParameter(ExpressionIdentifier(kind3), ExpressionIdentifier(entity3)) :: DeclarationParameter(ExpressionIdentifier(kind4), ExpressionIdentifier(entity4)) :: DeclarationParameter(ExpressionIdentifier(kind5), ExpressionIdentifier(entity5)) :: DeclarationParameter(ExpressionIdentifier(kind6), ExpressionIdentifier(entity6)) :: Nil, right, OperatorQueryAll) =>
                Event((s, x) => Predicate.given(generator(kind1), generator(kind2), generator(kind3), generator(kind4), generator(kind5), generator(kind6)).thereExists(predicate6(s, x, Seq((kind1, entity1), (kind2, entity2), (kind3, entity3), (kind4, entity4), (kind5, entity5), (kind6, entity6)), right))(s) match {
                    case Some(t) => Some(s, x +
                        (entity1 ->(kind1, t._1)) +
                        (entity2 ->(kind2, t._2)) +
                        (entity3 ->(kind3, t._3)) +
                        (entity4 ->(kind4, t._4)) +
                        (entity5 ->(kind5, t._5)) +
                        (entity5 ->(kind6, t._6))
                    )
                    case None => None
                })

            case StatementQuery(Nil, right, OperatorQueryNone) =>
                Event((s, x) => evaluateExpression(s, x, right).map(_.toSeq) match {
                    case Some(Seq(AttributeValueBoolean(false))) => Some(s, x)
                    case _ => None
                })

            case StatementQuery(DeclarationParameter(ExpressionIdentifier(kind1), ExpressionIdentifier(entity1)) :: Nil, right, OperatorQueryNone) =>
                Event((s, x) => Predicate.given(generator(kind1)).thereExists(predicate1(s, x, Seq((kind1, entity1)), right))(s) match {
                    case Some(t) => None
                    case None => Some(s, x)
                })

            case StatementQuery(DeclarationParameter(ExpressionIdentifier(kind1), ExpressionIdentifier(entity1)) :: DeclarationParameter(ExpressionIdentifier(kind2), ExpressionIdentifier(entity2)) :: Nil, right, OperatorQueryNone) =>
                Event((s, x) => Predicate.given(generator(kind1), generator(kind2)).thereExists(predicate2(s, x, Seq((kind1, entity1), (kind2, entity2)), right))(s) match {
                    case Some(t) => None
                    case None => Some(s, x)
                })

            case StatementQuery(DeclarationParameter(ExpressionIdentifier(kind1), ExpressionIdentifier(entity1)) :: DeclarationParameter(ExpressionIdentifier(kind2), ExpressionIdentifier(entity2)) :: DeclarationParameter(ExpressionIdentifier(kind3), ExpressionIdentifier(entity3)) :: Nil, right, OperatorQueryNone) =>
                Event((s, x) => Predicate.given(generator(kind1), generator(kind2), generator(kind3)).thereExists(predicate3(s, x, Seq((kind1, entity1), (kind2, entity2), (kind3, entity3)), right))(s) match {
                    case Some(t) => None
                    case None => Some(s, x)
                })

            case StatementQuery(DeclarationParameter(ExpressionIdentifier(kind1), ExpressionIdentifier(entity1)) :: DeclarationParameter(ExpressionIdentifier(kind2), ExpressionIdentifier(entity2)) :: DeclarationParameter(ExpressionIdentifier(kind3), ExpressionIdentifier(entity3)) :: DeclarationParameter(ExpressionIdentifier(kind4), ExpressionIdentifier(entity4)) :: Nil, right, OperatorQueryNone) =>
                Event((s, x) => Predicate.given(generator(kind1), generator(kind2), generator(kind3), generator(kind4)).thereExists(predicate4(s, x, Seq((kind1, entity1), (kind2, entity2), (kind3, entity3), (kind4, entity4)), right))(s) match {
                    case Some(t) => None
                    case None => Some(s, x)
                })

            case StatementQuery(DeclarationParameter(ExpressionIdentifier(kind1), ExpressionIdentifier(entity1)) :: DeclarationParameter(ExpressionIdentifier(kind2), ExpressionIdentifier(entity2)) :: DeclarationParameter(ExpressionIdentifier(kind3), ExpressionIdentifier(entity3)) :: DeclarationParameter(ExpressionIdentifier(kind4), ExpressionIdentifier(entity4)) :: DeclarationParameter(ExpressionIdentifier(kind5), ExpressionIdentifier(entity5)) :: Nil, right, OperatorQueryNone) =>
                Event((s, x) => Predicate.given(generator(kind1), generator(kind2), generator(kind3), generator(kind4), generator(kind5)).thereExists(predicate5(s, x, Seq((kind1, entity1), (kind2, entity2), (kind3, entity3), (kind4, entity4), (kind5, entity5)), right))(s) match {
                    case Some(t) => None
                    case None => Some(s, x)
                })

            case StatementQuery(DeclarationParameter(ExpressionIdentifier(kind1), ExpressionIdentifier(entity1)) :: DeclarationParameter(ExpressionIdentifier(kind2), ExpressionIdentifier(entity2)) :: DeclarationParameter(ExpressionIdentifier(kind3), ExpressionIdentifier(entity3)) :: DeclarationParameter(ExpressionIdentifier(kind4), ExpressionIdentifier(entity4)) :: DeclarationParameter(ExpressionIdentifier(kind5), ExpressionIdentifier(entity5)) :: DeclarationParameter(ExpressionIdentifier(kind6), ExpressionIdentifier(entity6)) :: Nil, right, OperatorQueryNone) =>
                Event((s, x) => Predicate.given(generator(kind1), generator(kind2), generator(kind3), generator(kind4), generator(kind5), generator(kind6)).thereExists(predicate6(s, x, Seq((kind1, entity1), (kind2, entity2), (kind3, entity3), (kind4, entity4), (kind5, entity5), (kind6, entity6)), right))(s) match {
                    case Some(t) => None
                    case None => Some(s, x)
                })
        }
    }

    def eventSubgoal(statementSubgoal: StatementSubgoal, goalFor: Symbol => Goal[State, SymbolMap, SymbolMap], declarationFor: Symbol => DeclarationGoal): Subgoal[State, SymbolMap, SymbolMap, SymbolMap, SymbolMap] = {
        val goal = new Goal[State, SymbolMap, SymbolMap]() {
            def label: String = {
                goalFor(statementSubgoal.goal.identifier).label
            }

            def strategySet: Set[Strategy[State, SymbolMap, SymbolMap]] = {
                goalFor(statementSubgoal.goal.identifier).strategySet
            }

            def satisfy(state: State, input: SymbolMap): GoalExecution[State, SymbolMap, SymbolMap] = {
                goalFor(statementSubgoal.goal.identifier).satisfy(state, input)
            }

        }

        Subgoal((s, x) => Map(declarationFor(statementSubgoal.goal.identifier).parameterList.map(_.parameter.identifier).zip(statementSubgoal.parameterList.map(parameter => x(parameter.identifier))).toSeq: _*),
            goal,
            (s, x, y1) => x)
    }

    def eventNarration(statementNarration: StatementNarration): Event[State, SymbolMap, SymbolMap] = {
        Event((s, x) => {
            val turn: Map[String, Map[String, String]] = x.map((p) => p._1.name -> s.entitySetMap(p._2._1).entityMap(p._2._2).attributeMap.map((a) => a._1.name -> a._2.map({
                case AttributeValueBoolean(boolean) => boolean
                case AttributeValueEntity(entity) => s"""(${entity._1}, ${entity._2})"""
                case AttributeValueNumber(number) => number
                case AttributeValueRelationshipBidirectional(left, right) => s"""(${left._1}, ${left._2}) <-> (${right._1}, ${right._2})"""
                case AttributeValueRelationshipUnidirectional(left, right) => s"""(${left._1}, ${left._2}) <-> (${right._1}, ${right._2})"""
                case AttributeValueSymbol(symbol) => symbol.name
                case AttributeValueSymbolOrdered(symbol, symbolSet) => symbol.name
            }).mkString(", ")))
            val t = Handlebars(statementNarration.string.value)

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

            Some(s.applyLens(State.focusNarration).modify(_ => Some(t(turnWithGender))), x)
        })

    }
}