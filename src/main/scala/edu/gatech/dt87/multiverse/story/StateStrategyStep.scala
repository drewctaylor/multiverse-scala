package edu.gatech.dt87.multiverse.story

import com.gilt.handlebars.scala.Handlebars
import com.gilt.handlebars.scala.binding.dynamic._
import edu.gatech.dt87.multiverse.planner._
import edu.gatech.dt87.multiverse.predicate.Predicate
import edu.gatech.dt87.multiverse.story.dsl.parser._
import edu.gatech.dt87.multiverse.story.state._
import monocle.function._
import monocle.std._
import monocle.syntax._

object StateStrategyStep {
    type SymbolMap = Map[Symbol, (Symbol, Int)]
    type EventT = Event[State, SymbolMap, SymbolMap]
    type SubgoalT = Subgoal[State, SymbolMap, SymbolMap, SymbolMap, SymbolMap]
    type StrategyStepT = StrategyStep[State, SymbolMap, SymbolMap]
    type StrategyT = Strategy[State, SymbolMap, SymbolMap]
    type GoalT = Goal[State, SymbolMap, SymbolMap]

    val idIterator = Iterator.from(0)

    /**
     * Given an assignment statement, return an event that binds an entity to a symbol as follows:
     *
     * 1) If the symbol exists
     *      If the symbol references an entity of a kind the same as the given entity kind,
     *        Succeed: the referenced entity is the same as the given entity.
     *      If the system references an entity of a kind different from the given kind,
     *        Fail: the symbol is in use.
     *
     * 2) If the symbol does not exists
     *      Bind the entity to the given symbol.
     *
     * @param statementAssignment the assignment statement
     * @return an event that binds an entity to a symbol.
     */
    def bind(statementAssignment: StatementAssignmentQualified): StrategyStepT = {
        statementAssignment match {
            case StatementAssignmentQualified(IdentifierAttributeQualified(IdentifierEntity(Some(kind), symbol), _), _, _) =>
                Event((state, symbolMap) => {
                    symbolMap.get(symbol) match {
                        case Some((k, i)) =>
                            if (kind == k) {
                                Some(state, symbolMap)
                            } else {
                                println( s"""The system could not reassign the symbol "${symbol.name}" to reference an entity of type "${kind.name}", as it already referenced an entity of type "${k.name}".""")
                                None
                            }
                        case None =>
                            val id = idIterator.next()
                            Some((State.insert(state, (kind, id)), symbolMap + (symbol -> (kind -> id))))
                    }
                })

            case _ =>
                Event((s, x) => Some(s, x))
        }
    }

    /**
     * Given an assignment statement, return an event that executes the assignment statement.
     *
     * @param statementAssignment the assignment statement
     * @return an event that executes the assignment statement.
     */
    def assign(statementAssignment: StatementAssignmentQualified): StrategyStepT = {
        statementAssignment match {
            case StatementAssignmentQualified(left, right, OperatorAssignmentUpdate) =>
                Event((state, symbolMap) =>
                    StateLocation.resolve(state, symbolMap, left).map(location =>
                        State.update(state, location, AttributeValueOperation.evaluateExpression(state, symbolMap, right)).map((_, symbolMap))).flatten)

            case StatementAssignmentQualified(left, right, OperatorAssignmentInsert) =>
                Event((state, symbolMap) =>
                    StateLocation.resolve(state, symbolMap, left).map(location =>
                        State.insert(state, location, AttributeValueOperation.evaluateExpression(state, symbolMap, right)).map((_, symbolMap))).flatten)

            case StatementAssignmentQualified(left, right, OperatorAssignmentRemove) =>
                Event((state, symbolMap) =>
                    StateLocation.resolve(state, symbolMap, left).map(location =>
                        State.remove(state, location, AttributeValueOperation.evaluateExpression(state, symbolMap, right)).map((_, symbolMap))).flatten)
        }
    }

    def query(statementQuery: StatementQuery): EventT = {
        val generator: (Symbol) => (State) => List[Int] =
            (kind) => (state) => state.entitySetMap.get(kind).map(_.entityMap.keys.toList) getOrElse List[Int]()

        val predicate1: (State, SymbolMap, Seq[(Symbol, Symbol)], Expression) => (Int) => Boolean =
            (state, symbolMap, symbolSeq, expression) =>
                (id1) =>
                    AttributeValueOperation.evaluateExpression(state, (List(id1) zip symbolSeq).foldLeft(symbolMap)((m, t) => m + (t._2._2 -> (t._2._1 -> t._1))), expression).map(_.toSeq) match {
                        case Some(Seq(AttributeValueBoolean(value))) => value
                        case _ =>
//                            println(s"""The query predicate "${expression.toString}" did not produce a boolean value.""")
                            false

                    }

        val predicate2: (State, SymbolMap, Seq[(Symbol, Symbol)], Expression) => (Int, Int) => Boolean =
            (state, symbolMap, symbolSeq, expression) =>
                (id1, id2) =>
                    AttributeValueOperation.evaluateExpression(state, (List(id1, id2) zip symbolSeq).foldLeft(symbolMap)((m, t) => m + (t._2._2 -> (t._2._1 -> t._1))), expression).map(_.toSeq) match {
                        case Some(Seq(AttributeValueBoolean(value))) => value
                        case _ =>
//                            println(s"""The query predicate "${expression.toString}" did not produce a boolean value.""")
                            false

                    }

        val predicate3: (State, SymbolMap, Seq[(Symbol, Symbol)], Expression) => (Int, Int, Int) => Boolean =
            (state, symbolMap, symbolSeq, expression) =>
                (id1, id2, id3) =>
                    AttributeValueOperation.evaluateExpression(state, (List(id1, id2, id3) zip symbolSeq).foldLeft(symbolMap)((m, t) => m + (t._2._2 -> (t._2._1 -> t._1))), expression).map(_.toSeq) match {
                        case Some(Seq(AttributeValueBoolean(value))) => value
                        case _ =>
//                            println(s"""The query predicate "${expression.toString}" did not produce a boolean value.""")
                            false

                    }

        val predicate4: (State, SymbolMap, Seq[(Symbol, Symbol)], Expression) => (Int, Int, Int, Int) => Boolean =
            (state, symbolMap, symbolSeq, expression) =>
                (id1, id2, id3, id4) =>
                    AttributeValueOperation.evaluateExpression(state, (List(id1, id2, id3, id4) zip symbolSeq).foldLeft(symbolMap)((m, t) => m + (t._2._2 -> (t._2._1 -> t._1))), expression).map(_.toSeq) match {
                        case Some(Seq(AttributeValueBoolean(value))) => value
                        case _ =>
//                            println(s"""The query predicate "${expression.toString}" did not produce a boolean value.""")
                            false

                    }

        val predicate5: (State, SymbolMap, Seq[(Symbol, Symbol)], Expression) => (Int, Int, Int, Int, Int) => Boolean =
            (state, symbolMap, symbolSeq, expression) =>
                (id1, id2, id3, id4, id5) =>
                    AttributeValueOperation.evaluateExpression(state, (List(id1, id2, id3, id4, id5) zip symbolSeq).foldLeft(symbolMap)((m, t) => m + (t._2._2 -> (t._2._1 -> t._1))), expression).map(_.toSeq) match {
                        case Some(Seq(AttributeValueBoolean(value))) => value
                        case _ =>
//                            println(s"""The query predicate "${expression.toString}" did not produce a boolean value.""")
                            false

                    }

        val predicate6: (State, SymbolMap, Seq[(Symbol, Symbol)], Expression) => (Int, Int, Int, Int, Int, Int) => Boolean =
            (state, symbolMap, symbolSeq, expression) =>
                (id1, id2, id3, id4, id5, id6) =>
                    AttributeValueOperation.evaluateExpression(state, (List(id1, id2, id3, id4, id5, id6) zip symbolSeq).foldLeft(symbolMap)((m, t) => m + (t._2._2 -> (t._2._1 -> t._1))), expression).map(_.toSeq) match {
                        case Some(Seq(AttributeValueBoolean(value))) => value
                        case _ =>
//                            println(s"""The query predicate "${expression.toString}" did not produce a boolean value.""")
                            false

                    }

        statementQuery match {
            case StatementQuery(Nil, all, exists) =>
                Event((s, x) =>
                    (all, exists) match {
                        case (Some(a), Some(e)) =>
                            AttributeValueOperation.evaluateExpression(s, x, ExpressionBinary(a, e, OperatorAnd)).map(_.toSeq) match {
                                case Some(Seq(AttributeValueBoolean(true))) => Some(s, x)
                                case _ => None
                            }
                        case (Some(a), None) =>
                            AttributeValueOperation.evaluateExpression(s, x, a).map(_.toSeq) match {
                                case Some(Seq(AttributeValueBoolean(true))) => Some(s, x)
                                case _ => None
                            }
                        case (None, Some(e)) =>
                            AttributeValueOperation.evaluateExpression(s, x, e).map(_.toSeq) match {
                                case Some(Seq(AttributeValueBoolean(true))) => Some(s, x)
                                case _ => None
                            }
                        case (None, None) =>
                            None
                    })

            case StatementQuery((kind1, symbol1) :: Nil, allOption, existsOption) =>
                Event((s, x) => {
                    val p1 = Predicate.given(generator(kind1))
                    val p2 = allOption.map(all => p1.forAll(predicate1(s, x, Seq((kind1, symbol1)), all))) getOrElse p1
                    val p3 = existsOption.map(exists => p2.thereExists(predicate1(s, x, Seq((kind1, symbol1)), exists))) getOrElse p2

                    p3(s) match {
                        case Some(t) => Some(s, x + (symbol1 ->(kind1, t)))
                        case None => None
                    }
                })

            case StatementQuery((kind1, entity1) :: (kind2, entity2) :: Nil, allOption, existsOption) =>
                Event((s, x) => {
                    val p1 = Predicate.given(generator(kind1), generator(kind2))
                    val p2 = allOption.map(all => p1.forAll(predicate2(s, x, Seq((kind1, entity1), (kind2, entity2)), all))) getOrElse p1
                    val p3 = existsOption.map(exists => p2.thereExists(predicate2(s, x, Seq((kind1, entity1), (kind2, entity2)), exists))) getOrElse p2

                    p3(s) match {
                        case Some(t) => Some(s, x +
                            (entity1 ->(kind1, t._1)) +
                            (entity2 ->(kind2, t._2)))
                        case None => None
                    }
                })

            case StatementQuery((kind1, entity1) :: (kind2, entity2) :: (kind3, entity3) :: Nil, allOption, existsOption) =>
                Event((s, x) => {
                    val p1 = Predicate.given(generator(kind1), generator(kind2), generator(kind3))
                    val p2 = allOption.map(all => p1.forAll(predicate3(s, x, Seq((kind1, entity1), (kind2, entity2), (kind3, entity3)), all))) getOrElse p1
                    val p3 = existsOption.map(exists => p2.thereExists(predicate3(s, x, Seq((kind1, entity1), (kind2, entity2), (kind3, entity3)), exists))) getOrElse p2

                    p3(s) match {
                        case Some(t) => Some(s, x +
                            (entity1 ->(kind1, t._1)) +
                            (entity2 ->(kind2, t._2)) +
                            (entity3 ->(kind3, t._3))
                        )
                        case None => None
                    }
                })

            case StatementQuery((kind1, entity1) :: (kind2, entity2) :: (kind3, entity3) :: (kind4, entity4) :: Nil, allOption, existsOption) =>
                Event((s, x) => {
                    val p1 = Predicate.given(generator(kind1), generator(kind2), generator(kind3), generator(kind4))
                    val p2 = allOption.map(all => p1.forAll(predicate4(s, x, Seq((kind1, entity1), (kind2, entity2), (kind3, entity3), (kind4, entity4)), all))) getOrElse p1
                    val p3 = existsOption.map(exists => p2.thereExists( predicate4(s, x, Seq((kind1, entity1), (kind2, entity2), (kind3, entity3), (kind4, entity4)), exists))) getOrElse p2

                    p3(s) match {
                        case Some(t) => Some(s, x +
                            (entity1 ->(kind1, t._1)) +
                            (entity2 ->(kind2, t._2)) +
                            (entity3 ->(kind3, t._3)) +
                            (entity4 ->(kind4, t._4))
                        )
                        case None => None
                    }
                })

            case StatementQuery((kind1, entity1) :: (kind2, entity2) :: (kind3, entity3) :: (kind4, entity4) :: (kind5, entity5) :: Nil, allOption, existsOption) =>
                Event((s, x) => {
                    val p1 = Predicate.given(generator(kind1), generator(kind2), generator(kind3), generator(kind4), generator(kind5))
                    val p2 = allOption.map(all => p1.forAll(predicate5(s, x, Seq((kind1, entity1), (kind2, entity2), (kind3, entity3), (kind4, entity4), (kind5, entity5)), all))) getOrElse p1
                    val p3 = existsOption.map(exists => p2.thereExists(predicate5(s, x, Seq((kind1, entity1), (kind2, entity2), (kind3, entity3), (kind4, entity4), (kind5, entity5)), exists))) getOrElse p2

                    p3(s) match {
                        case Some(t) => Some(s, x +
                            (entity1 ->(kind1, t._1)) +
                            (entity2 ->(kind2, t._2)) +
                            (entity3 ->(kind3, t._3)) +
                            (entity4 ->(kind4, t._4)) +
                            (entity5 ->(kind5, t._5))
                        )
                        case None => None
                    }
                })

            case StatementQuery((kind1, entity1) :: (kind2, entity2) :: (kind3, entity3) :: (kind4, entity4) :: (kind5, entity5) :: (kind6, entity6) :: Nil, allOption, existsOption) =>
                Event((s, x) => {
                    val p1 = Predicate.given(generator(kind1), generator(kind2), generator(kind3), generator(kind4), generator(kind5), generator(kind6))
                    val p2 = allOption.map(all => p1.forAll(predicate6(s, x, Seq((kind1, entity1), (kind2, entity2), (kind3, entity3), (kind4, entity4), (kind5, entity5), (kind6, entity6)), all))) getOrElse p1
                    val p3 = existsOption.map(exists => p2.thereExists(predicate6(s, x, Seq((kind1, entity1), (kind2, entity2), (kind3, entity3), (kind4, entity4), (kind5, entity5), (kind6, entity6)), exists))) getOrElse p2

                    p3(s) match {
                        case Some(t) => Some(s, x +
                            (entity1 ->(kind1, t._1)) +
                            (entity2 ->(kind2, t._2)) +
                            (entity3 ->(kind3, t._3)) +
                            (entity4 ->(kind4, t._4)) +
                            (entity5 ->(kind5, t._5)) +
                            (entity5 ->(kind6, t._6))
                        )
                        case None => None
                    }
                })
        }
    }

    /**
     * Given a subgoal statement, return an event that executes the subgoal statement.
     *
     * @param statementSubgoal the subgoal statement
     * @param goalFor a function that, when given a goal symbol, returns a goal declaration and a goal
     * @return an event that executes the subgoal statement
     */
    def subgoal(statementSubgoal: StatementSubgoal, goalFor: Symbol => (Goal[State, SymbolMap, SymbolMap], DeclarationGoal)): StrategyStepT = {
        val goal = new Goal[State, SymbolMap, SymbolMap] {
            def label: String = {
                goalFor(statementSubgoal.goal)._1.label
            }

            def strategySet: Set[Strategy[State, SymbolMap, SymbolMap]] = {
                goalFor(statementSubgoal.goal)._1.strategySet
            }

            def satisfy(state: State, input: SymbolMap): GoalExecution[State, SymbolMap, SymbolMap] = {
                goalFor(statementSubgoal.goal)._1.satisfy(state, input)
            }
        }

        val transformInput: (State, SymbolMap) => SymbolMap = (state, symbolMapOld) => {
            goalFor(statementSubgoal.goal)._2.parameterList
                .zip(statementSubgoal.parameterList)
                .foldLeft(List[(Symbol, (Symbol, Int))]())((symbolMapNew, tuple) =>
                tuple match {
                    case ((kindExpected, symbol), expression) =>
                        AttributeValueOperation.evaluateExpression(state, symbolMapOld, expression).map(_.toSeq) match {
                            case Some(Seq(AttributeValueEntity(entity))) =>
                                val (kindActual, id) = entity

                                if (kindExpected == kindActual) {
                                    symbolMapNew :+ (symbol ->(kindActual, id))

                                } else {
                                    println( s"""The argument "${expression.toString}" to the subgoal "${statementSubgoal.goal}" does not reference an entity of type "${kindExpected.name}.""")
                                    symbolMapNew

                                }
                            case _ =>
                                println( s"""The argument "${expression.toString}" to the subgoal "${statementSubgoal.goal}" does not reference an entity of type "${kindExpected.name}.""")
                                symbolMapNew
                        }
                }).toMap
        }

        val transformOutput: (State, SymbolMap, SymbolMap) => SymbolMap = (state, symbolMapInput, symbolMapOutput) => {
            symbolMapInput
        }

        Subgoal[State, SymbolMap, SymbolMap, SymbolMap, SymbolMap](
            transformInput,
            goal,
            transformOutput
        )
    }


    def narrate(statementNarration: StatementNarration): Event[State, SymbolMap, SymbolMap] = {
        Event((state, x) => {
            val turn: Map[String, Map[String, String]] = x.map((p) => p._1.name -> state.entitySetMap(p._2._1).entityMap(p._2._2).attributeMap.map((a) => a._1.name -> a._2.map({
                case AttributeValueBoolean(boolean) => boolean
                case AttributeValueEntity(entity) => s"""(${entity._1}, ${entity._2})"""
                case AttributeValueNumber(number) => number
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

            Some(state.applyLens(State.focusNarration).modify(_ => Some(t(turnWithGender))), x)
        })

    }
}