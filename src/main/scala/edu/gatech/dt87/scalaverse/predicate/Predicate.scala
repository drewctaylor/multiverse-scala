package edu.gatech.dt87.scalaverse.predicate

import edu.gatech.dt87.scalaverse.random.Random

object Predicate {

    def forAll[A, S](generatorA: (S) => List[A],
                     predicate: (A) => Boolean): (S) => Boolean = {
        (state) => generatorA(state).forall(predicate)
    }

    def forAll[A, B, S](generatorA: (S) => List[A],
                        generatorB: (S) => List[B],
                        predicate: (A, B) => Boolean): (S) => Boolean = {
        (state) =>
            (for {
                a <- generatorA(state)
                b <- generatorB(state)
            } yield (a, b)).forall(predicate.tupled)
    }

    def forAll[A, B, C, S](generatorA: (S) => List[A],
                           generatorB: (S) => List[B],
                           generatorC: (S) => List[C],
                           predicate: (A, B, C) => Boolean): (S) => Boolean = {
        (state) =>
            (for {
                a <- generatorA(state)
                b <- generatorB(state)
                c <- generatorC(state)
            } yield (a, b, c)).forall(predicate.tupled)
    }

    def forAll[A, B, C, D, S](generatorA: (S) => List[A],
                              generatorB: (S) => List[B],
                              generatorC: (S) => List[C],
                              generatorD: (S) => List[D],
                              predicate: (A, B, C, D) => Boolean): (S) => Boolean = {
        (state) =>
            (for {
                a <- generatorA(state)
                b <- generatorB(state)
                c <- generatorC(state)
                d <- generatorD(state)
            } yield (a, b, c, d)).forall(predicate.tupled)
    }

    def forAll[A, B, C, D, E, S](generatorA: (S) => List[A],
                                 generatorB: (S) => List[B],
                                 generatorC: (S) => List[C],
                                 generatorD: (S) => List[D],
                                 generatorE: (S) => List[E],
                                 predicate: (A, B, C, D, E) => Boolean): (S) => Boolean = {
        (state) =>
            (for {
                a <- generatorA(state)
                b <- generatorB(state)
                c <- generatorC(state)
                d <- generatorD(state)
                e <- generatorE(state)
            } yield (a, b, c, d, e)).forall(predicate.tupled)
    }

    def forAll[A, B, C, D, E, F, S](generatorA: (S) => List[A],
                                 generatorB: (S) => List[B],
                                 generatorC: (S) => List[C],
                                 generatorD: (S) => List[D],
                                 generatorE: (S) => List[E],
                                 generatorF: (S) => List[F],
                                 predicate: (A, B, C, D, E, F) => Boolean): (S) => Boolean = {
        (state) =>
            (for {
                a <- generatorA(state)
                b <- generatorB(state)
                c <- generatorC(state)
                d <- generatorD(state)
                e <- generatorE(state)
                f <- generatorF(state)
            } yield (a, b, c, d, e, f)).forall(predicate.tupled)
    }

    def thereExists[A, S](generatorA: (S) => List[A],
                          predicate: (A) => Boolean): (S) => Option[A] = {
        (state) => Random.shuffle(generatorA(state)).find(predicate)
    }

    def thereExists[A, B, S](generatorA: (S) => List[A],
                             generatorB: (S) => List[B],
                             predicate: (A, B) => Boolean): (S) => Option[(A, B)] = {
        (state) =>
            Random.shuffle(for {
                a <- generatorA(state)
                b <- generatorB(state)
            } yield (a, b)).find(predicate.tupled)

    }

    def thereExists[A, B, C, S](generatorA: (S) => List[A],
                                generatorB: (S) => List[B],
                                generatorC: (S) => List[C],
                                predicate: (A, B, C) => Boolean): (S) => Option[(A, B, C)] = {
        (state) =>
            Random.shuffle(for {
                a <- generatorA(state)
                b <- generatorB(state)
                c <- generatorC(state)
            } yield (a, b, c)).find(predicate.tupled)
    }

    def thereExists[A, B, C, D, S](generatorA: (S) => List[A],
                                   generatorB: (S) => List[B],
                                   generatorC: (S) => List[C],
                                   generatorD: (S) => List[D],
                                   predicate: (A, B, C, D) => Boolean): (S) => Option[(A, B, C, D)] = {
        (state) =>
            Random.shuffle(for {
                a <- generatorA(state)
                b <- generatorB(state)
                c <- generatorC(state)
                d <- generatorD(state)
            } yield (a, b, c, d)).find(predicate.tupled)
    }

    def thereExists[A, B, C, D, E, S](generatorA: (S) => List[A],
                                      generatorB: (S) => List[B],
                                      generatorC: (S) => List[C],
                                      generatorD: (S) => List[D],
                                      generatorE: (S) => List[E],
                                      predicate: (A, B, C, D, E) => Boolean): (S) => Option[(A, B, C, D, E)] = {
        (state) =>
            Random.shuffle(for {
                a <- generatorA(state)
                b <- generatorB(state)
                c <- generatorC(state)
                d <- generatorD(state)
                e <- generatorE(state)
            } yield (a, b, c, d, e)).find(predicate.tupled)
    }


    def thereExists[A, B, C, D, E, F, S](generatorA: (S) => List[A],
                                      generatorB: (S) => List[B],
                                      generatorC: (S) => List[C],
                                      generatorD: (S) => List[D],
                                      generatorE: (S) => List[E],
                                      generatorF: (S) => List[F],
                                      predicate: (A, B, C, D, E, F) => Boolean): (S) => Option[(A, B, C, D, E, F)] = {
        (state) =>
            Random.shuffle(for {
                a <- generatorA(state)
                b <- generatorB(state)
                c <- generatorC(state)
                d <- generatorD(state)
                e <- generatorE(state)
                f <- generatorF(state)
            } yield (a, b, c, d, e, f)).find(predicate.tupled)
    }

    class Given1[A, S](generatorA: (S) => List[A],
                       forAllList: List[(S) => Boolean],
                       thereExists: (S) => Option[A]) {

        def this(generatorA: (S) => List[A],
                 forAllList: List[(S) => Boolean] = List()) = {

            this(generatorA, forAllList, (state) => Some(
                Random.shuffle(generatorA(state)).head
            ))
        }

        def forAll(predicate: (A) => Boolean): Given1[A, S] = {
            new Given1(generatorA, Predicate.forAll(generatorA, predicate) :: forAllList, thereExists)
        }

        def thereExists(predicate: (A) => Boolean): Given1[A, S] = {
            new Given1(generatorA, forAllList, Predicate.thereExists(generatorA, predicate))
        }

        def apply(state: S): Option[A] = {
            if (forAllList.forall(forAll => forAll(state))) thereExists(state) else None
        }
    }

    class Given2[A, B, S](generatorA: (S) => List[A],
                          generatorB: (S) => List[B],
                          forAllList: List[(S) => Boolean],
                          thereExists: (S) => Option[(A, B)]) {

        def this(generatorA: (S) => List[A],
                 generatorB: (S) => List[B],
                 forAllList: List[(S) => Boolean] = List()) = {

            this(generatorA, generatorB, forAllList, (state) => Some(
                Random.shuffle(generatorA(state)).head,
                Random.shuffle(generatorB(state)).head
            ))
        }

        def forAll(predicate: (A, B) => Boolean): Given2[A, B, S] = {
            new Given2(generatorA, generatorB, Predicate.forAll(generatorA, generatorB, predicate) :: forAllList, thereExists)
        }

        def thereExists(predicate: (A, B) => Boolean): Given2[A, B, S] = {
            new Given2(generatorA, generatorB, forAllList, Predicate.thereExists(generatorA, generatorB, predicate))
        }

        def apply(state: S): Option[(A, B)] = {
            if (forAllList.forall(forAll => forAll(state))) thereExists(state) else None
        }
    }

    class Given3[A, B, C, S](generatorA: (S) => List[A],
                             generatorB: (S) => List[B],
                             generatorC: (S) => List[C],
                             forAllList: List[(S) => Boolean],
                             thereExists: (S) => Option[(A, B, C)]) {

        def this(generatorA: (S) => List[A],
                 generatorB: (S) => List[B],
                 generatorC: (S) => List[C],
                 forAllList: List[(S) => Boolean] = List()) = {

            this(generatorA, generatorB, generatorC, forAllList, (state) => Some(
                Random.shuffle(generatorA(state)).head,
                Random.shuffle(generatorB(state)).head,
                Random.shuffle(generatorC(state)).head
            ))
        }

        def forAll(predicate: (A, B, C) => Boolean): Given3[A, B, C, S] = {
            new Given3(generatorA, generatorB, generatorC, Predicate.forAll(generatorA, generatorB, generatorC, predicate) :: forAllList, thereExists)
        }

        def thereExists(predicate: (A, B, C) => Boolean): Given3[A, B, C, S] = {
            new Given3(generatorA, generatorB, generatorC, forAllList, Predicate.thereExists(generatorA, generatorB, generatorC, predicate))
        }

        def apply(state: S): Option[(A, B, C)] = {
            if (forAllList.forall(forAll => forAll(state))) thereExists(state) else None
        }
    }

    class Given4[A, B, C, D, S](generatorA: (S) => List[A],
                                generatorB: (S) => List[B],
                                generatorC: (S) => List[C],
                                generatorD: (S) => List[D],
                                forAllList: List[(S) => Boolean],
                                thereExists: (S) => Option[(A, B, C, D)]) {

        def this(generatorA: (S) => List[A],
                 generatorB: (S) => List[B],
                 generatorC: (S) => List[C],
                 generatorD: (S) => List[D],
                 forAllList: List[(S) => Boolean] = List()) = {

            this(generatorA, generatorB, generatorC, generatorD, forAllList, (state) => Some(
                Random.shuffle(generatorA(state)).head,
                Random.shuffle(generatorB(state)).head,
                Random.shuffle(generatorC(state)).head,
                Random.shuffle(generatorD(state)).head
            ))
        }

        def forAll(predicate: (A, B, C, D) => Boolean): Given4[A, B, C, D, S] = {
            new Given4(generatorA, generatorB, generatorC, generatorD, Predicate.forAll(generatorA, generatorB, generatorC, generatorD, predicate) :: forAllList, thereExists)
        }

        def thereExists(predicate: (A, B, C, D) => Boolean): Given4[A, B, C, D, S] = {
            new Given4(generatorA, generatorB, generatorC, generatorD, forAllList, Predicate.thereExists(generatorA, generatorB, generatorC, generatorD, predicate))
        }

        def apply(state: S): Option[(A, B, C, D)] = {
            if (forAllList.forall(forAll => forAll(state))) thereExists(state) else None
        }
    }

    class Given5[A, B, C, D, E, S](generatorA: (S) => List[A],
                                   generatorB: (S) => List[B],
                                   generatorC: (S) => List[C],
                                   generatorD: (S) => List[D],
                                   generatorE: (S) => List[E],
                                   forAllList: List[(S) => Boolean],
                                   thereExists: (S) => Option[(A, B, C, D, E)]) {

        def this(generatorA: (S) => List[A],
                 generatorB: (S) => List[B],
                 generatorC: (S) => List[C],
                 generatorD: (S) => List[D],
                 generatorE: (S) => List[E],
                 forAllList: List[(S) => Boolean] = List()) = {

            this(generatorA, generatorB, generatorC, generatorD, generatorE, forAllList, (state) => Some(
                Random.shuffle(generatorA(state)).head,
                Random.shuffle(generatorB(state)).head,
                Random.shuffle(generatorC(state)).head,
                Random.shuffle(generatorD(state)).head,
                Random.shuffle(generatorE(state)).head
            ))
        }

        def forAll(predicate: (A, B, C, D, E) => Boolean): Given5[A, B, C, D, E, S] = {
            new Given5(generatorA, generatorB, generatorC, generatorD, generatorE, Predicate.forAll(generatorA, generatorB, generatorC, generatorD, generatorE, predicate) :: forAllList, thereExists)
        }

        def thereExists(predicate: (A, B, C, D, E) => Boolean): Given5[A, B, C, D, E, S] = {
            new Given5(generatorA, generatorB, generatorC, generatorD, generatorE, forAllList, Predicate.thereExists(generatorA, generatorB, generatorC, generatorD, generatorE, predicate))
        }

        def apply(state: S): Option[(A, B, C, D, E)] = {
            if (forAllList.forall(forAll => forAll(state))) thereExists(state) else None
        }
    }


    class Given6[A, B, C, D, E, F, S](generatorA: (S) => List[A],
                                   generatorB: (S) => List[B],
                                   generatorC: (S) => List[C],
                                   generatorD: (S) => List[D],
                                   generatorE: (S) => List[E],
                                   generatorF: (S) => List[F],
                                   forAllList: List[(S) => Boolean],
                                   thereExists: (S) => Option[(A, B, C, D, E, F)]) {

        def this(generatorA: (S) => List[A],
                 generatorB: (S) => List[B],
                 generatorC: (S) => List[C],
                 generatorD: (S) => List[D],
                 generatorE: (S) => List[E],
                 generatorF: (S) => List[F],
                 forAllList: List[(S) => Boolean] = List()) = {

            this(generatorA, generatorB, generatorC, generatorD, generatorE, generatorF, forAllList, (state) => Some(
                Random.shuffle(generatorA(state)).head,
                Random.shuffle(generatorB(state)).head,
                Random.shuffle(generatorC(state)).head,
                Random.shuffle(generatorD(state)).head,
                Random.shuffle(generatorE(state)).head,
                Random.shuffle(generatorF(state)).head
            ))
        }

        def forAll(predicate: (A, B, C, D, E, F) => Boolean): Given6[A, B, C, D, E, F, S] = {
            new Given6(generatorA, generatorB, generatorC, generatorD, generatorE, generatorF, Predicate.forAll(generatorA, generatorB, generatorC, generatorD, generatorE, generatorF, predicate) :: forAllList, thereExists)
        }

        def thereExists(predicate: (A, B, C, D, E, F) => Boolean): Given6[A, B, C, D, E, F, S] = {
            new Given6(generatorA, generatorB, generatorC, generatorD, generatorE, generatorF, forAllList, Predicate.thereExists(generatorA, generatorB, generatorC, generatorD, generatorE, generatorF, predicate))
        }

        def apply(state: S): Option[(A, B, C, D, E, F)] = {
            if (forAllList.forall(forAll => forAll(state))) thereExists(state) else None
        }
    }

    def given[A, B, C, D, E, F, S](generatorA: (S) => List[A],
                                generatorB: (S) => List[B],
                                generatorC: (S) => List[C],
                                generatorD: (S) => List[D],
                                generatorE: (S) => List[E],
                                generatorF: (S) => List[F]) = new Given6[A, B, C, D, E, F, S](generatorA, generatorB, generatorC, generatorD, generatorE, generatorF)

    def given[A, B, C, D, E, S](generatorA: (S) => List[A],
                                generatorB: (S) => List[B],
                                generatorC: (S) => List[C],
                                generatorD: (S) => List[D],
                                generatorE: (S) => List[E]) = new Given5[A, B, C, D, E, S](generatorA, generatorB, generatorC, generatorD, generatorE)

    def given[A, B, C, D, S](generatorA: (S) => List[A],
                             generatorB: (S) => List[B],
                             generatorC: (S) => List[C],
                             generatorD: (S) => List[D]) = new Given4[A, B, C, D, S](generatorA, generatorB, generatorC, generatorD)


    def given[A, B, C, S](generatorA: (S) => List[A],
                          generatorB: (S) => List[B],
                          generatorC: (S) => List[C]) = new Given3[A, B, C, S](generatorA, generatorB, generatorC)


    def given[A, B, S](generatorA: (S) => List[A],
                       generatorB: (S) => List[B]) = new Given2[A, B, S](generatorA, generatorB)

    def given[A, S](generatorA: (S) => List[A]) = new Given1[A, S](generatorA)
}