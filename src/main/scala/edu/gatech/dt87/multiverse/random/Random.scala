package edu.gatech.dt87.multiverse.random

import scala.collection.generic.CanBuildFrom

object Random {
    val randomSeed = 0
    val random = new scala.util.Random(randomSeed)

    def nextInt(n: Int): Int = {
        random.nextInt(n)
    }

    def nextBoolean() : Boolean = {
        random.nextBoolean()
    }

    def shuffle[T, CC[X] <: TraversableOnce[X]](xs: CC[T])(implicit bf: CanBuildFrom[CC[T], T, CC[T]]): CC[T] = {
        random.shuffle(xs)(bf)
    }
}
