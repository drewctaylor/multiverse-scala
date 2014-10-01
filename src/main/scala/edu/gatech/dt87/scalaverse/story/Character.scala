package edu.gatech.dt87.scalaverse.story

import monocle.Lenser

case class Character(
                        first: String,
                        last: String,
                        gender: Gender,
                        orientation: Set[Gender],
                        age: Integer,
                        spouse: Option[Character] = None,
                        life: Life = ALIVE,
                        ex: Option[List[Character]] = None)

object Character {
    val lenser  =new Lenser[Character];
    val first = lenser(_.first)
    val last = lenser(_.last)
    val gender = lenser(_.gender)
    val orientation = lenser(_.orientation)
    val age = lenser(_.age)
    val spouse = lenser(_.spouse)
    val life = lenser(_.life)
    val ex = lenser(_.ex)
}


object Predicate {

    def distinct = (a: Character, b: Character) => a != b

    def alive = (a: Character) => a.life == ALIVE

    def dead = (a: Character) => a.life == DEAD

    def attractive = (a: Character, b: Character) => distinct(a, b) && alive(b) && a.orientation.contains(b.gender)

    def compatible = (a: Character, b: Character) => attractive(a, b) && attractive(b, a)

    def incompatible = (a: Character, b: Character) => !attractive(a, b) || !attractive(b, a)

    def single = (a: Character) => a.spouse.isEmpty

    def married = (a: Character) => a.spouse.isDefined

    def areMarried = (a: Character, b: Character) => {
        a.spouse.isDefined && b.spouse.isDefined && a.spouse.get.first == b.first && b.spouse.get.first == a.first
    }

    def marriageable = (a: Character, b: Character) => compatible(a, b) && single(a) && single(b)
    def character = (s: State) => s.characterSet
    def characterIs = (c: (Character) => Boolean) => (s: State) => s.characterSet.filter(c)
}