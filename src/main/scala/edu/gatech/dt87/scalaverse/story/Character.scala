package edu.gatech.dt87.scalaverse.story

case class Character(first: String, last: String, gender: Gender, orientation: Set[Gender], age: Integer, var spouse : Option[Character] = None, life : Life = ALIVE);

object Predicate {
    def distinct = (a : Character, b : Character) => a != b

    def alive = (a : Character) => a.life == ALIVE

    def dead = (a : Character) => a.life == DEAD

    def attractive = (a : Character, b: Character) => distinct(a, b) && alive(b) && a.orientation.contains(b.gender)

    def compatible = (a : Character, b : Character) => attractive(a, b) && attractive(b, a)

    def incompatible = (a : Character, b : Character) => !attractive(a, b) || !attractive(b, a)

    def single = (a : Character) => a.spouse.isEmpty

    def married = (a : Character) => a.spouse.isDefined

    def marriageable = (a : Character, b : Character) => compatible(a, b) && single(a) && single(b)

    def character = (s : Story) => s.cast

    def characterIs = (c : (Character) => Boolean) => (s : Story) => s.cast.filter(c)
}