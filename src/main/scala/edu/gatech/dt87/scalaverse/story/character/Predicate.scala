package edu.gatech.dt87.scalaverse.story.character

import edu.gatech.dt87.scalaverse.story.State;

object Predicate {

     def distinct = (a: Character, b: Character) => a.id != b.id

     def alive = (a: Character) => a.life == ALIVE

     def dead = (a: Character) => a.life == DEAD

     def attractive = (a: Character, b: Character) => distinct(a, b) && a.orientation.contains(b.gender)

     def compatible = (a: Character, b: Character) => attractive(a, b) && attractive(b, a)

     def incompatible = (a: Character, b: Character) => !attractive(a, b) || !attractive(b, a)

     def single = (a: Character) => a.spouse.isEmpty

     def married = (a: Character) => a.spouse.isDefined

     def areMarried = (a: Character, b: Character) => {
         a.spouse.isDefined && b.spouse.isDefined && a.spouse.get == b.id && b.spouse.get == a.id
     }

     def marriageable = (a: Character, b: Character) => compatible(a, b) && single(a) && single(b)

     def character = (s: State) => s.characterSet

     def characterIs = (c: (Character) => Boolean) => (s: State) => s.characterSet.filter(c)
 }
