package edu.gatech.dt87.scalaverse.story.character

/*
 * Character names based on year and gender.
 * Random character.
 */

import edu.gatech.dt87.scalaverse.random.Random
import edu.gatech.dt87.scalaverse.story.character.StringOperation._
import monocle.Lenser

case class Character(
                        id: Int,
                        first: String,
                        last: String,
                        gender: Gender,
                        orientation: Set[Gender],
                        age: Int,
                        spouse: Option[Int],
                        life: Life) {

    def sub(): String = {
        gender match {
            case MALE => "he"
            case FEMALE => "she"
        }
    }

    def Sub(): String = {
        sub.capitalize
    }

    def obj(): String = {
        gender match {
            case MALE => "him"
            case FEMALE => "her"
        }
    }

    def Obj(): String = {
        obj.capitalize
    }

    def pos(): String = {
        gender match {
            case MALE => "his"
            case FEMALE => "hers"
        }
    }

    def Pos(): String = {
        pos.capitalize
    }

    def det(): String = {
        gender match {
            case MALE => "his"
            case FEMALE => "her"
        }
    }

    def Det(): String = {
        det.capitalize
    }
}

object Character {
    val index = Iterator.from(0)

    def apply(
                 first: String,
                 last: String,
                 gender: Gender,
                 orientation: Set[Gender],
                 age: Int): Character = {
        new Character(index.next(), first, last, gender, orientation, age, None, ALIVE)
    }

    def random(
                 firstIn: Option[String] = None,
                 lastIn: Option[String] = None,
                 genderIn: Option[Gender] = None,
                 orientationIn: Option[Set[Gender]] = None,
                 ageIn: Option[Int] = None): Character = {

        val gender = genderIn.getOrElse(if (Random.nextBoolean()) MALE else FEMALE)
        val age = ageIn.getOrElse(Random.nextInt(125))
        val first = firstIn.getOrElse(CharacterNameSource.nextFirst(gender, 2009 - age))
        val last = lastIn.getOrElse(CharacterNameSource.nextLast())
        val orientation: Set[Gender] = orientationIn.getOrElse(if(Random.nextInt(10) == 1) Set(gender) else if(gender == MALE) Set(FEMALE) else Set(MALE))

        new Character(index.next(), first, last, gender, orientation, age, None, ALIVE)
    }

    val lenser = new Lenser[Character]
    val first = lenser(_.first)
    val last = lenser(_.last)
    val gender = lenser(_.gender)
    val orientation = lenser(_.orientation)
    val age = lenser(_.age)
    val spouse = lenser(_.spouse)
    val life = lenser(_.life)

}

object CharacterNameSource {
    val firstIteratorFor = prepareFirst("first.csv")
    val lastIterator = prepareLast("last.csv")

    def prepareFirst(file: String): Map[(Gender, Int), Iterator[String]] = {
        io.Source.fromInputStream(getClass.getResourceAsStream(file)).getLines()
            .map(line => line.split(","))
            .map(array => (array(0).toGender, array(1).toInt, array(2)))
            .foldLeft(Map[(Gender, Int), Seq[String]]())((map, record) => map.updated((record._1, record._2), map.getOrElse((record._1, record._2), Seq[String]()) :+ record._3: Seq[String]))
            .map(entry => entry)
            .foldLeft(Map[(Gender, Int), Iterator[String]]())((map, entry) => map + (entry._1 -> entry._2.toIterator))
    }

    def prepareLast(file: String): Iterator[String] = {
        Random.shuffle(io.Source.fromInputStream(getClass.getResourceAsStream(file)).getLines()).toIterator
    }

    def nextFirst(gender: Gender, year: Int): String = {
        val firstIterator = firstIteratorFor.getOrElse((gender, year / 10 * 10), throw new RuntimeException("The system has no iterator for the given year."))

        if (firstIterator.hasNext) {
            firstIterator.next()
        } else {
            throw new RuntimeException("The system has no more first names for the given year.")
        }
    }

    def nextLast(): String = {
        if (lastIterator.hasNext) {
            lastIterator.next()
        } else {
            throw new RuntimeException("The system has no more names.")
        }
    }
}