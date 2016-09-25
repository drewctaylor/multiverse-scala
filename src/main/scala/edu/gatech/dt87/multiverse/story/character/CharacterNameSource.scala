//package edu.gatech.dt87.scalaverse.story.character
//
//import edu.gatech.dt87.scalaverse.random._
//import edu.gatech.dt87.scalaverse.story.character.StringExtension._
//
//object CharacterNameSource {
//    val firstIteratorFor = prepareFirst("first.csv")
//    val lastIterator = prepareLast("last.csv")
//
//    def prepareFirst(file: String): Map[(CharacterGender, Int), Iterator[String]] = {
//        io.Source.fromInputStream(getClass.getResourceAsStream(file)).getLines()
//            .map(line => line.split(","))
//            .map(array => (array(0).toGender, array(1).toInt, array(2)))
//            .foldLeft(Map[(CharacterGender, Int), Seq[String]]())((map, record) => map.updated((record._1, record._2), map.getOrElse((record._1, record._2), Seq[String]()) :+ record._3: Seq[String]))
//            .map(entry => entry)
//            .foldLeft(Map[(CharacterGender, Int), Iterator[String]]())((map, entry) => map + (entry._1 -> entry._2.toIterator))
//    }
//
//    def prepareLast(file: String): Iterator[String] = {
//        Random.shuffle(io.Source.fromInputStream(getClass.getResourceAsStream(file)).getLines()).toIterator
//    }
//
//    def nextFirst(gender: CharacterGender, year: Int): String = {
//        val firstIterator = firstIteratorFor.getOrElse((gender, year / 10 * 10), throw new RuntimeException("The system has no iterator for the given year."))
//
//        if (firstIterator.hasNext) {
//            firstIterator.next()
//        } else {
//            throw new RuntimeException("The system has no more first names for the given year.")
//        }
//    }
//
//    def nextLast(): String = {
//        if (lastIterator.hasNext) {
//            lastIterator.next()
//        } else {
//            throw new RuntimeException("The system has no more names.")
//        }
//    }
//}
