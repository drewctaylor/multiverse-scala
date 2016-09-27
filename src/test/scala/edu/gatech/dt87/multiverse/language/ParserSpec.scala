package edu.gatech.dt87.multiverse.language

import edu.gatech.dt87.multiverse.language.parser.Parser
import org.scalatest.FlatSpec

class ParserSpec extends FlatSpec {
    "The Parser" should "parse the Boolean.multiverse file" in {
        val source = io.Source.fromInputStream(getClass.getResourceAsStream("Boolean.multiverse")).mkString

        assert(Parser.phrase(Parser.declarationStory)(new Parser.lexical.Scanner(source)) match {
            case Parser.Success(result, next) => true
            case Parser.NoSuccess(message, next) =>
                println(next.pos, message)
                false
        })
    }

    it should "parse the Number.multiverse file" in {
        val source = io.Source.fromInputStream(getClass.getResourceAsStream("Number.multiverse")).mkString

        assert(Parser.phrase(Parser.declarationStory)(new Parser.lexical.Scanner(source)) match {
            case Parser.Success(result, next) => true
            case Parser.NoSuccess(message, next) =>
                println(next.pos, message)
                false
        })
    }

    it should "parse the Set.multiverse file" in {
        val source = io.Source.fromInputStream(getClass.getResourceAsStream("Set.multiverse")).mkString

        assert(Parser.phrase(Parser.declarationStory)(new Parser.lexical.Scanner(source)) match {
            case Parser.Success(result, next) => true
            case Parser.NoSuccess(message, next) =>
                println(next.pos, message)
                false
        })
    }

    it should "parse the MurderMystery.multiverse file" in {
        val source = io.Source.fromInputStream(getClass.getResourceAsStream("MurderMystery.multiverse")).mkString

        assert(Parser.phrase(Parser.declarationStory)(new Parser.lexical.Scanner(source)) match {
            case Parser.Success(result, next) => true
            case Parser.NoSuccess(message, next) =>
                println(next.pos, message)
                false
        })
    }

    it should "parse the MelrosePlace.multiverse file" in {
        val source = io.Source.fromInputStream(getClass.getResourceAsStream("MelrosePlace.multiverse")).mkString

        assert(Parser.phrase(Parser.declarationStory)(new Parser.lexical.Scanner(source)) match {
            case Parser.Success(result, next) => true
            case Parser.NoSuccess(message, next) =>
                println(next.pos, message)
                false
        })
    }
}