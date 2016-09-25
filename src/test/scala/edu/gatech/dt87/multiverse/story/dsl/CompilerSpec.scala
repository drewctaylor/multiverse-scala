package edu.gatech.dt87.multiverse.story.dsl

import edu.gatech.dt87.multiverse.story.dsl.compiler.Compiler
import org.scalatest.FlatSpec

class CompilerSpec extends FlatSpec {
    "The Compiler" should "compile the Boolean.multiverse file" in {
        val source = io.Source.fromInputStream(getClass.getResourceAsStream("Boolean.multiverse")).mkString

        assert(Compiler.compile(source).isSuccess)
    }

    it should "compile the Number.multiverse file" in {
        val source = io.Source.fromInputStream(getClass.getResourceAsStream("Number.multiverse")).mkString

        assert(Compiler.compile(source).isSuccess)
    }

    it should "compile the Set.multiverse file" in {
        val source = io.Source.fromInputStream(getClass.getResourceAsStream("Set.multiverse")).mkString

        assert(Compiler.compile(source).isSuccess)
    }

    it should "compile the MurderMystery.multiverse file" in {
        val source = io.Source.fromInputStream(getClass.getResourceAsStream("MurderMystery.multiverse")).mkString

        assert(Compiler.compile(source).isSuccess)
    }

    it should "compile the MelrosePlace.multiverse file" in {
        val source = io.Source.fromInputStream(getClass.getResourceAsStream("MelrosePlace.multiverse")).mkString

        assert(Compiler.compile(source).isSuccess)
    }
}