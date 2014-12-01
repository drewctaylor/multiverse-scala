package edu.gatech.dt87.multiverse.ui

import edu.gatech.dt87.multiverse.random.Random
import edu.gatech.dt87.multiverse.story.dsl.compiler.Compiler

object Main {
    def main(argument: Array[String]): Unit = {
        val text = io.Source.fromFile(argument(0)).mkString

        val parsed = Compiler.compile(text)

        Random.random = parsed.map(_._1.seed).flatten.map(new scala.util.Random(_)) getOrElse new scala.util.Random()
        parsed.map((tuple) => new JFrameWithWebView("ui.html", new Server(tuple._1, tuple._2)))
    }
}