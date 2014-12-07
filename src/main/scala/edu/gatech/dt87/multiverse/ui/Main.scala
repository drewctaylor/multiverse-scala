package edu.gatech.dt87.multiverse.ui

import edu.gatech.dt87.multiverse.random.Random
import edu.gatech.dt87.multiverse.story.dsl.compiler.Compiler

object Main {
    def main(argument: Array[String]): Unit = {
        if(argument.length == 1) {
            val source = io.Source.fromFile(argument(0)).mkString
            val parsed = Compiler.compile(source)

            parsed.map((tuple) => new JFrameWithWebView("ui.html", new Server(tuple._1, tuple._2)))
        } else {
            println("Please provide a file.")
        }
    }
}