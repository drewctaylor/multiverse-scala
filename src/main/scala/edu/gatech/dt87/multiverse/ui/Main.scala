package edu.gatech.dt87.multiverse.ui

import edu.gatech.dt87.multiverse.story.dsl.Parser

object Main {
    def main(argument: Array[String]): Unit = {
        val text = io.Source.fromFile(argument(0)).mkString

        val parsed = Parser.parse(text)

        parsed.map((tuple) => new JFrameWithWebView("ui.html", new Server(tuple._1, tuple._2)))
    }
}