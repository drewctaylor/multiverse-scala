package edu.gatech.dt87.scalaverse.ui

import javafx.application.Platform
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.concurrent.Worker
import javafx.embed.swing.JFXPanel
import javafx.scene.Scene
import javafx.scene.web.WebView
import javax.swing.{UIManager, JFrame}

import netscape.javascript.JSObject

object Main {
    def main(argument: Array[String]): Unit = {
        new JFrameWithWebView("ui.html", new Server())
    }
}