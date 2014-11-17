package edu.gatech.dt87.multiverse.ui

import javafx.application.Platform
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.concurrent.Worker
import javafx.embed.swing.JFXPanel
import javafx.scene.Scene
import javafx.scene.web.WebView
import javax.swing.JFrame

import netscape.javascript.JSObject

/**
 * A JFrame that contains a WebView.
 *
 * The WebView provides a view onto the given url. To the global object with the name globalObjectName, the system
 * attaches an object with the name serverObjectName; this object serves as the proxy for the given server.
 *
 * @param url the WebView url
 * @param globalObjectName the name of the global object
 * @param serverObjectName the name of the server object
 * @param server the server object
 * @tparam S the server type
 */

class JFrameWithWebView[S](url: String, globalObjectName: String, serverObjectName: String, server: S) extends JFrame {
    /**
     * An alternate constructor.
     *
     * If used, the name of the global object is "window" and the name of the server object is "server".
     *
     * @param url the WebView url
     * @param server the server object
     */
    def this(url: String, server: S) {
        this(url, "window", "server", server)
    }

    val jfxPanel = new JFXPanel

    Platform.runLater(new Runnable() {
        def run() {
            val webView = new WebView
            val webEngine = webView.getEngine
            val scene = new Scene(webView)
            webEngine.load(getClass.getResource(url).toExternalForm)
            jfxPanel.setScene(scene)

            val changeListener = new ChangeListener[Worker.State] {
                def changed(ov: ObservableValue[_ <: Worker.State], old: Worker.State, newState: Worker.State): Unit = {
                    val jsObject = webEngine.executeScript(globalObjectName).asInstanceOf[JSObject]
                    jsObject.setMember(serverObjectName, server)
                }
            }

            webEngine.getLoadWorker.stateProperty.addListener(changeListener)
        }
    })

    getContentPane.add(jfxPanel)
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    pack()
    setVisible(true)
}
