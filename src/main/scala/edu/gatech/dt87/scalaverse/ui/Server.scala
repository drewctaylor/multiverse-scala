package edu.gatech.dt87.scalaverse.ui

class Server {
    def select(stateId: String, goalId: String): String = {
        "{ stateId: \"" + stateId + "\", goalId: \"" + goalId + "\" }"
    }
}
