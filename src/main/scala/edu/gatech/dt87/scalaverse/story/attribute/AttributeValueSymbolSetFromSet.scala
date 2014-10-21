package edu.gatech.dt87.scalaverse.story.attribute

/**
 * A value that may be any subset of a given symbol set.
 *
 * @param validSet a given symbol set
 * @param symbolSet a subset of the given symbol set
 */
case class AttributeValueSymbolSetFromSet(validSet: Set[String], symbolSet: Set[String]) {
    def insert(symbol: String): AttributeValueSymbolSetFromSet = {
        if (validSet.contains(symbol)) {
            if (symbolSet.contains(symbol)) {
                this
            } else {
                AttributeValueSymbolSetFromSet(validSet, symbolSet + symbol)
            }
        } else {
            throw new IllegalArgumentException(s"The symbol $symbol is not a member of $validSet")
        }
    }

    def remove(symbol: String): AttributeValueSymbolSetFromSet = {
        if (validSet.contains(symbol)) {
            if (symbolSet.contains(symbol)) {
                AttributeValueSymbolSetFromSet(validSet, symbolSet - symbol)
            } else {
                this
            }
        } else {
            throw new IllegalArgumentException(s"The symbol $symbol is not a member of $validSet")
        }
    }

    def contains(symbol: String): Boolean = {
        if (validSet.contains(symbol)) {
            symbolSet.contains(symbol)
        } else {
            throw new IllegalArgumentException(s"The symbol $symbol is not a member of $validSet")
        }
    }
}
    