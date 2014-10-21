package edu.gatech.dt87.scalaverse.story.attribute

/**
 * A value that may be any set of symbols.
 *
 * @param symbolSet a set of symbols.
 */
case class AttributeValueSymbolSet(symbolSet: Set[String]) extends AttributeValue{
    /**
     * Given a symbol, return a value that is the union of the current set and the given symbol.
     *
     * @param symbol a symbol
     * @return a value that is the union of the current set and the given symbol
     */
    def insert(symbol: String): AttributeValueSymbolSet = {
        if (symbolSet.contains(symbol)) {
            this
        }
        else {
            AttributeValueSymbolSet(symbolSet + symbol)
        }
    }

    def remove(symbol: String): AttributeValueSymbolSet = {
        if (symbolSet.contains(symbol)) {
            AttributeValueSymbolSet(symbolSet - symbol)
        } else {
            this
        }
    }

    def contains(symbol: String): Boolean = symbolSet.contains(symbol)
}
