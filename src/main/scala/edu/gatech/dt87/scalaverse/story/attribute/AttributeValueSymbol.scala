package edu.gatech.dt87.scalaverse.story.attribute

/**
 * A value that may be any symbol.
 *
 * @param symbol a symbol.
 */
case class AttributeValueSymbol(symbol: String) extends AttributeValue {
    /**
     * Given a symbol, return a value that is the given symbol.
     *
     * @param other a symbol
     * @return a value that is the given symbol
     */
    def update(other: String): AttributeValueSymbol = {
        if (symbol == other) {
            this
        }
        else {
            AttributeValueSymbol(other)
        }
    }
}
