package edu.gatech.dt87.scalaverse.story.attribute

/**
 * A value that may be any symbol from the given symbol set.
 *
 * @param validSet the given symbol set
 * @param symbol a value from the given symbol set
 */
case class AttributeValueSymbolFromSet(validSet: Set[String], symbol: String) extends AttributeValue{
    def update(symbolUpdate: String): AttributeValueSymbolFromSet = {
        if (validSet.contains(symbolUpdate)) {
            if (symbol == symbolUpdate) {
                this
            } else {
                AttributeValueSymbolFromSet(validSet, symbolUpdate)
            }
        } else {
            throw new IllegalArgumentException(s"The symbol $symbolUpdate is not a member of $validSet")
        }
    }
}
