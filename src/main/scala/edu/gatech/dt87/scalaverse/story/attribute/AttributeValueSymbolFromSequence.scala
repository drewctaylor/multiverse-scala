package edu.gatech.dt87.scalaverse.story.attribute

/**
 * A value that may be any symbol from a given symbol sequence.
 *
 * @param validSequence a given symbol sequence
 * @param symbol a symbol from the given symbol sequence
 */
case class AttributeValueSymbolFromSequence(validSequence: Seq[String], symbol: String) extends AttributeValue{
    def update(symbolUpdate: String): AttributeValueSymbolFromSequence = {
        if (validSequence.contains(symbolUpdate)) {
            if (symbol == symbolUpdate) {
                this
            } else {
                AttributeValueSymbolFromSequence(validSequence, symbolUpdate)
            }
        } else {
            throw new IllegalArgumentException(s"The symbol $symbolUpdate is not a member of $validSequence")
        }
    }

    def increase(): AttributeValueSymbolFromSequence = {
        if (validSequence.indexOf(symbol) == validSequence.size - 1) {
            this
        } else {
            AttributeValueSymbolFromSequence(validSequence, validSequence(validSequence.indexOf(symbol) + 1))
        }
    }

    def decrease(): AttributeValueSymbolFromSequence = {
        if (validSequence.indexOf(symbol) == 0) {
            this
        } else {
            AttributeValueSymbolFromSequence(validSequence, validSequence(validSequence.indexOf(symbol) - 1))
        }
    }

    def increaseToMaximum(): AttributeValueSymbolFromSequence = {
        if (validSequence.indexOf(symbol) == validSequence.size - 1) {
            this
        } else {
            AttributeValueSymbolFromSequence(validSequence, validSequence(validSequence.indexOf(validSequence.size - 1)))
        }
    }

    def decreaseToMinimum(): AttributeValueSymbolFromSequence = {
        if (validSequence.indexOf(symbol) == 0) {
            this
        } else {
            AttributeValueSymbolFromSequence(validSequence, validSequence(validSequence.indexOf(0)))
        }
    }

    def isMaximum(): Boolean = validSequence.indexOf(symbol) == validSequence.size - 1

    def isMinimum(): Boolean = validSequence.indexOf(symbol) == 0
}
