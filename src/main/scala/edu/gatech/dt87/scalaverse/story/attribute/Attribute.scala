package edu.gatech.dt87.scalaverse.story.attribute

sealed trait Attribute {
    def name: Symbol
}

sealed trait AttributeValidatable extends Attribute {
    def validSequence: Seq[Symbol]
}

sealed trait AttributeOne extends Attribute

sealed trait AttributeSet extends Attribute

sealed trait AttributeOrdered extends Attribute

sealed trait AttributeUnordered extends Attribute

case class AttributeOneOrdered(name: Symbol, validSequence: Seq[Symbol])
    extends Attribute
    with AttributeOne
    with AttributeOrdered
    with AttributeValidatable

case class AttributeOneSymbol(name: Symbol)
    extends Attribute
    with AttributeOne

case class AttributeOneUnordered(name: Symbol, validSequence: Seq[Symbol])
    extends Attribute
    with AttributeOne
    with AttributeUnordered
    with AttributeValidatable

case class AttributeSubsetOrdered(name: Symbol, validSequence: Seq[Symbol])
    extends Attribute
    with AttributeSet
    with AttributeOrdered
    with AttributeValidatable

case class AttributeSubsetSymbol(name: Symbol)
    extends Attribute
    with AttributeSet

case class AttributeSubsetUnordered(name: Symbol, validSequence: Seq[Symbol])
    extends Attribute
    with AttributeOne
    with AttributeUnordered
    with AttributeValidatable
