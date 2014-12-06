package edu.gatech.dt87.multiverse.story.state

sealed trait AttributeKind

sealed trait AttributeKindBoolean extends AttributeKind

sealed trait AttributeKindNumber extends AttributeKind

sealed trait AttributeKindSymbol extends AttributeKind

sealed trait AttributeKindEntity extends AttributeKind

sealed trait AttributeKindMany

sealed trait AttributeKindOne

sealed trait AttributeKindOrdered

sealed trait AttributeKindUnordered

case object AttributeKindBooleanMany
    extends AttributeKindBoolean
    with AttributeKindMany
    with AttributeKindUnordered

case object AttributeKindBooleanOne
    extends AttributeKindBoolean
    with AttributeKindOne
    with AttributeKindUnordered

case class AttributeKindEntityMany(kind: EntityKind)
    extends AttributeKindEntity
    with AttributeKindMany
    with AttributeKindUnordered

case class AttributeKindEntityOne(kind: EntityKind)
    extends AttributeKindEntity
    with AttributeKindOne
    with AttributeKindUnordered

case object AttributeKindNumberMany
    extends AttributeKindNumber
    with AttributeKindMany
    with AttributeKindOrdered

case object AttributeKindNumberOne
    extends AttributeKindNumber
    with AttributeKindOne
    with AttributeKindOrdered

case object AttributeKindSymbolMany
    extends AttributeKindSymbol
    with AttributeKindMany
    with AttributeKindUnordered

case object AttributeKindSymbolOne
    extends AttributeKindSymbol
    with AttributeKindOne
    with AttributeKindUnordered

case class AttributeKindSymbolOrderedMany(symbolSequence: Seq[Symbol])
    extends AttributeKindSymbol
    with AttributeKindMany
    with AttributeKindOrdered

case class AttributeKindSymbolOrderedOne(symbolSequence: Seq[Symbol])
    extends AttributeKindSymbol
    with AttributeKindOne
    with AttributeKindOrdered

case class AttributeKindSymbolUnorderedMany(symbolSequence: Set[Symbol])
    extends AttributeKindSymbol
    with AttributeKindMany
    with AttributeKindUnordered

case class AttributeKindSymbolUnorderedOne(symbolSet: Set[Symbol])
    extends AttributeKindSymbol
    with AttributeKindOne
    with AttributeKindUnordered

case class EntityKind(attributeMap: Map[Symbol, AttributeKind])

case class StateKind(entityMap: Map[Symbol, EntityKind])
