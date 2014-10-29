package edu.gatech.dt87.scalaverse.story.dsl

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

class Parser extends RegexParsers with PackratParsers {

    lazy val declarationState = "state" ~ blockState ^^ {
        case s ~ b => DeclarationState(b)
    }

    lazy val declarationAttribute : PackratParser[DeclarationAttribute] = declarationOneSymbol | declarationOneUnordered | declarationOneOrdered | declarationSubsetSymbol | declarationSubsetUnordered | declarationSubsetOrdered

    lazy val declarationOneSymbol :PackratParser[DeclarationAttribute] = "attribute" ~ identifier ~ "symbol" ^^ {
        case a ~ i ~ s1 => DeclarationAttributeOneSymbol(i)
    }

    lazy val declarationOneUnordered  :PackratParser[DeclarationAttribute]= "attribute" ~ identifier ~ "one" ~ "of" ~ "unordered" ~ "set" ~ blockLiteral ^^ {
        case a ~ i ~ s1 ~ s2 ~ s3 ~ s4 ~ b => DeclarationAttributeOneUnordered(i, b)
    }

    lazy val declarationOneOrdered :PackratParser[DeclarationAttribute] = "attribute" ~ identifier ~ "one" ~ "of" ~ "ordered" ~ "set" ~ blockLiteral ^^ {
        case a ~ i ~ s1 ~ s2 ~ s3 ~ s4 ~ b => DeclarationAttributeOneOrdered(i, b)
    }

    lazy val declarationSubsetSymbol :PackratParser[DeclarationAttribute] = "attribute" ~ identifier ~ "set" ~ "of" ~ "symbols" ^^ {
        case a ~ i ~ s1 ~ s2 ~ s3 => DeclarationAttributeSubsetSymbol(i)
    }

    lazy val declarationSubsetUnordered :PackratParser[DeclarationAttribute] = "attribute" ~ identifier ~ "subset" ~ "of" ~ "unordered" ~ "set" ~ blockLiteral ^^ {
        case a ~ i ~ s1 ~ s2 ~ s3 ~ s4 ~ b => DeclarationAttributeSubsetUnordered(i, b)
    }

    lazy val declarationSubsetOrdered :PackratParser[DeclarationAttribute] = "attribute" ~ identifier ~ "subset" ~ "of" ~ "ordered" ~ "set" ~ blockLiteral ^^ {
        case a ~ i ~ s1 ~ s2 ~ s3 ~ s4 ~ b => DeclarationAttributeSubsetOrdered(i, b)
    }

    lazy val declarationRelationship: PackratParser[DeclarationRelationship] = "relationship" ~ expressionRelationship ~ blockAssignment ^^ {
        case c ~ i ~ b => DeclarationRelationship(i, b)
    }

    lazy val declarationSetting: PackratParser[DeclarationSetting] = "setting" ~ expressionEntity ~ blockAssignment ^^ {
        case c ~ i ~ b => DeclarationSetting(i, b)
    }

    lazy val declarationCharacter: PackratParser[DeclarationCharacter] = "character" ~ expressionEntity ~ blockAssignment ^^ {
        case c ~ i ~ b => DeclarationCharacter(i, b)
    }

    lazy val blockState: PackratParser[BlockState] = "{" ~ rep(declarationAttribute) ~ rep(declarationCharacter) ~ rep(declarationRelationship) ~ "}" ^^ {
        case bl ~ a ~ c ~ r ~ br => BlockState(a, c, r)
    }

    lazy val blockLiteral: PackratParser[BlockLiteral] = "{" ~ rep(expressionLiteral) ~ "}" ^^ {
        case bl ~ el ~ br => BlockLiteral(el)
    }

    lazy val blockAssignment: PackratParser[BlockAssignment] = "{" ~ rep(expressionAssignment) ~ "}" ^^ {
        case bl ~ el ~ br => BlockAssignment(el)
    }

    lazy val expression: PackratParser[Expression] = expressionAssignment

    lazy val expressionAssignment: PackratParser[Expression] = expressionConditionalOr ~ rep((":=" | "+=" | "-=") ~ expressionConditionalOr) ^^ {
        case el ~ Nil => el
        case el ~ ((op ~ er) :: rest) => rest.map({ case op1 ~ er1 => (op1, er1)}).foldLeft(ExpressionBinary(el, Symbol(op), er))((expression, pair) => pair match {
            case (op1, er1) => ExpressionBinary(expression, Symbol(op1), er1)
        })
    }

    lazy val expressionConditionalOr: PackratParser[Expression] = expressionConditionalAnd ~ rep("||" ~ expressionConditionalAnd) ^^ {
        case el ~ Nil => el
        case el ~ ((op ~ er) :: rest) => rest.map({ case op1 ~ er1 => (op1, er1)}).foldLeft(ExpressionBinary(el, Symbol(op), er))((expression, pair) => pair match {
            case (op1, er1) => ExpressionBinary(expression, Symbol(op1), er1)
        })
    }

    lazy val expressionConditionalAnd: PackratParser[Expression] = expressionEquality ~ rep("&&" ~ expressionEquality) ^^ {
        case el ~ Nil => el
        case el ~ ((op ~ er) :: rest) => rest.map({ case op1 ~ er1 => (op1, er1)}).foldLeft(ExpressionBinary(el, Symbol(op), er))((expression, pair) => pair match {
            case (op1, er1) => ExpressionBinary(expression, Symbol(op1), er1)
        })
    }

    lazy val expressionEquality: PackratParser[Expression] = expressionRelational ~ rep(("==" | "!=") ~ expressionRelational) ^^ {
        case el ~ Nil => el
        case el ~ ((op ~ er) :: rest) => rest.map({ case op1 ~ er1 => (op1, er1)}).foldLeft(ExpressionBinary(el, Symbol(op), er))((expression, pair) => pair match {
            case (op1, er1) => ExpressionBinary(expression, Symbol(op1), er1)
        })
    }

    lazy val expressionRelational: PackratParser[Expression] = expressionAdditive ~ rep(("<" | "<=" | ">=" | ">") ~ expressionAdditive) ^^ {
        case el ~ Nil => el
        case el ~ ((op ~ er) :: rest) => rest.map({ case op1 ~ er1 => (op1, er1)}).foldLeft(ExpressionBinary(el, Symbol(op), er))((expression, pair) => pair match {
            case (op1, er1) => ExpressionBinary(expression, Symbol(op1), er1)
        })
    }

    lazy val expressionAdditive: PackratParser[Expression] = expressionMultiplicative ~ rep(("+" | "-") ~ expressionMultiplicative) ^^ {
        case el ~ Nil => el
        case el ~ ((op ~ er) :: rest) => rest.map({ case op1 ~ er1 => (op1, er1)}).foldLeft(ExpressionBinary(el, Symbol(op), er))((expression, pair) => pair match {
            case (op1, er1) => ExpressionBinary(expression, Symbol(op1), er1)
        })
    }

    lazy val expressionMultiplicative: PackratParser[Expression] = expressionUnary ~ rep(("*" | "/") ~ expressionUnary) ^^ {
        case el ~ Nil => el
        case el ~ ((op ~ er) :: rest) => rest.map({ case op1 ~ er1 => (op1, er1)}).foldLeft(ExpressionBinary(el, Symbol(op), er))((expression, pair) => pair match {
            case (op1, er1) => ExpressionBinary(expression, Symbol(op1), er1)
        })
    }

    lazy val expressionUnary: PackratParser[Expression] = opt("!") ~ expressionPostfix ^^ {
        case None ~ e => e
        case Some(s) ~ e => ExpressionUnary(e, Symbol(s))
    }

    lazy val expressionPostfix: PackratParser[Expression] = expressionAtom ~ opt("++" | "+++" | "--" | "---") ^^ {
        case e ~ None => e
        case e ~ Some(s) => ExpressionUnary(e, Symbol(s))
    }

    lazy val expressionAtom: PackratParser[Expression] = expressionLiteral | expressionAttribute | expressionEnclosed

    lazy val expressionEnclosed: PackratParser[Expression] = "(" ~ expression ~ ")" ^^ {
        case pl ~ e ~ pr => e
    }

    lazy val expressionAttribute: PackratParser[ExpressionAttribute] = opt((expressionEntity | expressionRelationshipEnclosed) ~ ".") ~ identifier ^^ {
        case Some((ao: ExpressionAttributeOwner) ~ d) ~ a => ExpressionAttribute(Some(ao), a)
        case None ~ a => ExpressionAttribute(None, a)
    }

    lazy val expressionEntity: PackratParser[ExpressionEntity] = identifier ^^ {
        case e => ExpressionEntity(e)
    }

    lazy val expressionRelationshipEnclosed: PackratParser[ExpressionAttributeOwner] = "(" ~ expressionRelationship ~ ")" ^^ {
        case pl ~ r ~ pr => r
    }

    lazy val expressionRelationship: PackratParser[ExpressionRelationship] = expressionRelationshipUnidirectional | expressionRelationshipBidirectional

    lazy val expressionRelationshipUnidirectional: PackratParser[ExpressionRelationshipUnidirectional] = identifier ~ "->" ~ identifier ^^ {
        case el ~ ur ~ er => ExpressionRelationshipUnidirectional(el, er)
    }

    lazy val expressionRelationshipBidirectional: PackratParser[ExpressionRelationshipBidirectional] = identifier ~ "<->" ~ identifier ^^ {
        case el ~ br ~ er => ExpressionRelationshipBidirectional(el, er)
    }

    lazy val expressionLiteral: PackratParser[ExpressionLiteral] = "\"" ~ litRegex ~ "\"" ^^ {
        case ql ~ l ~ qr => ExpressionLiteral(Symbol(l))
    }

    lazy val litRegex = """\p{Alpha}[\p{Alnum} -]*""".r

    lazy val identifier: PackratParser[Symbol] = identifierRegex ^^ {
        case i => Symbol(i)
    }

    lazy val identifierRegex = """\p{Alpha}\p{Alnum}*""".r

}

sealed trait Declaration

case class DeclarationRelationship(entity: ExpressionRelationship, block: BlockAssignment) extends Declaration

case class DeclarationCharacter(entity: ExpressionEntity, block: BlockAssignment) extends Declaration

case class DeclarationSetting(entity: ExpressionEntity, block: BlockAssignment) extends Declaration

case class DeclarationState(block: BlockState) extends Declaration

sealed trait DeclarationAttribute extends Declaration

case class DeclarationAttributeOneSymbol(attribute: Symbol) extends DeclarationAttribute

case class DeclarationAttributeOneUnordered(attribute: Symbol, block: BlockLiteral) extends DeclarationAttribute

case class DeclarationAttributeOneOrdered(attribute: Symbol, block: BlockLiteral) extends DeclarationAttribute

case class DeclarationAttributeSubsetSymbol(attribute: Symbol) extends DeclarationAttribute

case class DeclarationAttributeSubsetUnordered(attribute: Symbol, block: BlockLiteral) extends DeclarationAttribute

case class DeclarationAttributeSubsetOrdered(attribute: Symbol, block: BlockLiteral) extends DeclarationAttribute

sealed trait Block

case class BlockAssignment(assignmentList: List[Expression]) extends Block

case class BlockLiteral(literalList : List[ExpressionLiteral]) extends Block

case class BlockState(declarationAttributeList : List[DeclarationAttribute], declarationCharacterList : List[DeclarationCharacter], declarationRelationshipList: List[DeclarationRelationship])

sealed trait Expression

case class ExpressionLiteral(literal: Symbol) extends Expression

case class ExpressionAttribute(owner: Option[ExpressionAttributeOwner], attribute: Symbol) extends Expression

case class ExpressionUnary(expression: Expression, op: Symbol) extends Expression

case class ExpressionBinary(left: Expression, op: Symbol, right: Expression) extends Expression

case class ExpressionAssignment(left: Expression, op: Symbol, right: Expression) extends Expression

sealed trait ExpressionAttributeOwner

sealed trait ExpressionRelationship extends ExpressionAttributeOwner

case class ExpressionRelationshipBidirectional(left: Symbol, right: Symbol) extends ExpressionRelationship

case class ExpressionRelationshipUnidirectional(left: Symbol, right: Symbol) extends ExpressionRelationship

case class ExpressionEntity(entity: Symbol) extends ExpressionAttributeOwner

object ParserTest extends Parser with App {

    parseAll(expression, "a := \"this is a test\"") match {
        case Success(storyState, next) => println(storyState)
        case NoSuccess(storyState, next) => println(storyState)
    }

    parseAll(expressionMultiplicative, "entity.test") match {
        case Success(storyState, next) => println(storyState)
        case NoSuccess(storyState, next) => println(storyState)
    }

    parseAll(expressionMultiplicative, "(entity -> entity).test") match {
        case Success(storyState, next) => println(storyState)
        case NoSuccess(storyState, next) => println(storyState)
    }

    parseAll(expressionMultiplicative, "(entity <-> entity).test") match {
        case Success(storyState, next) => println(storyState)
        case NoSuccess(storyState, next) => println(storyState)
    }

    parseAll(declarationCharacter, "character matt { first := \"Matt\" last := \"Fielding\" }") match {
        case Success(storyState, next) => println(storyState)
        case NoSuccess(storyState, next) => println(storyState)
    }

    parseAll(declarationRelationship, "relationship matt <-> jake { first := \"Matt\" last := \"Fielding\" age += \"young adult\" + \"adult\" (matt->jake).friendship := \"high\" }") match {
        case Success(storyState, next) => storyState match {
            case DeclarationRelationship(_, BlockAssignment(l)) => l.foreach(println(_))
        }
        case NoSuccess(storyState, next) => println(storyState)
    }

    parseAll(declarationState, io.Source.fromInputStream(getClass.getResourceAsStream("Sample.story")).mkString)match {
        case Success(storyState, next) => storyState match {
            case DeclarationState(BlockState(a, c, r)) => {
                a.foreach(println(_))
                c.foreach(println(_))
                r.foreach(println(_))
            }
        }
        case NoSuccess(storyState, next) => println(storyState)
    }
}
