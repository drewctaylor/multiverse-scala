package edu.gatech.dt87.multiverse.story.dsl.lexer

import scala.util.parsing.combinator.lexical.Scanners

class Lexer extends Scanners with LexerTokens {

    def token: Parser[Token] = identifierToken | numberToken | stringToken | operatorToken

    def identifierToken: Parser[Token] = letter ~ rep(letter | digit) ^^ {
        case a ~ b =>
            val token = (a +: b).mkString

            if (tokenReservedIdentifierMap.contains(token)) {
                tokenReservedIdentifierMap(token)
            } else {
                TokenIdentifier(token)
            }
    }

    def numberToken: Parser[Token] = opt(sign) ~ opt(rep(digit) ~ decimal) ~ rep1(digit) ~ opt(letter) ^^ {
        case None ~ None ~ d ~ None => TokenLiteralNumber(d.mkString)
        case None ~ Some(b ~ c) ~ d ~ None => TokenLiteralNumber(b.mkString + c + d.mkString)
        case Some(a) ~ None ~ d ~ None => TokenLiteralNumber((a +: d).mkString)
        case Some(a) ~ Some(b ~ c) ~ d ~ None => TokenLiteralNumber((a +: b).mkString + c + d.mkString)
        case a ~ bc ~ d ~ Some(e) => ErrorToken(s"The parser expected a number, but encountered '$e'.")
    }

    def stringToken: Parser[Token] = '\"' ~ rep(stringTokenContent) ~ opt('\"') ^^ {
        case a ~ b ~ Some(c) => TokenLiteralString(b.mkString)
        case a ~ b ~ None => ErrorToken( s"""The parser expected a '"'.""")
    }

    def stringTokenContent: Parser[String] = (('\\' ~ escape) | except('\"')) ^^ {
        case a ~ b => List(a, b).mkString
        case a => List(a).mkString
    }

    def operatorToken: Parser[Token] = {
        val operatorTokenSeq = tokenReservedOperatorMap.keys.toSeq
        val operatorTokenSeqSort = operatorTokenSeq.sortWith(_.length < _.length)
        val operatorTokenParserSeq = operatorTokenSeqSort.map(token => accept(token.toList) ^^ {
            a => tokenReservedOperatorMap(token)
        })

        operatorTokenParserSeq.foldRight(failure("The parser expected an operator."): Parser[Token])((a, b) => {
            b | a
        })
    }

    override def whitespace: Parser[Any] = rep(whitespaceHelper | comment)

    def whitespaceHelper: Parser[Elem] = elem("whitespace", _.isWhitespace)

    def comment: Parser[Parser[Elem]] = '~' ~ rep(commentContent) ~ opt('~') ^^ {
        case a ~ b ~ Some(c) => elem("comment", _ => true)
        case a ~ b ~ None => failure("The parser expected a '~'.")
    }

    def commentContent: Parser[String] = (('\\' ~ escape) | except('~')) ^^ {
        case a ~ b => List(a, b).mkString
        case a => List(a).mkString
    }

    def letter: Parser[Elem] = elem("letter", _.isLetter)

    def digit: Parser[Elem] = elem("digit", _.isDigit)

    def sign: Parser[Elem] = elem("sign", _ == '-')

    def decimal: Parser[Elem] = elem("decimal", _ == '.')

    def escape: Parser[Elem] = elem("escape", _ => true)

    def except(characterSequence: Char*): Parser[Elem] = elem("except $characterSequence", character => characterSequence forall (character != _))
}