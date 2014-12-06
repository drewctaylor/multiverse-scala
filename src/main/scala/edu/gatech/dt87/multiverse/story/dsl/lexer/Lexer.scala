package edu.gatech.dt87.multiverse.story.dsl.lexer

import scala.util.parsing.combinator.lexical.Scanners

/**
 * A lexer for the multiverse language.
 */
class Lexer extends Scanners with LexerTokens {
    /**
     * @return a parser that produces a token: an identifier, a number, a string, or an operator.
     */
    def token: Parser[Token] = identifierToken | numberToken | stringToken | operatorToken

    /**
     * A parser that produces an identifier token: a letter optionally followed by a sequence of letters or digits. If
     * the identifier token is among the reserved identifier tokens, the parser returns the case object for that
     * reserved identifier token.
     *
     * @return a parser that produces an identifier token
     */
    def identifierToken: Parser[Token] = letter ~ rep(letter | digit) ^^ {
        case a ~ b =>
            val token = (a +: b).mkString

            if (tokenReservedIdentifierMap.contains(token)) {
                tokenReservedIdentifierMap(token)
            } else {
                TokenIdentifier(token)
            }
    }

    /**
     * A parser that produces a number token: an optional sign, followed by an optional sequence of digits and decimal,
     * followed by at least one digit.
     *
     * @return a parser that produces a number token
     */
    def numberToken: Parser[Token] = opt(sign) ~ opt(rep(digit) ~ decimal) ~ rep1(digit) ~ opt(letter) ^^ {
        case None ~ None ~ d ~ None => TokenLiteralNumber(d.mkString)
        case None ~ Some(b ~ c) ~ d ~ None => TokenLiteralNumber(b.mkString + c + d.mkString)
        case Some(a) ~ None ~ d ~ None => TokenLiteralNumber((a +: d).mkString)
        case Some(a) ~ Some(b ~ c) ~ d ~ None => TokenLiteralNumber((a +: b).mkString + c + d.mkString)
        case a ~ bc ~ d ~ Some(e) => ErrorToken(s"The parser expected a number, but encountered '$e'.")
    }

    /**
     * A parser that produces a string token: a quote, followed by a sequence of string content characters, followed by
     * a quote.
     *
     * @return a parser that produces a string token
     */
    def stringToken: Parser[Token] = '\"' ~ rep(stringTokenContent) ~ opt('\"') ^^ {
        case a ~ b ~ Some(c) => TokenLiteralString(b.mkString)
        case a ~ b ~ None => ErrorToken( s"""The parser expected a '"'.""")
    }

    /**
     * @return a parser that matches string content: a backslash followed by any character or any character other than
     *         a quote.
     */
    def stringTokenContent: Parser[String] = (('\\' ~ escape) | except('\"')) ^^ {
        case a ~ b => List(a, b).mkString
        case a => List(a).mkString
    }

    /**
     * A parser that produces an operator token: a sequence of characters that are neither letters nor digits and that
     * is among the reserved operator tokens.
     *
     * @return a parser that produces an operator token
     */
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

    /**
     * @return a parser that matches whitespace or a comment
     */
    override def whitespace: Parser[Any] = rep(whitespaceHelper | comment)

    /**
     * @return a parser that matches whitespace
     */
    def whitespaceHelper: Parser[Elem] = elem("whitespace", _.isWhitespace)

    /**
     * @return a parser that matches a comment: a tilde, followed by a sequence of comment content characters, followed
     *         by a tilde.
     */
    def comment: Parser[Any] = '~' ~ rep(commentContent) ~ opt('~') ^^ {
        case a ~ b ~ Some(c) => elem("comment", _ => true)
        case a ~ b ~ None => failure("The parser expected a '~'.")
    }

    /**
     * @return a parser that matches comment content: a backslash followed by any character or any character other than
     *         a tilde.
     */
    def commentContent: Parser[String] = (('\\' ~ escape) | except('~')) ^^ {
        case a ~ b => List(a, b).mkString
        case a => List(a).mkString
    }

    /**
     * @return a parser that matches a letter.
     */
    def letter: Parser[Elem] = elem("letter", _.isLetter)

    /**
     * @return a parser that matches a digit.
     */
    def digit: Parser[Elem] = elem("digit", _.isDigit)

    /**
     * @return a parser that matches a sign.
     */
    def sign: Parser[Elem] = elem("sign", _ == '-')

    /**
     * @return a parser that matches a decimal.
     */
    def decimal: Parser[Elem] = elem("decimal", _ == '.')

    /**
     * @return a parser that matches any character.
     */
    def escape: Parser[Elem] = elem("escape", _ => true)

    /**
     * @param characterSequence a character sequence
     * @return a parser that matches any character except those in the given character sequence
     */
    def except(characterSequence: Char*): Parser[Elem] = elem("except $characterSequence", character => characterSequence forall (character != _))
}