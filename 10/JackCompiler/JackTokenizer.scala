// Robustness is sacrificed for simplicity.

package com.drguildo.tecs.jack

import java.io.File

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.matching.Regex

object TokenType extends Enumeration {
  type TokenType = Value
  val KEYWORD, SYMBOL, IDENTIFIER, INT_CONST, STRING_CONST = Value
}
import TokenType._

/**
  * Removes all comments and white space from the input stream and breaks it
  * into Jack-language tokens, as specified by the Jack grammar.
  */
class JackTokenizer(inputFile: File) extends Iterable[(String, TokenType)] {
  val keywords = Array("class", "constructor", "function", "method", "field",
                       "static", "var", "int", "char", "boolean", "void",
                       "true", "false", "null", "this", "let", "do", "if",
                       "else", "while", "return")
  val symbols = Array('{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-',
                      '*', '/', '&', '|', '<', '>', '=', '~')

  var input = Source.fromFile(inputFile).mkString
  input = new Regex("""//.*""").replaceAllIn(input, "") // Remove single-line comments.
  input = new Regex("""/\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/""").replaceAllIn(input, "") // Remove multi-line comments.
  input = new Regex("""\r""").replaceAllIn(input, "") // Remove carriage returns.
  input = new Regex("""\n""").replaceAllIn(input, " ") // Remove newline characters.
  input = new Regex("""\t""").replaceAllIn(input, " ") // Remove tabs.

  val tokens = ListBuffer[(String, TokenType)]()

  var i = 0
  while (i < input.length) {
    while (i < input.length && input(i) == ' ') {
      i = i + 1
    }

    if (i < input.length) {
      if (symbols.contains(input(i))) {
        println("Symbol: " + input(i))
        tokens.append((input(i).toString, SYMBOL))
        i = i + 1
      } else if (input(i) == '"') {
        var sc = ""
        i = i + 1
        while (i < input.length && input(i) != '"') {
          sc = sc + input(i)
          i = i + 1
        }
        i = i + 1
        println("String: " + sc)
        tokens.append((sc, STRING_CONST))
      } else if (input(i).isDigit) {
        var ic = ""
        while (i < input.length && input(i) != ' ' && input(i).isDigit) {
          ic = ic + input(i)
          i = i + 1
        }
        println("Integer: " + ic)
        tokens.append((ic, INT_CONST))
      } else {
        var t = ""
        while (i < input.length && input(i) != ' ' && !symbols.contains(input(i))) {
          t = t + input(i)
          i = i + 1
        }
        if (keywords.contains(t)) {
          println("Keyword: " + t)
          tokens.append((t, KEYWORD))
        } else {
          println("Identifier: " + t)
          tokens.append((t, IDENTIFIER))
        }
      }
    }
  }

  i = 0

  def hasMoreTokens = i < tokens.length

  def nextToken(): (String, TokenType) = {
    val token = tokens(i)
    i = i + 1
    token
  }

  def iterator = tokens.iterator
}
