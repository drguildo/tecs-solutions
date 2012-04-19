package com.drguildo.tecs.jack

// Robustness is sacrificed for simplicity.

import java.io.File

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.matching.Regex

import TokenType._

/**
 * Removes all comments and white space from the input stream and breaks it
 * into Jack-language tokens, as specified by the Jack grammar.
 */
class JackTokenizer(inputFile: File, debug: Boolean) {
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
  input = new Regex("""\t""").replaceAllIn(input, " ")
  // Remove tabs.

  val tokens = ListBuffer[Token]()

  var i = 0
  while (i < input.length) {
    while (i < input.length && input(i) == ' ') {
      i = i + 1
    }

    if (i < input.length) {
      if (symbols.contains(input(i))) {
        if (debug)
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
        if (debug)
          println("String: " + sc)
        tokens.append((sc, STRING_CONST))
      } else if (input(i).isDigit) {
        var ic = ""
        while (i < input.length && input(i) != ' ' && input(i).isDigit) {
          ic = ic + input(i)
          i = i + 1
        }
        if (debug)
          println("Integer: " + ic)
        tokens.append((ic, INT_CONST))
      } else {
        var t = ""
        while (i < input.length && input(i) != ' ' && !symbols.contains(input(i))) {
          t = t + input(i)
          i = i + 1
        }
        if (keywords.contains(t)) {
          if (debug)
            println("Keyword: " + t)
          tokens.append((t, KEYWORD))
        } else {
          if (debug)
            println("Identifier: " + t)
          tokens.append((t, IDENTIFIER))
        }
      }
    }
  }

  reset()

  def hasMoreTokens = i < tokens.length

  def nextToken(): Token = {
    if (i < tokens.length) {
      if (debug)
        println(i + ": " + tokens(i))
      val token = tokens(i)
      i = i + 1
      token
    } else {
      throw new Exception("No more tokens available.")
    }
  }

  def reset() {
    rewind(i)
  }

  def rewind() {
    rewind(1)
  }

  def rewind(n: Int) {
    if ((i - n) >= 0) {
      i = i - n
      println("Rewinding by " + n + "...")
    } else {
      throw new Exception("Attempt to rewind past beginning of token list.")
    }
  }
}
