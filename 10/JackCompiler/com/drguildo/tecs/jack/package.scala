package com.drguildo.tecs
package object jack {
  object TokenType extends Enumeration {
    type TokenType = Value
    val KEYWORD, SYMBOL, IDENTIFIER, INT_CONST, STRING_CONST = Value
  }
  import TokenType._

  type Token = (String, TokenType)
}
