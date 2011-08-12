// Robustness is sacrificed for simplicity.

import scala.io.Source

object CommandType extends Enumeration {
  type CommandType = Value
  val A_COMMAND = Value // Address
  val C_COMMAND = Value // Computation
  val L_COMMAND = Value // Label
}
import CommandType._

class Parser(filename: String) {
  var symbols = new SymbolTable()

  var lines = Source.fromFile(filename).getLines
                                       .toList
                                       .map(s => s.takeWhile(c => c != '/'))
                                       .map(s => s.replaceAll("\\s+", ""))
                                       .filter(s => s != "")

  var line = 0
  for (l <- lines) {
    commandType(l) match {
      case L_COMMAND => parseLabel(l)
      case _         => line = line + 1
    }
  }

  for (l <- lines) {
    commandType(l) match {
      case A_COMMAND => println(parseIntruction(l))
      case C_COMMAND => println(parseIntruction(l))
      case _         => Nil
    }
  }

  def parseIntruction(instr: String): String = commandType(instr) match {
    case A_COMMAND => {
      val s = symbol(instr)

      if (symbol(instr)(0).isDigit) {
        "0" + dec2bin(s)
      } else {
        symbols.addVariable(s)
        "0" + dec2bin(symbols.getAddress(s).toString)
      }
    }
    case C_COMMAND => "111" + Code.comp(comp(instr)) + Code.dest(dest(instr)) + Code.jump(jump(instr))
  }

  def parseLabel(label: String) {
    symbols.addLabel(symbol(label), line)
  }

  def commandType(instr: String): CommandType = instr(0) match {
    case '@' => A_COMMAND
    case '(' => L_COMMAND
    case _  => C_COMMAND
  }

  def symbol(instr: String): String = commandType(instr) match {
    case A_COMMAND => instr.drop(1)
    case L_COMMAND => instr.substring(1, instr.length - 1)
    case _         => ""
  }

  def dest(instr: String): String = {
    if (instr.contains('=')) {
      instr.takeWhile(x => x != '=')
    } else {
      return ""
    }
  }

  def comp(instr: String): String = {
    var x = 0
    var y = instr.length

    if (instr.contains('=')) x = instr.indexOf('=') + 1
    if (instr.contains(';')) y = instr.indexOf(';')

    instr.substring(x, y)
  }

  def jump(instr: String): String = {
    if (instr.contains(';')) {
      instr.takeRight(3)
    } else {
      return ""
    }
  }

  def dec2bin(num: String): String = {
    def d(n : Int): String = {
      if (n == 0 | n == 1) (n % 2).toString
      else d(n / 2) + (n % 2).toString
    }

    val x = d(num.toInt)

    ("0" * (15 - x.length)) + x
  }
}

object Code {
  def dest(mnemonic: String): String = mnemonic match {
    case "M"   => "001"
    case "D"   => "010"
    case "MD"  => "011"
    case "A"   => "100"
    case "AM"  => "101"
    case "AD"  => "110"
    case "AMD" => "111"
    case _     => "000"
  }

  def comp(mnemonic: String): String = mnemonic match {
    case "0"   => "0101010"
    case "1"   => "0111111"
    case "-1"  => "0111010"
    case "D"   => "0001100"
    case "A"   => "0110000"
    case "!D"  => "0001101"
    case "!A"  => "0110001"
    case "-D"  => "0001111"
    case "-A"  => "0110011"
    case "D+1" => "0011111"
    case "A+1" => "0110111"
    case "D-1" => "0001110"
    case "A-1" => "0110010"
    case "D+A" => "0000010"
    case "D-A" => "0010011"
    case "A-D" => "0000111"
    case "D&A" => "0000000"
    case "D|A" => "0010101"
    case "M"   => "1110000"
    case "!M"  => "1110001"
    case "-M"  => "1110011"
    case "M+1" => "1110111"
    case "M-1" => "1110010"
    case "D+M" => "1000010"
    case "D-M" => "1010011"
    case "M-D" => "1000111"
    case "D&M" => "1000000"
    case "D|M" => "1010101"
    case _     => throw new IllegalArgumentException("Invalid computation.")
  }

  def jump(mnemonic: String): String = mnemonic match {
    case "JGT" => "001"
    case "JEQ" => "010"
    case "JGE" => "011"
    case "JLT" => "100"
    case "JNE" => "101"
    case "JLE" => "110"
    case "JMP" => "111"
    case _     => "000"
  }
}

class SymbolTable {
  var symbols = Map[String, Int](
    "SP" -> 0x0000, "LCL" -> 0x0001, "ARG" -> 0x0002, "THIS" -> 0x0003, "THAT" -> 0x0004,
    "R0" -> 0x0000, "R1" -> 0x0001, "R2" -> 0x0002, "R3" -> 0x0003,
    "R4" -> 0x0004, "R5" -> 0x0005, "R6" -> 0x0006, "R7" -> 0x0007,
    "R8" -> 0x0008, "R9" -> 0x0009, "R10" -> 0x000A, "R11" -> 0x000B,
    "R12" -> 0x000C, "R13" -> 0x000D, "R14" -> 0x000E, "R15" -> 0x000F,
    "SCREEN" -> 0x4000, "KBD" -> 0x6000
  )
  var base = 0x0010

  def addLabel(symbol: String, address: Int) {
    if (!symbols.contains(symbol)) {
      symbols += symbol -> address
    }
  }

  def addVariable(symbol: String) {
    if (!symbols.contains(symbol)) {
      symbols += symbol -> base
      base += 1
    }
  }

  def contains(symbol: String): Boolean = {
    symbols.contains(symbol)
  }

  def getAddress(symbol: String): Int = {
    symbols.get(symbol) match {
      case Some(address) => address
      case None    => throw new IllegalArgumentException("Invalid symbol.")
    }
  }
}

object Assembler {
  def main(args: Array[String]) {
    new Parser(args(0))
  }
}
