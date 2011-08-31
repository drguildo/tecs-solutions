// Robustness is sacrificed for simplicity.

class Parser(filename: String) {
  import scala.io.Source
  var lines = Source.fromFile(filename).getLines
                                       .toList
                                       .map(s => s.takeWhile(c => c != '/'))
                                       .filter(s => s != "")
                                       .map(s => s.split(' '))

  val cg = new CodeGenerator(lines)
}

class CodeGenerator(code: List[Array[String]]) {
  val segmentRegisters = Map("local" -> "LCL", "argument" -> "ARG",
                              "this" -> "THIS", "that" -> "THAT")

  var labels = 0

  /*
  val stackBase = 256

  val argumentBase = 300
  val localBase = 400

  val thisBase = 3000
  val thatBase = 3010

  println("@" + stackBase)
  println("D=A")
  println("@SP")
  println("M=D")

  println("@" + localBase)
  println("D=A")
  println("@LCL")
  println("M=D")

  println("@" + argumentBase)
  println("D=A")
  println("@ARG")
  println("M=D")

  println("@" + thisBase)
  println("D=A")
  println("@THIS")
  println("M=D")

  println("@" + thatBase)
  println("D=A")
  println("@THAT")
  println("M=D")
  */

  for (command <- code) {
    command(0) match {
      case "push" => push(command(1), command(2).toInt)
      case "pop"  => pop(command(1), command(2).toInt)
      case "add"  => binaryOperation("M+D")
      case "sub"  => binaryOperation("M-D")
      case "eq"   => comparison("JEQ")
      case "lt"   => comparison("JLT")
      case "gt"   => comparison("JGT")
      case "neg"  => neg()
      case "and"  => binaryOperation("D&M")
      case "or"   => binaryOperation("D|M")
      case _      => throw new IllegalArgumentException("Invalid command.")
    }
  }

  // Arithmetic and logical commands.

  def comparison(op: String) {
    decrementPointer("SP")
    dereferencePointer("SP")
    println("D=M")

    decrementPointer("SP")
    dereferencePointer("SP")
    println("D=M-D")

    val trueLabel = generateLabel()
    val falseLabel = generateLabel()
    println("@" + trueLabel)
    println("D;" + op)
    println("D=0")
    println("@" + falseLabel)
    println("0;JMP")
    println("(" + trueLabel + ")")
    println("D=-1")
    println("(" + falseLabel + ")")

    dereferencePointer("SP")
    println("M=D")

    incrementPointer("SP")
  }

  def binaryOperation(op: String) {
    decrementPointer("SP")
    dereferencePointer("SP")
    println("D=M")

    decrementPointer("SP")
    dereferencePointer("SP")
    println("M=" + op)
    incrementPointer("SP")
  }

  def neg() {
    decrementPointer("SP")
    dereferencePointer("SP")
    println("M=-M")
    incrementPointer("SP")
  }

  // Memory access commands.

  def push(segment: String, index: Int) {
    // Fetch value from segment.
    // XXX: Check segment index is valid.
    if (segment == "constant") {
      println("@" + index)
      println("D=A")
    } else {
      println("@" + index)
      println("D=A")
      if (segment == "pointer") {
        println("@3")
      } else if (segment == "temp") {
        println("@5")
      } else {
        dereferencePointer(segmentRegisters(segment))
      }
      println("A=A+D")
      println("D=M")
    }

    // Store value at the top of the stack.
    // XXX: Check for stack overflow.
    dereferencePointer("SP")
    println("M=D")

    // XXX: Check for stack underflow.
    incrementPointer("SP")
  }

  def pop(segment: String, index: Int) {
    decrementPointer("SP")

    println("@" + index)
    println("D=A")
    if (segment == "pointer") {
      println("@3")
    } else if (segment == "temp") {
      println("@5")
    } else {
      dereferencePointer(segmentRegisters(segment))
    }
    println("D=A+D")
    println("@R13")
    println("M=D")

    dereferencePointer("SP")
    println("D=M")
    println("@R13")
    println("A=M")
    println("M=D")
  }

  // Helper functions.

  def dereferencePointer(register: String) {
    // Store the value contained in a register in A.
    println("@" + register)
    println("A=M")
  }

  def decrementPointer(register: String) {
    println("@" + register)
    println("M=M-1")
  }

  def incrementPointer(register: String) {
    println("@" + register)
    println("M=M+1")
  }

  def generateLabel(): String = {
    val l = "L" + labels
    labels = labels + 1
    l
  }
}

import java.io.File
val f = new File(args(0))

if (f.isDirectory) {
} else {
  new Parser(args(0))
}
