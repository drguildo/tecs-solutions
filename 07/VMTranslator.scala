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
  var labels = 0

  val stackBase = 256

  println("@" + stackBase)
  println("D=A")
  println("@SP")
  println("M=D")

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
    // XXX: Check segment index is valid.
    if (segment == "constant") {
      println("@" + index)
      println("D=A")
    } else {
      ""
    }

    // Store value at the top of the stack.
    // XXX: Check for stack overflow.
    dereferencePointer("SP")
    println("M=D")

    // XXX: Check for stack underflow.
    incrementPointer("SP")
  }

  def pop(segment: String, index: Int) {
    throw new UnsupportedOperationException()
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
