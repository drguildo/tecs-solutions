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
  val stackBase = 256

  println("@" + stackBase)
  println("D=A")
  println("@SP")
  println("M=D")

  for (command <- code) {
    command(0) match {
      case "push" => push(command(1), command(2).toInt)
      case "pop"  => println("got a pop")
      case "add"  => add()
      case _      => println("unimplemented") //throw new IllegalArgumentException("Invalid command.")
    }
  }

  // Arithmetic and logical commands.

  def add() {
    decrementPointer("SP")
    dereferencePointer("SP")
    println("D=M")

    decrementPointer("SP")
    dereferencePointer("SP")
    println("M=D+M") // XXX: D gets clobbered by the previous 2 operations.
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
  }

  // Helper functions.

  def getAddress(segment: String, index: Int): Int = {
    0
  }

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

  def copyStackTo(register: String) {
  }
}

import java.io.File
val f = new File(args(0))

if (f.isDirectory) {
} else {
  new Parser(args(0))
}
