// Robustness is sacrificed for simplicity.

import java.io.File

object Parser {
  import scala.collection.mutable.HashMap
  import scala.io.Source

  val segmentRegisters = Map("local"    -> "LCL",
                             "argument" -> "ARG",
                             "this"     -> "THIS",
                             "that"     -> "THAT")

  var currentFunction: Option[String] = None
  var filename = ""
  var numLabels = 0

  def parse(file: File) {
    currentFunction = None
    filename = file.getName.stripSuffix(".vm")

    var code = Source.fromFile(file).getLines.toList

    for (line <- code) {
      val command = line.split(' ')
                        .map(_.trim)
                        .filter(_ != "")
                        .takeWhile(!_.contains('/')) // Remove comments.

      if (command.length > 0) {
        println("// " + command.mkString(" "))

        command(0) match {
          // push segment index
          case "push" => push(command(1), command(2).toInt)
          // pop segment index
          case "pop"  => pop(command(1), command(2).toInt)

          case "add"  => binaryOperation("M+D")
          case "sub"  => binaryOperation("M-D")

          case "neg"  => unaryOperation("-M")
          case "eq"   => comparison("JEQ")
          case "gt"   => comparison("JGT")
          case "lt"   => comparison("JLT")
          case "and"  => binaryOperation("D&M")
          case "or"   => binaryOperation("D|M")
          case "not"  => unaryOperation("!M")

          // labal name
          case "label"   => println("(" + expandLabel(command(1)) + ")")
          // goto name
          case "goto"    => println("@" + expandLabel(command(1)))
          println("0;JMP")
          // if-goto name
          case "if-goto" => decrementPointer("SP")
          dereferencePointer("SP")
          println("D=M")
          println("@" + expandLabel(command(1)))
          println("D;JNE")

          // function name nlocals
          case "function" => function(command(1), command(2).toInt)
          // call name nargs
          case "call"     => call(command(1), command(2).toInt)
          case "return"   => return_()

          case _ => throw new Exception("Invalid or unsupported command.")
        }
      }
    }
  }

  // Memory access commands.

  def push(segment: String, index: Int) {
    // Fetch value from segment.
    // XXX: Check segment index is valid.
    if (segment == "constant") {
      println("@" + index)
      println("D=A")
    } else if (segment == "static") {
      println("@" + filename + "." + index)
      println("D=M")
    } else {
      println("@" + index)
      println("D=A")
      segment match {
        case "pointer" => println("@3")
        case "temp"    => println("@5")
        case _         => dereferencePointer(segmentRegisters(segment))
      }
      println("A=A+D")
      println("D=M")
    }

    // Store value at the top of the stack.
    dereferencePointer("SP")
    println("M=D")

    // XXX: Check for stack overflow.
    incrementPointer("SP")
  }

  def pop(segment: String, index: Int) {
    if (segment == "static") {
      decrementPointer("SP")
      dereferencePointer("SP")
      println("D=M")
      println("@" + filename + "." + index)
      println("M=D")
    } else {
      decrementPointer("SP")

      println("@" + index)
      println("D=A")
      segment match {
        case "pointer" => println("@3")
        case "temp"    => println("@5")
        case _         => dereferencePointer(segmentRegisters(segment))
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

  def unaryOperation(op: String) {
    decrementPointer("SP")
    dereferencePointer("SP")
    println("M=" + op)
    incrementPointer("SP")
  }

  // Function calling

  /* (f)
   * repeat k times:
   * push 0 */

  def function(name: String, locals: Int) {
    currentFunction = Some(name)
    println("(" + name + ")")
    for (i <- 0 until locals) {
      dereferencePointer("SP")
      println("M=0")
      incrementPointer("SP")
    }
  }

  /* push return-address
   * push LCL
   * push ARG
   * push THIS
   * push THAT
   * ARG = SP-n-5
   * LCL = SP
   * goto f
   * (return-address) */

  def call(name: String, args: Int) {
    val returnLabel = generateLabel()
    println("@" + returnLabel)
    println("D=A")
    dereferencePointer("SP")
    println("M=D")
    incrementPointer("SP")

    for (symbol <- List("LCL", "ARG", "THIS", "THAT")) {
      println("@" + symbol)
      println("D=M")
      dereferencePointer("SP")
      println("M=D")
      incrementPointer("SP")
    }

    // ARG = SP-5-args
    println("@" + (5+args))
    println("D=A")
    dereferencePointer("SP")
    println("D=A-D")
    println("@ARG")
    println("M=D")

    dereferencePointer("SP")
    println("D=A")
    println("@LCL")
    println("M=D")

    println("@" + name)
    println("0;JMP")

    println("(" + returnLabel + ")")
  }

  /* FRAME = LCL
   * RET = *(FRAME-5)
   * *ARG = pop()
   * SP = ARG+1
   * THAT = *(FRAME-1)
   * THIS = *(FRAME-2)
   * ARG = *(FRAME-3)
   * LCL = *(FRAME-4)
   * goto RET */

  def return_() {
    dereferencePointer("LCL")
    println("D=A")
    println("@FRAME")
    println("M=D")

    println("@5")
    println("D=A")
    dereferencePointer("FRAME")
    println("A=A-D")
    println("D=M")
    println("@RET")
    println("M=D")

    decrementPointer("SP")
    dereferencePointer("SP")
    println("D=M")
    dereferencePointer("ARG")
    println("M=D")

    dereferencePointer("ARG")
    println("D=A+1")
    println("@SP")
    println("M=D")

    println("@1")
    println("D=A")
    dereferencePointer("FRAME")
    println("A=A-D")
    println("D=M")
    println("@THAT")
    println("M=D")

    println("@2")
    println("D=A")
    dereferencePointer("FRAME")
    println("A=A-D")
    println("D=M")
    println("@THIS")
    println("M=D")

    println("@3")
    println("D=A")
    dereferencePointer("FRAME")
    println("A=A-D")
    println("D=M")
    println("@ARG")
    println("M=D")

    println("@4")
    println("D=A")
    dereferencePointer("FRAME")
    println("A=A-D")
    println("D=M")
    println("@LCL")
    println("M=D")

    dereferencePointer("RET")
    println("0;JMP")
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
    val l = "L" + numLabels
    numLabels = numLabels + 1
    l
  }

  def expandLabel(label: String): String = {
    currentFunction match {
     case Some(n) => n + "$" + label
     case None    => label
    }
  }
}

// Bootstrap

// Locate the stack at RAM[256]
println("@256")
println("D=A")
println("@SP")
println("M=D")

Parser.call("Sys.init", 0)

val f = new File(args(0))

if (f.isDirectory) {
  // TODO: Parse a directory of files.
} else {
  for (filename <- args) {
    val file = new File(filename)
    Parser.parse(file)
  }
}
