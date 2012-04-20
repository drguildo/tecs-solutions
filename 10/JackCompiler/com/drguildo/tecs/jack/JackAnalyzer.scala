package com.drguildo.tecs.jack

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter

import TokenType._

/**
 * The analyzer program operates on a given source, where source is either a
 * file name of the form Xxx.jack or a directory name containing one or more
 * such files. For each source Xxx.jack file, the analyzer goes through the
 * following logic:
 *
 * 1. Create a JackTokenizer from the Xxx.jack input file;
 * 2. Create an output file called Xxx.xml and prepare it for writing;
 * 3. Use the CompilationEngine to compile the input JackTokenizer into the
 * output file.
 */
object JackAnalyzer {
  def main(args: Array[String]) {
    if (args.length > 0) {
      val arg = new File(args(0))

      if (arg.isFile) {
        parseFile(arg)
      } else if (arg.isDirectory) {
        for (f <- arg.listFiles) {
          if (f.getName.takeRight(5) == ".jack") {
            parseFile(f)
          }
        }
      } else {
        usage()
      }
    } else {
      usage()
    }
  }

  def parseFile(inputFile: File) {
    val analyzer = new JackTokenizer(inputFile, true)
    val compilationEngine = new CompilationEngine(analyzer)

    val fileWriter = new FileWriter(inputFile.getName.dropRight(5) + ".xml")

    for (line <- compilationEngine.output)
      fileWriter.write(line + "\n")

    fileWriter.close()
  }

  def usage() {
    println("usage: JackAnalyzer [file|directory]")
  }
}
