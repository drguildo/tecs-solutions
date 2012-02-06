package com.drguildo.tecs.jack

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter

/**
  * The analyzer program operates on a given source, where source is either a
  * file name of the form Xxx.jack or a directory name containing one or more
  * such files. For each source Xxx.jack file, the analyzer goes through the
  * following logic:
  *
  * 1. Create a JackTokenizer from the Xxx.jack input file;
  * 2. Create an output file called Xxx.xml and prepare it for writing;
  * 3. Use the CompilationEngine to compile the input JackTokenizer into the
  *    output file.
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

  def parseFile(f: File) {
    import TokenType._

    val outPath = f.getPath.stripSuffix(".jack") + "T.xml"
    val out = new BufferedWriter(new FileWriter(outPath))

    out.write("<tokens>\n")

    var s = ""
    val tokens = new JackTokenizer(f)
    while (tokens.hasMoreTokens) {
      val t = tokens.nextToken
      s = t._2 match {
        case KEYWORD      => "<keyword> " + t._1 + " </keyword>"
        case SYMBOL       => {
          t._1 match {
            case "<" => "<symbol> &lt; </symbol>"
            case ">" => "<symbol> &gt; </symbol>"
            case _   =>  "<symbol> " + t._1 + " </symbol>"
          }
        }
        case IDENTIFIER   => "<identifier> " + t._1 + " </identifier>"
        case INT_CONST    => "<integerConstant> " + t._1 + " </integerConstant>"
        case STRING_CONST => "<stringConstant> " + t._1 + " </stringConstant>"
      }
      s = s + "\n"
      out.write(s)
    }

    out.write("</tokens>\n")
    out.close

    val syntax = new CompilationEngine(tokens)
  }

  def usage() {
    println("usage: JackAnalyzer [file|directory]")
  }
}

