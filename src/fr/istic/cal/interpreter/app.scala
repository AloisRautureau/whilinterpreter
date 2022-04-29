package fr.istic.cal.interpreter

import scala.io.StdIn._
import fr.istic.cal.interpreter.Interpreter.Memory

object app extends App {
  type History = List[String]
  var hist = Nil : History
  var mem = Nil : Memory
  main
  
  def main = {
    println("While Language Interpreter")
    while(true) {
      print("wli> ")
      hist = readLine() :: hist
      val command = WhileParser.analysercommand(hist.head)
      val result = Interpreter.interpreterCommand(command, mem)
      val toPrint = Interpreter.diff(result, mem).foldLeft("")( (acc, v) => acc + showChanges(v, result) + "\n" )
      mem = result
      print(toPrint)
    }
  }
  
  def showChanges(v: Variable, mem: Memory): String = {
    v match {
      case Var(name) => name + " = " + Prettyprinter.prettyPrintExpr(Interpreter.valueToExpression(Interpreter.lookUp(v, mem)))
    }
  }
  
  def deleteInput(inputLength: Int): Unit = {
    for(_ <- Range(0, inputLength)) print("\b")
  }
}