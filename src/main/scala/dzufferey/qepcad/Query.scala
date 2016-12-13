package dzufferey.qepcad

import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._

case class Query(freeVariables: List[Variable],
                 toEliminate: List[Variable],
                 formula: Formula,
                 assumption: Option[Formula]) {

  //keep a map that we can use for renaming variables after parsing
  val renaming = Qepcad.sanitization(freeVariables ++ toEliminate)
  val derenaming = Qepcad.reverse(renaming)

  val assumption2 = assumption.map(Qepcad.sanitizeNames(renaming, _))
  val unaccounted = toEliminate.filterNot( v => formula.boundVariables(v) )
  val closure = if (unaccounted.isEmpty) formula else Exists(unaccounted, formula)
  val formula2 = Qepcad.sanitizeNames(renaming, closure)
  val allVars = (freeVariables ++ toEliminate).map(renaming)

  def variables = allVars.mkString("(", ", ", ")")
  def nbrFV = freeVariables.length
  def printFormula = Printer.printFormula(formula2) + "."
  def printAssume = assumption2.map(a => "assume " + Printer.printFormula(a) )

  def print {
    println("variables:")
    println(variables)
    println("free variables:")
    println(nbrFV.toString)
    println("formula:")
    println(printFormula)
    println("assumption:")
    printAssume.foreach(println)
  }

  def execute(mcCallum: Boolean = true,
              memory: Long = Qepcad.defaultMemory,
              primeList: Int = Qepcad.defaultPrime,
              timeout: Long = Qepcad.defaultTimeout): Formula = {
    try {
      import java.io._
      val solver = java.lang.Runtime.getRuntime.exec(Array(Qepcad.command, "-noecho", "+N"+memory, "+L"+primeList), null, null)
      val output = new BufferedWriter(new OutputStreamWriter(solver.getOutputStream()))
      val input = new BufferedReader(new InputStreamReader(solver.getInputStream()))
      val error = new BufferedReader(new InputStreamReader(solver.getErrorStream()))
     
      val txt1 = "Enter an informal description  between '[' and ']':"
      val txt2 = "Enter a variable list:"
      val txt3 = "Enter the number of free variables:"
      val txt4 = "Enter a prenex formula:"
      val txt5 = "Before Normalization >"
      val success = "An equivalent quantifier-free formula:"
      
      val to = System.currentTimeMillis + timeout
      
      def readUntil(txt: String) {
        while (!input.ready && System.currentTimeMillis <= to) {
          Thread.sleep(10)
        }
        if (System.currentTimeMillis > to) {
          solver.destroy
          sys.error("Timeout")
        } else {
          val line = input.readLine.trim
          Logger("Qepcad <- ", Info, line)
          if (line != txt) readUntil(txt)
        }
      }
     
      def write(txt: String) {
        Logger("Qepcad -> ", Info, txt)
        output.write(txt)
        output.newLine()
        output.flush()
      }
      
     
      readUntil(txt1)
      write("[ react robot model simplification ]")
     
      readUntil(txt2)
      write(variables)
      
      readUntil(txt3)
      write(nbrFV.toString)
     
      readUntil(txt4)
      write(printFormula)
     
      readUntil(txt5)
      val a = printAssume
      if (a.isDefined) {
        write(a.get)
        readUntil(txt5)
      }
      
      if (!mcCallum && nbrFV > 3) {
        write("go")
        write("proj-op (m,m," + (3 until (nbrFV+toEliminate.size)).map(_ => "h").mkString(",")  + ")")
      }
     
      write("finish")
     
      readUntil(success)
      var line = input.readLine.trim
      while (line == "") {
        line = input.readLine.trim
      }
     
      solver.waitFor
     
      val formula = Parser(line)

      //dumpQuery(Some(formula))

      Qepcad.sanitizeNames(derenaming, formula)
    } catch {
      case t: Throwable =>
        //dumpQuery(None)
        throw t
    }
  }

  def dumpQuery(result: Option[Formula]) = {
    if (!toEliminate.isEmpty) {
      val fname = Namer("eliminationQuery") + ".txt"
      val query =
        "free variables:\n" + freeVariables.map(renaming).mkString(", ") + "\n" +
        "variables to eliminate:\n  " + toEliminate.map(renaming).mkString(", ") + "\n" +
        "formula:\n  " + printFormula + "\n" +
        printAssume.mkString("assumption:\n  ", "\n  ", "\n") +
        "result:\n  " + result.map(Printer.printFormula).getOrElse("???") + "\n"
      IO.writeInFile(fname, query)
    }
  }

}
