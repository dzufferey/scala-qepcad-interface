package dzufferey.qepcad

import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._

object Qepcad {

  val command = "qepcad"

  lazy val isPresent =
    try SysCmd(Array(command, "-h"))._1 == 0 
    catch { case _: Throwable => false }

  val defaultMemory = 100000000L
  val defaultPrime = 2000
  val defaultTimeout = 120 * 1000 // 2 min
  
  def sanitize(str: String): String = str.replaceAll("_", "").replaceAll("\\.", "")
  
  def sanitizeNames(rename: Map[Variable, Variable], f: Formula): Formula = f match {
    case l @ Literal(_) => l
    case v @ Variable(_) => rename(v)
    case a @ Application(fct, args) => Application(fct, args.map(sanitizeNames(rename, _))).setType(a.tpe)
    case b @ Binding(bt, vs, f) =>
      val vs2 = vs.map(rename)
      val f2 = sanitizeNames(rename, f)
      Binding(bt, vs2, f2).setType(b.tpe)
  }

  def sanitization(vars: Iterable[Variable]): Map[Variable, Variable] = {
    vars.foldLeft(Map[Variable,Variable]())( (acc, v) => acc + (v -> Variable(sanitize(v.name)).setType(v.tpe)) )
  }

  def reverse(map: Map[Variable, Variable]): Map[Variable, Variable] = {
    map.foldLeft(Map[Variable,Variable]())( (acc, v) => {
      assert(!acc.contains(v._2), "variables clash")
      acc + (v._2 -> v._1)
    })
  }

  def query(freeVariables: List[Variable],
            toEliminate: List[Variable],
            formula: Formula,
            assumption: Option[Formula]) = {
    Query(freeVariables, toEliminate, formula, assumption)
  }

  def isSupported(f: Formula): Boolean = {
    try {
      dzufferey.qepcad.Printer.printFormula(f)(Info)
      true
    } catch { case _: Throwable =>
      false
    }
  }

}
