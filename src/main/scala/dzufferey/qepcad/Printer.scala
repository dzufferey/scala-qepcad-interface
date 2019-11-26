package dzufferey.qepcad

import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._


object Printer {

  def printMonomial(f: Formula, first: Boolean)(implicit level: Level = Error): String = {
    val elts = f match {
      case Times(lst @ _*) => lst
      case other => List(other)
    }
    val (lst2, sign) = Misc.mapFold(elts.toList, true, (e: Formula, acc: Boolean) =>  e match {
      case Divides(Literal(l: Long), l2 @ Literal(_)) if l < 0L => (Divides(Literal(-l),l2), !acc)
      case Literal(l: Long) if l < 0L => (Literal(-l), !acc)
      case Literal(d: Double) if d < 0.0 => (Literal(-d), !acc)
      case other => (other, acc)
    })
    val prefix = if (!sign) " - " else if (first) " " else " + "
    prefix + lst2.map(printFormula).mkString(" ")
  }

  def printPolynomial(f: Formula)(implicit level: Level = Error): String = f match {
    case Plus(lst @ _*) =>
      val hd = printMonomial(lst.head, true)
      val tl = lst.tail.map( printMonomial(_, false) ).mkString(" ")
      hd + tl
    case other => printMonomial(other, true)
  }


  def printFormula(formula: Formula)(implicit level: Level = Error): String = formula match {
    case Variable(name) => name
    case Literal(true) => "TRUE"
    case Literal(false) => "FALSE"
    case Literal(i: Int) => i.toString
    case Literal(l: Long) => l.toString
    case Literal(d: Double) if d.isWhole => d.toLong.toString //TODO check for overflow
    case ForAll(vs, f) => vs.map(v => "(A "+v+")").mkString + "[ " + printFormula(f) + " ]"
    case Exists(vs, f) => vs.map(v => "(E "+v+")").mkString + "[ " + printFormula(f) + " ]"
    case And(lst @ _*) => lst.map(printFormula).mkString("[ ", " /\\ ", " ]")
    case Or(lst @ _*) =>  lst.map(printFormula).mkString("[ ", " \\/ ", " ]")
    case Implies(a, b) =>  "[ " + printFormula(a) + " ==> " + printFormula(b) + " ]"
    case Eq(a, b) => printFormula(a) + " = " + printFormula(b)
    case Not(Eq(a, b)) => printFormula(a) + " /= " + printFormula(b)
    case Leq(a, b) => printFormula(a) + " <= " + printFormula(b)
    case Geq(a, b) => printFormula(a) + " >= " + printFormula(b)
    case Lt(a, b) => printFormula(a) + " < " + printFormula(b)
    case Gt(a, b) => printFormula(a) + " > " + printFormula(b)
    case Plus(_*) => printPolynomial(formula)
    case Times(_*) => printMonomial(formula, true)
    case Minus(a, b) => "(" + printFormula(a) + " - " + printFormula(b) + ")" 
    case Divides(a @ Literal(_), b @ Literal(_)) => printFormula(a)+"/"+printFormula(b)
    case Divides(a, b @ Literal(_)) =>  "(" + printFormula(a) + " 1/" + printFormula(b) + ")" 
    case Application(DRealDecl.pow, List(a @ Variable(_), b @ Literal(_))) => "(" + printFormula(a) + "^" + printFormula(b) + ")"
    case other => Logger.logAndThrow("qepcad.Printer", level, "not supported: " + other)
  }
  
}
