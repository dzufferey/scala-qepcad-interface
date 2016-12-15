package dzufferey.qepcad

import dzufferey.smtlib._
import dzufferey.smtlib.InlineOps._
import dzufferey.utils._
import dzufferey.utils.LogLevel._

object Common {

  Logger.disallow("Typer") // suppress: failed to unify: Int ⇔ Real

  def mkVar(name: String) = Variable(name).setType(Real)

  def pow(f: Formula, i: Int) = Application(DRealDecl.pow, List(f, Literal(i)))

  val a = mkVar("a")
  val b = mkVar("b")
  val c = mkVar("c")
  val x = mkVar("x")
  val y = mkVar("y")

  val a2 = pow(a, 2)
  val b2 = pow(b, 2)
  val c2 = pow(c, 2)
  val x2 = pow(x, 2)
  val y2 = pow(y, 2)

}
