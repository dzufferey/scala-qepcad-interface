package dzufferey.qepcad

import org.scalatest._
import dzufferey.smtlib._
import dzufferey.smtlib.InlineOps._
import dzufferey.utils._
import dzufferey.utils.LogLevel._
import Common._

class QepcadTest extends FunSuite {

  //Logger.moreVerbose
  //Logger.moreVerbose

  def mkVar(name: String) = Variable(name).setType(Real)

  def pow(f: Formula, i: Int) = Application(DRealDecl.pow, List(f, Literal(i)))

  // example from https://www.usna.edu/CS/qepcadweb/B/user/UsingQEPCAD.html
  test("X Axis Ellipse") {
    if (Qepcad.isPresent) {
      val fv = List(a, b, c)
      val elim = List(x, y)
      val inner0 =  And(
        a > 0,
        b > 0,
        ( Plus(Minus(b2 * x2, Literal(2) * b2 * x * c),
               b2 * c2,
               Minus(a2 * y2, a2 * b2)) === 0) ==> (x2 + y2 - 1 <= 0)
      )
      val inner = ArithmeticSimplification.polynomialNF(inner0)
      val f = ForAll(elim, inner)
      val expected = And(
        a > 0,
        b > 0,
        Plus(c, Literal(-1) * a, Literal(1)) >= 0,
        Plus(c, a, Literal(-1)) <= 0,
        Or(
            Plus(b2, Literal(-1) * a) < 0,
            Plus(b2 * c2, pow(b, 4), Times(Literal(-1), a2, b2), Literal(-1) * b2, a2) <= 0
        )
      )
      val q = Qepcad.query(fv, elim, f, None)
      val result = q.execute(memory = 8000000)
      assert(result == expected)
    } else {
      Logger("Qepcad", Error, "qepcad not found in the path.")
    }
  }

}

