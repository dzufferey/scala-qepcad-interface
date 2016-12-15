package dzufferey.qepcad

import org.scalatest._
import dzufferey.smtlib._
import dzufferey.smtlib.InlineOps._
import dzufferey.utils._
import dzufferey.utils.LogLevel._
import Common._
import ArithmeticSimplification.polynomialNF

class ArithmeticSimplificationTest extends FunSuite {

  test("normalize (x - c)²") {
    val f = polynomialNF(pow(x - c, 2))
    val expected = polynomialNF(pow(x, 2) - (Literal(2) * x * c) + pow(c, 2))
    assert(f == expected)
  }

  test("normalize b² (x - c)²") {
    val b2 = pow(b, 2)
    val f = polynomialNF(b2 * pow(x - c, 2))
    val expected = polynomialNF(b2 * pow(x, 2) - (b2 * 2 * x * c) + b2 * pow(c, 2))
    assert(f == expected)
  }

  test("more complex normalization 1") {
    val f = (Plus(Minus(b2 * x2, Literal(2) * b2 * x * c),
                  b2 * c2,
                  Minus(a2 * y2, a2 * b2)) === 0) ==> (x2 + y2 - 1 <= 0)
    val g = ((b2 * pow(x - c, 2)) + (a2 * y2) - (a2 * b2) === 0) ==> (x2 + y2 - 1 <= 0)
    val f1 = polynomialNF(f)
    val g1 = polynomialNF(g)
    assert(f1 == g1)
  }

}
