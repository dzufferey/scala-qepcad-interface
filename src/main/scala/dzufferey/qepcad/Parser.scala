package dzufferey.qepcad

import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object Parser extends StandardTokenParsers {

  import scala.util.parsing.combinator._
  import scala.util.parsing.combinator.lexical._
  import scala.util.parsing.combinator.token._
  import scala.util.parsing.combinator.syntactical._

  lexical.delimiters += (
    "(", ")", "[", "]", "/\\", "\\/",
    "=", "/=", "<", ">", ">=", "<=",
    "+", "-", "*", "/", "^"
  )

  lexical.reserved += (
    "TRUE", "FALSE"
  )

  def lit(l: Long) = Literal(l).setType(Real)

  // num ::= [-] numericLit [/ numericLit]
  def coeff: Parser[Formula] = (
    opt("-") ~ numericLit ~ opt("/" ~> numericLit) ^^ { case sign ~ num ~ denom =>
        val n = if (sign.isDefined) -(num.toLong) else num.toLong
        denom match {
          case Some(d) => Divides(lit(n), lit(d.toLong)).setType(Real)
          case None => lit(n)
        }
      }
  )
  
  def posCoeff: Parser[Formula] = (
    numericLit ~ opt("/" ~> numericLit) ^^ { case num ~ denom =>
      denom match {
        case Some(d) => Divides(lit(num.toLong), lit(d.toLong)).setType(Real)
        case None => lit(num.toLong)
      }
    }
  )

  // variable ::= ident [^ num]
  def variable: Parser[Formula] = (
    ident ~ opt("^" ~> coeff) ^^ { case name ~ exponent => 
        val v = Variable(name).setType(Real)
        exponent match {
         case Some(e) => DRealDecl.pow(v, e)
         case None => v
        }
      }
  )

  //monome ::= rep1( num | variable )
  def monomial: Parser[Formula] =
    rep1( posCoeff | variable ) ^^ ( lst =>
      if (lst.size > 1) Times(lst:_*) else lst.head
    )


  def inverse(a: Formula): Formula = a match {
    case Times(Literal(l: Double), tail @ _*) => Times(Literal(-l) +: tail:_*)
    case Times(Literal(l: Long), tail @ _*) => Times(Literal(-l).setType(Real) +: tail:_*)
    case Literal(l: Double) => Literal(-l)
    case Literal(l: Long) => Literal(-l).setType(Real)
    case Times(lst @ _*)=> Times(Literal(-1) +: lst :_*)
    case other => Times(Literal(-1), other)
  }

  //polynome ::= monome + polynome
  //           | monome - polynome
  //           | monome
  def polynomial: Parser[Formula] = (
    "+" ~> monomial ~ polynomial ^^ { case m ~ Plus(lst @ _*) => Plus(m +: lst:_*) }
  | "-" ~> monomial ~ polynomial ^^ { case m ~ Plus(lst @ _*) => Plus(inverse(m) +: lst:_*) }
  | monomial ~ polynomial ^^ { case m ~ Plus() => m
                               case m ~ Plus(lst @ _*) => Plus(m +: lst:_*) }
  | success(Plus())
  )

  // equation ::= polynome (= | > | >= | < | <=) num
  def equation: Parser[Formula] = (
    polynomial ~ ("=" ~> coeff) ^^ { case p ~ c => Eq(p, c) }
  | polynomial ~ ("/=" ~> coeff) ^^ { case p ~ c => Not(Eq(p, c)) }
  | polynomial ~ (">" ~> coeff) ^^ { case p ~ c => Gt(p, c) }
  | polynomial ~ ("<" ~> coeff) ^^ { case p ~ c => Lt(p, c) }
  | polynomial ~ (">=" ~> coeff) ^^ { case p ~ c => Geq(p, c) }
  | polynomial ~ ("<=" ~> coeff) ^^ { case p ~ c => Leq(p, c) }
  )

  //system ::= equation
  //         | '[' repsep1(equation, /\) ']'
  //         | '[' repsep1(equation, \/) ']'
  def system: Parser[Formula] = (
    "TRUE" ^^^ True()
  | "FALSE" ^^^ False()
  | "[" ~> system <~ "]"
  | equation ~ "/\\" ~ system ^^ { case a ~ _ ~ And(lst @ _*) => And(a +: lst :_*)
                                   case a ~ _ ~ b => And(a, b) }
  | equation ~ "\\/" ~ system ^^ { case a ~ _ ~ Or(lst @ _*) => Or(a +: lst :_*)
                                   case a ~ _ ~ b => Or(a, b) }
  | equation
  )

  def apply(str: String): Formula = {
    Logger("qepcad.Parser", Info, "parsing: " + str)
    val tokens = new lexical.Scanner(str)
    val result = phrase(system)(tokens)
    if (result.successful) {
      val res = result.get
      Logger("qepcad.Parser", Info, "parsed: " + res)
      res
    } else {
      Logger.logAndThrow("qepcad.Parser", LogLevel.Error, "parsing error: " + result.toString)
    }
  }
  
}
