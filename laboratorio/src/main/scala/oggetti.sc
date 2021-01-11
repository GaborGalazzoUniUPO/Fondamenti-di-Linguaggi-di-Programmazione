import scala.annotation.tailrec
import scala.language.implicitConversions

//16
class Rational(n: Int, d: Int) {
  require(d != 0)
  private val m = mcd(n.abs, d.abs)
  val numer = n / m
  val denom = d / m
  def this(n: Int) = this(n, 1)
  def + (that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def + (that: Int): Rational = this + new Rational(that)

  def * (that: Rational): Rational =
    new Rational(
      numer * that.numer,
      denom * that.denom
    )

  def * (that: Int): Rational = this * new Rational(that)

  override def toString = numer +"/"+ denom
  @tailrec
  private def  mcd(a: Int, b: Int): Int = if (b == 0) a else mcd(b,a%b)

  def apply (n: Int, d: Int) = new Rational(n,d)
  def unapply (r: Rational) = Some(numer, denom)


}

implicit def intToRational(i: Int): Rational = new Rational(i)

val r1 = new Rational(5,7)
val r2 = new Rational(12,8)
r1+r2
r1*r2
r1+1
r2*2
1+r1
2*r2

abstract class Expr
case class Var(name: String) extends Expr
case class Number(num: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(operator: String,
                 left: Expr, right: Expr) extends Expr

def simplify(e: Expr): Expr = {
  e match {
    case UnOp("!", UnOp("!", b)) => b
    case BinOp("+", Number(0), n) => n
    case BinOp("+", n, Number(0)) => n
    case BinOp("*", Number(0), _) => Number(0)
    case BinOp("*", _, Number(0)) => Number(0)
    case BinOp("*", Number(1), n) => n
    case BinOp("*", n, Number(1)) => n
    case _ => e
  }
}

def expToString(e: Expr): String = {
  e match {
    case Var(name) => name
    case Number(num) => num.toString
    case UnOp(operator, arg) => operator+"("+expToString(arg)+")"
    case BinOp(operator, left, right) => "("+expToString(left)+")"+operator+"("+expToString(right)+")"
  }
}
val e = BinOp("+", BinOp("/", Var("x"), Number(2)),BinOp("/", Number(1.5), Var("x")))
expToString(e)
simplify(UnOp("!",UnOp("!",Var("x"))))
simplify(UnOp("!",UnOp("!",Var("x"))))
simplify(Var("2"))


