import scala.annotation.tailrec
import scala.language.implicitConversions

//16
class Rational(n: Int, d: Int) {
  require(d != 0)
  private val m = mcd(n.abs, d.abs)
  val numer = n / m
  val denom = d / m

  def this(n: Int) = this(n, 1)

  def +(that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def +(that: Int): Rational = this + new Rational(that)

  def *(that: Rational): Rational =
    new Rational(
      numer * that.numer,
      denom * that.denom
    )

  def *(that: Int): Rational = this * new Rational(that)

  override def toString = numer + "/" + denom

  @tailrec
  private def mcd(a: Int, b: Int): Int = if (b == 0) a else mcd(b, a % b)

  def apply(n: Int, d: Int) = new Rational(n, d)

  def unapply(r: Rational) = Some(numer, denom)


}

implicit def intToRational(i: Int): Rational = new Rational(i)

val r1 = new Rational(5, 7)
val r2 = new Rational(12, 8)
r1 + r2
r1 * r2
r1 + 1
r2 * 2
1 + r1
2 * r2

// 17
abstract class Expr

case class Var(name: String) extends Expr {
  override def toString = name
}

case class Number(num: Double) extends Expr {
  override def toString = num.toString
}

case class UnOp(operator: String, arg: Expr) extends Expr {
  override def toString = arg match {
    case Var(_) | Number(_) => operator + arg
    case _ => operator + "(" + arg + ")"
  }
}

case class BinOp(operator: String,
                 left: Expr, right: Expr) extends Expr {
  override def toString = left match {
    case Var(_) | Number(_) => left + operator + (right match {
      case Var(_) | Number(_) => right
      case _ => "(" + right + ")"
    })
    case _ => "(" + left + ")"  + operator + (right match {
      case Var(_) | Number(_) => right
      case _ => "(" + right + ")"
    })
  }
}

def simplify(e: Expr): Expr = {
  e match {
    case UnOp("!", UnOp("!", b)) => simplify(b)
    case BinOp("+", Number(0), n) => simplify(n)
    case BinOp("+", n, Number(0)) => simplify(n)
    case BinOp("*", Number(0), _) => Number(0)
    case BinOp("*", _, Number(0)) => Number(0)
    case BinOp("*", Number(1), n) => simplify(n)
    case BinOp("*", n, Number(1)) => simplify(n)
    case Var(s) => Var(s)
    case Number(n) => Number(n)
    case UnOp(o, v) => UnOp(o, simplify(v))
    case BinOp(o, v1, v2) => BinOp(o, simplify(v1), simplify(v2))
  }
}

val e = BinOp("+", BinOp("/", Var("x"), Number(2)), BinOp("/", Number(1.5), Var("x")))
simplify(UnOp("!", UnOp("!", UnOp("!", UnOp("!", Var("x"))))))
simplify(UnOp("!", UnOp("!", Var("x"))))
simplify(Var("2"))

val test = BinOp("-",
  BinOp("*",
    BinOp("+",
      BinOp("*", Var("a"), Number(0)),
      UnOp("!", UnOp("!", Var("b")))),
    BinOp("/",
      BinOp("+", Number(3), Number(0)),
      BinOp("*", Number(9), Number(1)))),
  BinOp("*", Number(1),
    BinOp("+", Number(0), UnOp("!", Var("a")))))
println(simplify(UnOp("!", UnOp("!", Var("a")))))
println(simplify(UnOp("!", UnOp("!", UnOp("!", UnOp("!", Var("a")))))))
println(test)
println(simplify(test))
println(simplify(test))
println(simplify(BinOp("+", BinOp("+", Number(0.0), Number(0.0)), Var("x"))))
println(simplify(BinOp("+", BinOp("+", Number(0.0), Number(0.0)), Var("x"))))
println(simplify(UnOp("!", UnOp("!", UnOp("!", UnOp("!", UnOp("!", Var("x"))))))))
