package calculator

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {

  def isCyclic(expr: Expr, visited: Set[String], mapExpre: Map[String, Signal[Expr]]): Boolean = {

    def isCyclic(expr: Expr, visited: Set[String]): Boolean = {
      expr match {
        case Literal(d) => false
        case Ref(vName) => mapExpre.get(vName).exists(s => visited.contains(vName) || isCyclic(s(), visited + vName))
        case Plus(exprA, exprB) => isCyclic(exprA, visited) || isCyclic(exprB, visited)
        case Minus(exprA, exprB) => isCyclic(exprA, visited) || isCyclic(exprB, visited)
        case Times(exprA, exprB) => isCyclic(exprA, visited) || isCyclic(exprB, visited)
        case Divide(exprA, exprB) => isCyclic(exprA, visited) || isCyclic(exprB, visited)
      }
    }

    isCyclic(expr, visited)


  }


  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {

    namedExpressions.map {
      kv =>

        val (k: String, v: Signal[Expr]) = kv

        val signal = if (isCyclic(v.apply(), Set(k), namedExpressions)) {
          Signal(Double.NaN)
        } else {
          Signal(eval(v.apply(), namedExpressions))
        }

        //val signal = Signal(eval(v.apply(), namedExpressions))

        k -> signal

    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {

    def testNAN(exprA: Expr, exprB: Expr, operation: (Double, Double) => Double): Double = {
      val a = eval(exprA, references)
      val b = eval(exprB, references)

      if (a.isNaN || b.isNaN) {
        Double.NaN
      } else {
        operation(a, b)
      }
    }

    expr match {
      case Literal(d) => d
      case Ref(vName) if references.contains(vName) => eval(references(vName).apply(), references)
      case Ref(vName) if !references.contains(vName) => Double.NaN
      case Plus(exprA, exprB) => testNAN(exprA, exprB, (a: Double, b: Double) => a + b)
      case Minus(exprA, exprB) => testNAN(exprA, exprB, (a: Double, b: Double) => a - b)
      case Times(exprA, exprB) => testNAN(exprA, exprB, (a: Double, b: Double) => a * b)
      case Divide(exprA, exprB) => testNAN(exprA, exprB, (a: Double, b: Double) => a / b)
    }


  }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]) = {
    references.get (name).fold[Expr] {
Literal (Double.NaN)
} {
exprSignal =>
exprSignal ()
}
  }
}
