package calculator

import calculator.Calculator.{checkCycle, getReferenceExpr}

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {

  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
    for {
      (name, expr) <- namedExpressions
    } yield name -> Signal(eval(getReferenceExpr(name, namedExpressions), namedExpressions))

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double =
    expr match {
      case Literal(l) => l
      case Ref(l) => if (checkCycle(l, expr, references)) eval(getReferenceExpr(l, references), references) else Double.NaN
      case Plus(a, b) => eval(a, references) + eval(b, references)
      case Minus(a, b) => eval(a, references) - eval(b, references)
      case Times(a, b) => eval(a, references) * eval(b, references)
      case Divide(a, b) => eval(a, references) / eval(b, references)
    }


  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */

  private def checkCycle(origin: String, name: Expr, references: Map[String, Signal[Expr]]): Boolean =
    allRef(Set.empty, references(origin)(), references)

  private def and(xx: Set[String], a: Expr, b: Expr, references: Map[String, Signal[Expr]]): Boolean = allRef(xx, a, references) && allRef(xx, b, references)

  private def allRef(xx: Set[String], name: Expr, references: Map[String, Signal[Expr]]): Boolean =
    name match {
      case Literal(l) => true
      case Ref(l) =>
        if (xx.contains(l)) false else {
          allRef(xx + l, getReferenceExpr(l, references), references)
        }

      case Plus(a, b) =>
        and(xx, a, b, references)

      case Minus(a, b) =>
        and(xx, a, b, references)

      case Times(a, b) =>
        and(xx, a, b, references)

      case Divide(a, b) =>
        and(xx, a, b, references)
    }


  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]): Expr =
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
}
