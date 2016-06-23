package polynomial

import types.Types._

import scala.collection.Set

/**
  * Created by Truji on 22/06/2016.
  */
object Types {

  type Var = String

  trait Monomial {

    def * (other: Monomial): Monomial = (this, other) match {
      case (Mzero, _) => Mzero
      case (_, Mzero) => Mzero
      case (M(vars1), M(vars2)) => M(vars1 ++ vars2)
    }

    def * (pol: Polynomial): Polynomial = pol match {
      case Pzero => Pzero
      case Polynomial(ms) => {
        val h = ms.head
        val t = ms.tail

        Polynomial(Set(h * this)) + this * Polynomial(t)
      }
    }

  }
  case object Mzero extends Monomial
  case class M(vars: Set[Var]) extends Monomial
  val Mone = M(Set.empty)

  val Pzero = Polynomial(Set.empty)
  val Pone = Polynomial(Set(Mone))
  case class Polynomial(monomials: Set[Monomial]) {

    private def sumMonomials(p: Set[Monomial], q: Set[Monomial]): Set[Monomial] =
      if (p.isEmpty) q
      else {
        val h = p.head
        val t = p.tail

        if (q.contains(h)) sumMonomials(t, q - h)
        else sumMonomials(t, q) + h
      }

    def + (other: Polynomial) =
      if (monomials.isEmpty) other
      else Polynomial(sumMonomials(monomials, other.monomials))

    def * (other: Polynomial): Polynomial = this match {
      case Pzero => Pzero
      case Polynomial(ms) =>
        (ms.head * other) + (other * Polynomial(ms.tail))
    }

  }

  def tr(prop: Prop): Polynomial = prop match {
    case Const(b) => if (b) Pone else Pzero
    case Atom(symbol) => Polynomial(Set(M(Set(symbol))))
    case Neg(p) => Pone + tr(p)
    case Conj(p, q) => tr(p) * tr(q)
    case Disj(p, q) => tr(q) + tr(p) + (tr(p) * tr(q))
    case Impl(p, q) => Pone + (tr(p) + (tr(p)*tr(q)))
    case Equi(p, q) => Pone + (tr(p) + tr(q))
  }

  def theta(polynomial: Polynomial): Prop = polynomial.monomials.toList match {
    case Nil => Const(false)
    case h::Nil => th2(h)
    case h::t => no (th2(h) <-> theta(Polynomial(t.toSet)))
  }

  def th2(monomial: Monomial): Prop = monomial match {
    case Mzero => Const(false)
    case M(vars) => vars.map(Atom).foldLeft[Prop](Const(true))((p1, p2) => p1 AND p2)
  }

}
