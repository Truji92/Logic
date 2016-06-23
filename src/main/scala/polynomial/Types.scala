package polynomial

import types.Types.Prop

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

}
