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

    def contains(v: Var) = this match {
      case Mzero => false
      case M(vars) => vars.contains(v)
    }

    def without(v: Var) = this match {
      case Mzero => Mzero
      case M(vars) => M(vars-v)
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

    def deriv(v: Var) = Polynomial(
      for {
        m <- monomials
        if m.contains(v)
      } yield m.without(v)
    )

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

  def deriv(prop: Prop, v: Var) = theta(tr(prop).deriv(v))

  def deltaP(a1: Polynomial, a2: Polynomial, v: Var) = {
    val c1 = a1.deriv(v)
    val c2 = a2.deriv(v)

    Pone + ((Pone+a1*a2)*(Pone+a1*c2+a2*c1+c1*c2))
  }

  def delta(p1: Prop, p2: Prop, v: Var) = theta(deltaP(tr(p1), tr(p2), v))

  def derivatives(props: Set[Prop], v: Var): Set[Prop] = {
    val propL = props.toList
    val pairs = propL.combinations(2).map{
      case e1::e2::Nil => (e1,e2)
    }.toList ::: propL.map(e => (e, e))

    (for {
      (p1, p2) <- pairs
      d = delta(p1, p2, v)
      if d != Const(true)
    } yield d).toSet
  }

  def deltaRefutable(varSelection: Iterable[Prop] => Var)(props: Set[Prop]): Boolean =
    if (!props.exists(p => p != Const(true))) false //Meh
    else if (props.contains(Const(false))) true
    else {
      deltaRefutable(varSelection)(derivatives(props, varSelection(props)))
    }

  def simpleVarSelection(props: Iterable[Prop]): Var = props.find(_.symbols.nonEmpty).map(_.symbols.head.symbol).get

  def simpleDeltaDemostrable(props: Set[Prop], prop: Prop): Boolean = deltaRefutable(simpleVarSelection)(props + no(prop))

  def simpleDeltaTeorema(prop: Prop) =
    deltaRefutable(simpleVarSelection)(Set(no(prop)))


}
