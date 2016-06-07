package types

import Types._

object NormalForm {

  /**
    * Devuelve la proposición en forma normal negativa
    *
    */
  def negative(prop: Prop) = insideNeg(removeImpl(removeEquiv(prop)))

  private def removeEquiv(prop: Prop): Prop = prop match {
    case p: Const => p
    case p: Atom => p
    case Neg(f) => Neg(removeEquiv(f))
    case Conj(f, g) => Conj(removeEquiv(f), removeEquiv(g))
    case Disj(f, g) => Disj(removeEquiv(f), removeEquiv(g))
    case Impl(f, g) => Impl(removeEquiv(f), removeEquiv(g))
    case Equi(f, g) => {
      val _f = removeEquiv(f)
      val _g = removeEquiv(g)
      Conj(Impl(_f,_g), Impl(_g, _f))
    }
  }

  private def removeImpl(prop: Prop): Prop = prop match {
    case p: Const => p
    case p: Atom => p
    case Neg(f) => Neg(removeImpl(f))
    case Conj(f, g) => Conj(removeImpl(f), removeImpl(g))
    case Disj(f, g) => Disj(removeImpl(f), removeImpl(g))
    case Equi(f, g) => Equi(removeImpl(f), removeImpl(g))
    case Impl(f, g) => Disj(Neg(removeImpl(f)), removeImpl(g))
  }

  private def insideNeg(prop: Prop): Prop = prop match {
    case p: Const => p
    case p: Atom => p
    case Neg(f) => toNegatedForm(f)
    case Conj(f, g) => Conj(insideNeg(f), insideNeg(g))
    case Disj(f, g) => Disj(insideNeg(f), insideNeg(g))
    case Equi(f, g) => Equi(insideNeg(f), insideNeg(g))
    case Impl(f, g) => Impl(insideNeg(f), insideNeg(g))
  }

  private def toNegatedForm(prop: Prop): Prop = prop match {
    case p: Const => Neg(p)
    case p: Atom => Neg(p)
    case Neg(f) => insideNeg(f)
    case Conj(f, g) => Disj(toNegatedForm(f), toNegatedForm(g))
    case Disj(f, g) => Conj(toNegatedForm(f), toNegatedForm(g))
  }
}
