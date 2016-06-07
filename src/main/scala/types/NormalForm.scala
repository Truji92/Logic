package types

import Types._

object NormalForm {

  /**
    * Devuelve la proposición en forma normal negativa
    *
    */
  def negative(prop: Prop) = takeInsideNeg(removeImpl(removeEquiv(prop)))

  /**
    * Devuelve la proposición en forma normal conjuntiva
    *
    */
  def conjunctive(prop: Prop) = takeInsideDisj(negative(prop))

  /**
    * Devuelve la proposición en forma normal disyuntiva
    *
    */
  def disjunctive(prop: Prop) = takeInsideConj(negative(prop))

  private def removeEquiv(prop: Prop): Prop = prop match {
    case p: Const => p
    case p: Atom => p
    case Neg(f) => Neg(removeEquiv(f))
    case Conj(f, g) => Conj(removeEquiv(f), removeEquiv(g))
    case Disj(f, g) => Disj(removeEquiv(f), removeEquiv(g))
    case Impl(f, g) => Impl(removeEquiv(f), removeEquiv(g))
    case Equi(f, g) =>
      val _f = removeEquiv(f)
      val _g = removeEquiv(g)
      Conj(Impl(_f,_g), Impl(_g, _f))
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

  private def takeInsideNeg(prop: Prop): Prop = prop match {
    case p: Const => p
    case p: Atom => p
    case Neg(f) => toNegatedForm(f)
    case Conj(f, g) => Conj(takeInsideNeg(f), takeInsideNeg(g))
    case Disj(f, g) => Disj(takeInsideNeg(f), takeInsideNeg(g))
    case Equi(f, g) => Equi(takeInsideNeg(f), takeInsideNeg(g))
    case Impl(f, g) => Impl(takeInsideNeg(f), takeInsideNeg(g))
  }

  private def toNegatedForm(prop: Prop): Prop = prop match {
    case p: Const => Neg(p)
    case p: Atom => Neg(p)
    case Neg(f) => takeInsideNeg(f)
    case Conj(f, g) => Disj(toNegatedForm(f), toNegatedForm(g))
    case Disj(f, g) => Conj(toNegatedForm(f), toNegatedForm(g))
  }

  /**
    * Interioriza Disj en una formula en forma normal negativa
    *
    * !!! prop debe estar en formal normal negativa
    */
  private def takeInsideDisj(prop: Prop): Prop = prop match {
    case Disj(Conj(f1, f2), g) =>
      val _g = takeInsideDisj(g)
      val _f1 = takeInsideDisj(f1)
      val _f2 = takeInsideDisj(f2)
      Conj(Disj(_f1, _g), Disj(_f2, _g))

    case Disj(f, Conj(g1, g2)) => takeInsideDisj(Disj(Conj(g1,g2), f))
    case Disj(f,g) => Conj(takeInsideDisj(f), takeInsideDisj(g))
    case _ => prop
  }

  /**
    * Interioriza Conj en una formula en forma normal negativa
    *
    * !!! prop debe estar en formal normal negativa
    */
  private def takeInsideConj(prop: Prop): Prop = prop match {
    case Conj(Disj(f1,f2), g) =>
      val _g = takeInsideDisj(g)
      val _f1 = takeInsideDisj(f1)
      val _f2 = takeInsideDisj(f2)
      Disj(Conj(_f1, _g), Conj(_f2, _g))

    case Conj(f, Disj(g1,g2)) => takeInsideConj(Conj(Disj(g1,g2), f))
    case Conj(f, g) => Disj(takeInsideConj(f), takeInsideConj(g))
    case _ => prop
  }
 }
