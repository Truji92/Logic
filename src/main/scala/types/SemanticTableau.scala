package types

import Types._

/**
  * Created by Truji on 09/06/2016.
  */
object SemanticTableau {

  /**
    * Indica si una fórmula es una doble negación
    *
    * @param prop fórmula
    */
  def isDoubleNeg(prop: Prop) = prop match {
    case Neg(Neg(_)) => true
    case _ => false
  }

  /**
    * Indica si una proposición es una fórmula Alfa
    * @param prop fórmula a analizar
    * @return
    */
  def isAlfa(prop: Prop) = prop match {
    case Conj(_, _)       => true
    case Neg(Impl(_, _))  => true
    case Neg(Disj(_, _))  => true
    case _                => false
  }


  /**
    * Indica si una proposición es una fórmula Beta
    * @param prop fórmula a analizar
    * @return
    */
  def isBeta(prop: Prop) = prop match {
    case Disj(_, _)       => true
    case Impl(_, _)       => true
    case Neg(Conj(_, _))  => true
    case Equi(_, _)       => true
    case Neg(Equi(_, _))  => true
    case _                => false
  }

  /**
    * Devuelve un Set que contiene los componentes de la formula prop
    * @param prop
    * @return
    */
  def components(prop: Prop) = prop match {
    case Neg(Neg(f))      => Set(f)
    case Conj(f, g)       => Set(f, g)
    case Neg(Impl(f, g))  => Set(f, Neg(g))
    case Neg(Disj(f, g))  => Set(Neg(f), Neg(g))
    case Disj(f, g)       => Set(f, g)
    case Impl(f, g)       => Set(Neg(f), g)
    case Neg(Conj(f, g))  => Set(Neg(f), Neg(g))
    case Equi(f, g)       => Set(Conj(f, g), Conj(no(f), no(g)))
    case Neg(Equi(f, g))  => Set(Conj(f, Neg(g)), Conj(Neg(f), g))
  }

  def allLiterals(props: Iterable[Prop]) = props.forall(_.isLiteral)

  def hasContradiction(props: Iterable[Prop]) = props.exists(p => props.exists(_ == Neg(p)))

  /**
    * Expansión de un conjunto de fórmulas mediante doble negación
    * @param props
    */
  def expDN(props: Iterable[Prop], prop: Prop) =
    Set(components(prop) ++ props.filterNot(_ == prop))

  def expAlfa(props: Iterable[Prop], prop: Prop) =
    Set(components(prop) ++ props.filterNot(_ == prop))

  def expBeta(props: Iterable[Prop], prop: Prop) = {
    val _props = props.filterNot(_ == prop)
    components(prop) map (p => Set(p) ++ _props  )
  }
}
