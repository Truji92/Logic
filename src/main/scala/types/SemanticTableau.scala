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
  def components(prop: Prop): Set[Prop] = prop match {
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

  /**
    * Si todos las fórmulas de un conjunto son literales
    * @param props
    * @return
    */
  def allLiterals(props: Iterable[Prop]) = props.forall(_.isLiteral)

  def hasContradiction(props: Iterable[Prop]) = props.exists(p => props.exists(_ == Neg(p)))

  /**
    * Expansión de un conjunto de fórmulas mediante doble negación
    *
    */
  def expDN(props: Iterable[Prop], prop: Prop) =
    Set(components(prop) ++ props.filterNot(_ == prop))

  /**
    * Expansión de un conjunto de fórmulas mediante expasión alfa
    *
    */
  def expAlfa(props: Iterable[Prop], prop: Prop) =
    Set(components(prop) ++ props.filterNot(_ == prop))

  /**
    * Expansión de un conjunto de fórmulas mediante expasión beta
    *
    */
  def expBeta(props: Iterable[Prop], prop: Prop) = {
    val _props = props.filterNot(_ == prop)
    components(prop) map (p => Set(p) ++ _props  )
  }

  //@TODO ??? si llega al else && betas.isEmpty??
  /**
    *
    * @param props
    * @return
    */
  def successors(props: Iterable[Prop]) = {
    lazy val doubleNegs = props.find(isDoubleNeg)
    lazy val alfas = props.find(isAlfa)
    lazy val betas = props.find(isBeta)

    if (doubleNegs.nonEmpty) expDN(props, doubleNegs.get)
    else if(alfas.nonEmpty) expAlfa(props, alfas.get)
    else expBeta(props, betas.get)
  }

  /**
    * Obtiene los modelos (en forma de conjunto de proposiciones) de un conjunto de fórmulas
    * por el método de tableros semánticos
    * @param props
    * @return
    */
  def modelsByTableaux(props: Iterable[Prop]): Set[Set[Prop]] =
    if (hasContradiction(props)) Set.empty
    else if (allLiterals(props)) Set(props.toSet)
    else successors(props) flatMap modelsByTableaux


  /**
    * Conjunto de modelos generales de props por el método de tableros semánticos
    * @param props
    * @return
    */
  def generalModels(props: Iterable[Prop]) = {
    val models = modelsByTableaux(props)

    models.filter(
      m => !(models - m).exists(_.subsetOf(m))
    )
  }

  /**
    * Indica si una fórmula es un teorema por el método de tableros semanticos
    * @param prop
    * @return
    */
  def isTheoremByTableaux(prop: Prop) = modelsByTableaux(Set(Neg(prop))) isEmpty

  /**
    * Verifica si la fórmula prop es consecuencia lógica del conjunto de fórmulas props mediante tableros semánticos
    * @param props
    * @param prop
    * @return
    */
  def isDeductibleByTableaux(props: Iterable[Prop], prop: Prop) =
    modelsByTableaux(Set(Neg(prop)) ++ props).isEmpty
}
