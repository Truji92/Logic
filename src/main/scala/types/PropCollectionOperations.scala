package types

import types.Types._

import scala.language.postfixOps

/**
  * Operaciones sobre conjuntos de Proposiciones
  */
object PropCollectionOperations {

  /**
    * Simbolos en un conjunto de Proposiciones
    *
    * @param props colección de proposiciones
    * @return
    */
  def symbols(props: Iterable[Prop]) =
    props.flatMap(_.symbols).toSet

  /**
    * Todas las interpretaciones de un conjunto de proposiciones
    *
    * @param props
    * @return
    */
  def interpretations(props: Iterable[Prop]) =
    symbols(props).subsets().map( interp => {
      interp.map(elem => (elem, true)).toMap
    } )

  /**
    * Indica si una interpretación es modelo de un conjunto
    *
    * @param interpretation
    * @param props
    * @return
    */
  def isModel(interpretation: Interpretation, props: Iterable[Prop]) =
    props.forall(_.isModel(interpretation))

  /**
    * Devuelve todos los modelos de un conjunto de proposiciones
    *
    * @param props
    */
  def models(props: Iterable[Prop]) =
    interpretations(props).filter(isModel(_, props))

  /**
    * Indica si un conjunto de proposiciones es consistente
    *
    * @param props
    * @return
    */
  def inconsistent(props: Iterable[Prop]) =
    models(props) isEmpty

  /**
    * Indica si un conjunto de proposiciones es inconsistente
    *
    * @param props
    * @return
    */
  def consistent(props: Iterable[Prop]) =
    !inconsistent(props)

  /**
    * Indica si cons es consecuencia lógica de un conjunto de proposiciones
    *
    * @param props
    * @param cons
    * @return
    */
  def logicalConsequence(props: Iterable[Prop])(cons: Prop) =
    interpretations(props ++ Iterable(cons)) forall {
      interp => ! ( isModel(interp, props) && !cons.isModel(interp) )
    }

}
