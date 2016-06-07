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
    */
  def interpretations(props: Iterable[Prop]) =
    symbols(props).subsets().map( interp => {
      interp.map(elem => (elem, true)).toMap
    } )

  /**
    * Indica si una interpretación es modelo de un conjunto
    *
    */
  def isModel(interpretation: Interpretation, props: Iterable[Prop]) =
    props.forall(_.isModel(interpretation))

  /**
    * Devuelve todos los modelos de un conjunto de proposiciones
    *
    */
  def models(props: Iterable[Prop]) =
    interpretations(props).filter(isModel(_, props))

  /**
    * Indica si un conjunto de proposiciones es consistente
    *
    * @return
    */
  def inconsistent(props: Iterable[Prop]) =
    models(props) isEmpty

  /**
    * Indica si un conjunto de proposiciones es inconsistente
    *
    */
  def consistent(props: Iterable[Prop]) =
    !inconsistent(props)

  /**
    * Indica si cons es consecuencia lógica de un conjunto de proposiciones
    *
    */
  def logicalConsequence(props: Iterable[Prop])(cons: Prop) =
    interpretations(props ++ Iterable(cons)) forall {
      interp => ! ( isModel(interp, props) && !cons.isModel(interp) )
    }

}
