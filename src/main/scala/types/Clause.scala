package types

import types.Types._

import scala.language.implicitConversions

/**
  * Created by Truji on 08/06/2016.
  */
object Clause {

  def apply(literals: Literal*): Clause = Clause(literals.toSet)

  /**
    * Creación de Cláusulas a partir de conjuntos de literales
    *
    * @param literals
    * @return
    */
  def fromLiterals(literals: Iterable[Literal]): Clause = Clause(literals.toSet)

  /**
    * Genera una clausula a partir de una fórmula-clausal (Solo contiene Literales y Disyunciones)
    *
    */
  def fromClausalProp(prop: Prop): Clause = prop match {
    case Neg(Atom(s)) => Clause(Set(NegL(Atom(s))))
    case Disj(f, g) => fromClausalProp(f) ++ fromClausalProp(g)
    case l: Literal => Clause(Set(l))
    case _ => throw new Exception(s"$prop no es una fórmula clausal")
  }

  /**
    * Genera el conjunto de clausulas equivalente a una fórmula en modo FNC
    */
  private def fromFNC(prop: Prop): Set[Clause] = prop match {
    case Conj(f,g) => fromFNC(f) ++ fromFNC(g)
    case _ => Set(fromClausalProp(prop))
  }

  /**
    * Genera el conjunto de cláusulas equivalente a una proposición
    *
    * @param prop
    * @return
    */
  def fromProp(prop: Prop) = fromFNC(NormalForm.conjunctive(prop))

  /**
    * Clausulas equivalentes a un conjunto de fórmulas
    *
    * @param props
    */
  def clauses(props: Iterable[Prop]) = props.flatMap(fromProp).toSet

  /**
    * Símbolos de un conjunto de cláusulas
    *
    * @param clauses
    * @return
    */
  def symbols(clauses: Iterable[Clause]) = clauses.flatMap(_.symbols).toSet

  /**
    * Interpretaciones de un conjunto de cláusulas
    *
    * @param clauses
    * @return
    */
  def interpretations(clauses: Iterable[Clause]) =
    symbols(clauses).subsets().map( interp => {
      interp.map(elem => (elem, true)).toMap
    } )

  /**
    * Indica si una interpretación es modelo de un conjunto de clausulas
    *
    * @param interpretation
    * @param clauses
    * @return
    */
  def isModel(interpretation: Interpretation, clauses: Iterable[Clause]) =
    clauses.forall(_.isModel(interpretation))

  /**
    * Modelos de un conjunto de clausulas
    *
    * @param clauses
    * @return
    */
  def models(clauses: Iterable[Clause]) =
    interpretations(clauses).filter(isModel(_, clauses))

  /**
    * Si un conjunto de cláusulas es válido
    *
    * @param clauses
    * @return
    */
  def isValid(clauses: Iterable[Clause]) = clauses.forall(_.isValid)

  /**
    * Si un conjunto de cláusulas es consistente
    *
    * @param clauses
    */
  def isConsistent(clauses: Iterable[Clause]) = interpretations(clauses).exists(isModel(_, clauses))

  def isInConsistent(clauses: Iterable[Clause]) = !isConsistent(clauses)

  def consequenceBetweenClauses(c1: Iterable[Clause], c2: Iterable[Clause]) =
    ! interpretations(c1 ++ c2).exists(i => isModel(i, c1) && !isModel(i, c2))

  def logicalConsequenceByClauses(props: Iterable[Prop], prop: Prop) =
    consequenceBetweenClauses(clauses(props), fromProp(prop))

  /**
    * Clase con conversión implicita para añadir operaciones al tipo Clause
    *
    * @param clause
    */
  case class Clause(clause: Set[Literal]) {

    /**
      * Símbolos proposicionales de la cláusula
      *
      * @return
      */
    def symbols = clause.flatMap(_.symbols)

    /**
      * Interpretaciones de una cláusula
      *
      * @return
      */
    def interpretations = symbols.subsets().map( interp => {
      interp.map(elem => (elem, true)).toMap
    })

    /**
      * Si una interpretación es modelo de la cláusula
      *
      * @param interpretation
      * @return
      */
    def isModel(interpretation: Interpretation) = clause.exists(_.isModel(interpretation))

    /**
      * Todos los modelos de la cláusula
      *
      * @return
      */
    def models = interpretations.filter(isModel)

    /**
      * Indica si una cláusula es válida
      *
      * @return
      */
    def isValid = interpretations.forall(isModel)

    def unSatisfiable = clause.isEmpty

    def satisfiable = !unSatisfiable

    def ++(other: Clause) = Clause(clause ++ other.clause)

    override def toString = clause.mkString("Clause(", ",", ")")
  }


}
