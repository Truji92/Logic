package types

import types.Types._

import scala.language.{postfixOps, implicitConversions}

object Clause {

  /**
    * Class Clause
    *
    *
    */
  case class Clause(literals: Set[Literal]) {

    /**
      * Símbolos proposicionales de la cláusula
      *
      * @return
      */
    def symbols = literals.flatMap(_.symbols)

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
    def isModel(interpretation: Interpretation) = literals.exists(_.isModel(interpretation))

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

    /**
      * Resolvente de esta clausula y otra respecto a un literal
      *
      * @param other
      * @param literal
      * @return
      */
    def resolvente(other: Clause, literal: Literal): Clause =
      Clause((this - literal) ++ (other - literal.complementary))

    /**
      * Conjunto de resolventes de esta clausula con otra
      *
      * @param other
      * @return
      */
    def resolventes(other: Clause): Set[Clause] =
      for {
        literal <- literals
        if other contains literal.complementary
      } yield this resolvente(other, literal)

    /**
      * Conjunto de resolventes de esta clausula y un conjunto de clausas
      *
      * @param others
      * @return
      */
    def resolventes(others: Set[Clause]): Set[Clause] = others flatMap (_.resolventes(this))

    /**
      * Conjunto de resolventes positivas de esta clausula y un conjunto
      *
      * @param others
      * @return
      */
    def positiveResolventes(others: Set[Clause]): Set[Clause] =
      if (isPositive) resolventes(others)
      else (for {
        other <- others
        if other.isPositive
      } yield resolventes(other)) flatten

    /**
      * Conjunto de resolventes positivas de esta clausula y un conjunto
      *
      * @param others
      * @return
      */
    def negativeResolventes(others: Set[Clause]): Set[Clause] =
      if (isNegative) resolventes(others)
      else (for {
        other <- others
        if other.isNegative
      } yield resolventes(other)) flatten

    /**
      * Conjunto de resolventes unitarias de esta clausula y un conjunto
      *
      * @param others
      * @return
      */
    def unitaryResolventes(others: Set[Clause]): Set[Clause] =
      if (isUnitary) resolventes(others)
      else (for {
        other <- others
        if other.isUnitary
      } yield resolventes(other)) flatten

    def entryResolvents(entry: Set[Clause], others: Set[Clause]): Set[Clause] =
      if (entry.contains(this)) resolventes(others)
      else (for {
        other <- others
        if entry.contains(other)
      } yield resolventes(other)) flatten

    def isTautology = literals.exists(f => literals.contains(f.complementary))

    def isPositive = literals.forall(_.isPositive)

    def isNegative = literals.forall(_.isNegative)

    def isUnitary = literals.size == 1

    def unSatisfiable = literals.isEmpty

    def satisfiable = !unSatisfiable

    def ++(other: Clause) = Clause(literals ++ other.literals)

    def - (literal: Literal) = Clause(literals - literal)

    def toProp: Prop = if (literals.isEmpty) Const(false) else literals.tail.foldLeft(literals.head.toProp)((p, l) => {
      p OR l.toProp
    })

    override def toString = literals.mkString("Clause(", ",", ")")
  }

  /**
    * Constructor con lista infinita de parámetros literales
    *
    * @param literals
    * @return
    */
  def apply(literals: Literal*): Clause = Clause(literals.toSet)

  /**
    * Conversión implicita para poder operar directamente sobre el set de literales
    *
    * @param clause
    * @return
    */
  implicit def clauseToSet(clause: Clause): Set[Literal] = clause.literals

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
    case Const(false) => Clause(Set.empty)
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

  /**
    * Si un conjunto de cláusulas es inconsistente
    *
    * @param clauses
    */
  def isInConsistent(clauses: Iterable[Clause]) = !isConsistent(clauses)

  def consequenceBetweenClauses(c1: Iterable[Clause], c2: Iterable[Clause]) =
    ! interpretations(c1 ++ c2).exists(i => isModel(i, c1) && !isModel(i, c2))

  def logicalConsequenceByClauses(props: Iterable[Prop], prop: Prop) =
    consequenceBetweenClauses(clauses(props), fromProp(prop))

  def withoutTautologys(clauses: Iterable[Clause]) = clauses filterNot (_.isTautology)

  /**
    * Indica si un conjunto de clausulas es incosistente por resolución
    *
    * @param clauses
    * @return
    */
  def isInconsistentByResolution(clauses: Set[Clause]) = {

    def inconsistent(support: Set[Clause], usables: Set[Clause]): Boolean =
      if (support isEmpty) false
      else if (support contains Clause(Set.empty)) true
      else {
        val actual = support.head
        val newUsables = usables + actual
        val newSupport = support.tail ++ (
            for {
              c <- actual.resolventes(newUsables)
              if !c.isTautology
              if !support.contains(c)
              if !newUsables.contains(c)
            } yield c
          )

        inconsistent(newSupport, newUsables)
      }

    inconsistent(clauses, Set.empty)
  }

  /**
    * Indica si un conjunto de clausulas es incosistente por resolución positiva
    *
    * @param clauses
    * @return
    */
  def isInconsistentByPositiveResolution(clauses: Set[Clause]): Boolean = {

    def inconsistent(support: Set[Clause], usables: Set[Clause]): Boolean =
      if (support isEmpty) false
      else if (support contains Clause(Set.empty)) true
      else {
        val actual = support.head
        val newUsables = usables + actual
        val newSupport = support.tail ++ (
          for {
            c <- actual.positiveResolventes(newUsables)
            if !c.isTautology
            if !support.contains(c)
            if !newUsables.contains(c)
          } yield c)

        inconsistent(newSupport, newUsables)
      }

    inconsistent(clauses, Set.empty)
  }

  /**
    * Indica si un conjunto de clausulas es incosistente por resolución negativa
    *
    * @param clauses
    * @return
    */
  def isInconsistentByNegativeResolution(clauses: Set[Clause]): Boolean = {

    def inconsistent(support: Set[Clause], usables: Set[Clause]): Boolean =
      if (support isEmpty) false
      else if (support contains Clause(Set.empty)) true
      else {
        val actual = support.head
        val newUsables = usables + actual
        val newSupport = support.tail ++ (
          for {
            c <- actual.negativeResolventes(newUsables)
            if !c.isTautology
            if !support.contains(c)
            if !newUsables.contains(c)
          } yield c)

        inconsistent(newSupport, newUsables)
      }

    inconsistent(clauses, Set.empty)
  }

  /**
    * Indica si un conjunto de clausulas es incosistente por resolución unitaria
    *
    * @param clauses
    * @return
    */
  def isInconsistentByUnitaryResolution(clauses: Set[Clause]): Boolean = {

    def inconsistent(support: Set[Clause], usables: Set[Clause]): Boolean =
      if (support isEmpty) false
      else if (support contains Clause(Set.empty)) true
      else {
        val actual = support.head
        val newUsables = usables + actual
        val newSupport = support.tail ++ (
          for {
            c <- actual.unitaryResolventes(newUsables)
            if !c.isTautology
            if !support.contains(c)
            if !newUsables.contains(c)
          } yield c)

        inconsistent(newSupport, newUsables)
      }

    inconsistent(clauses, Set.empty)
  }

  /**
    * Indica si un conjunto de clausulas es incosistente por resolución por entradas
    *
    * @param clauses
    * @return
    */
  def isInconsistentByEntryResolution(entry: Set[Clause], clauses: Set[Clause]): Boolean = {

    def inconsistent(entry: Set[Clause], support: Set[Clause], usables: Set[Clause]): Boolean =
      if (support isEmpty) false
      else if (support contains Clause(Set.empty)) true
      else {
        val actual = support.head
        val newUsables = usables + actual
        val newSupport = support.tail ++ (
          for {
            c <- actual.entryResolvents(entry, newUsables)
            if !c.isTautology
            if !support.contains(c)
            if !newUsables.contains(c)
          } yield c)

        inconsistent(entry, newSupport, newUsables)
      }

    inconsistent(entry, clauses, Set.empty)
  }

  /**
    * Indica si un conjunto de clausulas es incosistente por resolución por Lineal
    *
    * @param clauses
    * @return
    */
  def isInconsistentByLinearResolution(clauses: Set[Clause], clause: Clause): Boolean =
    if (clauses.isEmpty) false
    else if(clause.isEmpty) true
    else {
      val all = clauses + clause
      clause.resolventes(clauses).diff(all).exists(
        res => isInconsistentByLinearResolution(all, res)
      )
    }

  /**
    * Indica si una propsición es valida mediante resolución
    *
    * @param prop
    * @return
    */
  def isValidByResolution(prop: Prop): Boolean = isInconsistentByResolution(withoutTautologys(fromProp(Neg(prop))).toSet)

  /**
    * Indica si una propsición es valida mediante resolución Positiva
    *
    * @param prop
    * @return
    */
  def isValidByPositiveResolution(prop: Prop): Boolean = isInconsistentByPositiveResolution(withoutTautologys(fromProp(Neg(prop))).toSet)

  /**
    * Indica si una propsición es valida mediante resolución Negativa
    *
    * @param prop
    * @return
    */
  def isValidByNegativeResolution(prop: Prop): Boolean = isInconsistentByNegativeResolution(withoutTautologys(fromProp(Neg(prop))).toSet)

  /**
    * Indica si una propsición es valida mediante resolución Unitaria
    *
    * @param prop
    * @return
    */
  def isValidByUnitaryResolution(prop: Prop): Boolean = isInconsistentByUnitaryResolution(withoutTautologys(fromProp(Neg(prop))).toSet)

  /**
    * Indica si una propsición es valida mediante resolución por entradas
    *
    * @param prop
    * @return
    */
  def isValidByEntryResolution(prop: Prop): Boolean = {
    val c = withoutTautologys(fromProp(Neg(prop))).toSet
    isInconsistentByEntryResolution(c, c)
  }

  /**
    * Indica si una formula es consecuencia lógica de un conjunto mediante resolucion
    *
    * @param props
    * @param prop
    * @return
    */
  def isConsequenceByResolution(props: Iterable[Prop], prop: Prop) =
    isInconsistentByResolution(withoutTautologys(clauses(Set(Neg(prop)) ++ props)).toSet)

  /**
    * Indica si una formula es consecuencia lógica de un conjunto mediante resolucion Positiva
    *
    * @param props
    * @param prop
    * @return
    */
  def isConsequenceByPositiveResolution(props: Iterable[Prop], prop: Prop) =
    isInconsistentByPositiveResolution(withoutTautologys(clauses(Set(Neg(prop)) ++ props)).toSet)

  /**
    * Indica si una formula es consecuencia lógica de un conjunto mediante resolucion Negativa
    *
    * @param props
    * @param prop
    * @return
    */
  def isConsequenceByNegativeResolution(props: Iterable[Prop], prop: Prop) =
    isInconsistentByNegativeResolution(withoutTautologys(clauses(Set(Neg(prop)) ++ props)).toSet)

  /**
    * Indica si una formula es consecuencia lógica de un conjunto mediante resolucion Unitaria
    *
    * @param props
    * @param prop
    * @return
    */
  def isConsequenceByUnitaryResolution(props: Iterable[Prop], prop: Prop) =
    isInconsistentByUnitaryResolution(withoutTautologys(clauses(Set(Neg(prop)) ++ props)).toSet)

  /**
    * Indica si una formula es consecuencia lógica de un conjunto mediante resolucion por entradas
    *
    * @param props
    * @param prop
    * @return
    */
  def isConsequenceByEntryResolution(props: Iterable[Prop], prop: Prop) ={
    val c = withoutTautologys(clauses(Set(Neg(prop)) ++ props)).toSet
    isInconsistentByEntryResolution(c,c)
  }

}
