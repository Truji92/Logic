package types

import types.Types._

import scala.language.{postfixOps, implicitConversions}


case class Sequent(left: Set[Prop], right: Set[Prop]) {

  /**
    * Indica si el secuente es un Axioma
    * @return
    */
  def isAxiom = left.exists(right.contains)

  /**
    * Calcula el secuente obtenido de aplicar al actual la regla izquierda correspondiente a f
    *
    */
  def leftRule(f: Prop): Set[Sequent] = {
    val nleft = left - f

    f match {
      case Neg(g)         => Set( Sequent(nleft, right + g) )
      case Conj(f1, f2)   => Set( Sequent(nleft + f1 + f2 , right) )
      case Disj(f1, f2)   => Set( Sequent(nleft + f1, right), Sequent(nleft + f2, right) )
      case Impl(f1, f2)   => Set( Sequent(nleft, right + f1), Sequent(nleft + f2, right) )
      case Equi(f1, f2)   => Set( Sequent(nleft + f1 + f2, right), Sequent(nleft, right + f1 + f2) )
    }
  }

  /**
    * Calcula el secuente obtenido de aplicar al actual la regla derecha correspondiente a f
    *
    */
  def rightRule(f: Prop): Set[Sequent] = {
    val nright = right - f

    f match {
      case Neg(f1)        => Set( Sequent(left + f1, nright) )
      case Conj(f1, f2)   => Set( Sequent(left, nright + f1), Sequent(left, nright + f2) )
      case Disj(f1, f2)   => Set( Sequent(left, nright + f1 + f2) )
      case Impl(f1, f2)   => Set( Sequent(left + f1, nright + f2) )
      case Equi(f1, f2)   => Set( Sequent(left + f1, nright + f2), Sequent(left + f2, nright + f1) )
    }
  }

  /**
    * Componentes no atómicos del lado izquierdo del secuente
    * @return
    */
  def nonAtomicLefts = left filter {
    case _: Atom => false
    case _ => true
  }

  /**
    * Componentes no atómicos del lado derecho del secuente
    * @return
    */
  def nonAtomicRights = right filter {
    case _: Atom => false
    case _ => true
  }

  /**
    * Si el secuente es provable
    * @return
    */
  def isProvable: Boolean = {
    lazy val lefts = nonAtomicLefts
    lazy val rights= nonAtomicRights

    if (isAxiom) true
    else if (lefts.nonEmpty) areProvableBySequents(leftRule(lefts.head))
    else if (rights.nonEmpty) areProvableBySequents(rightRule(rights.head))
    else false
  }

  private def proof(n: Int): Boolean = {
    def mark(n: Int) = 1 to n map(_ => "|" ) mkString " "

    lazy val lefts = nonAtomicLefts
    lazy val rights= nonAtomicRights

    println(mark(n) + left.mkString("[", ", ", "]") + "==>" + right.mkString("[", ", ", "]"))

    if (isAxiom) true
    else if (lefts.nonEmpty) proofBySequents(leftRule(lefts.head), n)
    else if (rights.nonEmpty) proofBySequents(rightRule(rights.head), n)
    else false
  }

  def proof: Boolean = proof(1)

  /**
    * Si todos los secuentes de seqs son provables
    * @param seqs
    * @return
    */
  private def areProvableBySequents(seqs : Iterable[Sequent]): Boolean =
    seqs forall (_.isProvable)

  private def proofBySequents(seqs : Iterable[Sequent], n: Int): Boolean =
    seqs forall (_.proof(n + 1))

}

object Sequent {

  /**
    * Indica si una fórmula es provable mediante secuentes
    * @param prop
    * @return
    */
  def isProvableBySequents(prop: Prop) =
    Sequent(Set.empty[Prop], Set(prop)) isProvable

  def proofBySequents(prop: Prop) =
    Sequent()(prop) proof

  /**
    * Indica si una fórmula es deducible de un conjunto mediante secuentes
    * @param props
    * @param prop
    * @return
    */
  def isDeductibleBySequents(props: Iterable[Prop], prop: Prop) =
    Sequent(props.toSet, Set(prop)) isProvable

  /**
    * Constructor simple de secuentes
    *
    * @param left
    * @param right
    * @return
    */
  def apply(left: Prop*)(right: Prop*): Sequent =
    Sequent(left.toSet, right.toSet)

}
