package types

import types.Types._
import types.Clause

/**
  * Created by Truji on 14/06/2016.
  */
object DavisPutnam {

  def isTautology(clause: Clause) = clause.exists(f => clause.contains(f.complementary))

  def withoutTautologys(clauses: Iterable[Clause]) = clauses filterNot isTautology

  def isUnitary(clause: Clause) = clause.size == 1

  def deleteUnitary(literal: Literal, clauses: Iterable[Clause]) =
    for {
      clause <- clauses
      if !clause.contains(literal)
    } yield clause - literal.complementary

  def firstUnitary(clauses: Iterable[Clause]) = clauses.find(isUnitary)

  def deleteUnitarys(cls: Iterable[Clause]): Iterable[Clause] =
    if(cls.exists(_ == Set.empty)) cls
    else firstUnitary(cls) match {
      case None => cls
      case Some(lit) => deleteUnitarys(deleteUnitary(lit.head, cls))
    }

  def getAllLiterals(clauses: Iterable[Clause]) = clauses.flatten.toSet

  def isPureLiteral(literal: Literal, clauses: Iterable[Clause]) = {
    val l = literal.complementary
    clauses.forall(!_.contains(l))
  }

  def deletePureLiteral(literal: Literal, clauses: Iterable[Clause]) =
    clauses.filter(!_.contains(literal))

  def pureLiterals(clauses: Iterable[Clause]) = getAllLiterals(clauses) filter(isPureLiteral(_, clauses))

  def deletePureLiterals(clauses: Iterable[Clause]): Iterable[Clause] = {
    val literals = pureLiterals(clauses)

    if (literals.isEmpty) clauses
    else deletePureLiterals(deletePureLiteral(literals.head, clauses))
  }

  def fork(clauses: Iterable[Clause], literal: Literal) = {
    val n_l = literal.complementary
    val clausesWithoutLit = clauses.filter(c => c != literal && c != n_l)

    (clauses.filter(_.contains(literal)) ++ clausesWithoutLit, clauses.filter(_.contains(n_l)) ++ clausesWithoutLit)
  }

  def hasUnitaryClauses(clauses: Iterable[Clause]) = firstUnitary(clauses).nonEmpty

  def hasPureLiterals(clauses: Iterable[Clause]) = pureLiterals(clauses).nonEmpty

  def inconsistentByDP(clauses: Iterable[Clause]) = {
    def inconsistent(clauses: Iterable[Clause]): Boolean = {
      if (clauses.isEmpty) false
      else if (clauses.exists(_ == Set.empty)) true
      else if (hasUnitaryClauses(clauses)) inconsistent(deleteUnitarys(clauses))
      else if (hasPureLiterals(clauses)) inconsistent(deletePureLiterals(clauses))
      else {
        val (l, r) = fork(clauses, clauses.head.head)
        inconsistent(l) && inconsistent(r)
      }
    }
    inconsistent(withoutTautologys(clauses))
  }

  def isValidByDP(prop: Prop) = inconsistentByDP(Clause.fromProp(Neg(prop)))

  def isConsecuenceByDP(props: Iterable[Prop], prop: Prop) =
    inconsistentByDP(Clause.clauses(props ++ Set(no(prop))))

}
