import types.{Clause, Types, PropCollectionOperations}
import Types._
import PropCollectionOperations._
import org.scalatest.FunSuite

import scala.language.postfixOps

/**
  *
  */
class ResolutionTests extends FunSuite {

  val p = Atom("p")
  val q = Atom("q")
  val r = Atom("r")
  val s = Atom("s")

  test("Literal positivo") {
    assert(
      p.isPositive
    )
  }

  test("Literal positivo false") {
    assert(
      ! no(p).isPositive
    )
  }

  test("Clausula positiva") {
    assert(
      Clause(p, q).isPositive
    )
  }

  test("Clausula positiva false") {
    assert(
      !Clause(p, no(q)).isPositive
    )
  }

  test("Resolventes positivas") {
    assert(
      Clause(no(p), q).positiveResolventes(Set(Clause(p, q), Clause(p, r), Clause(no(q), s))) == Set(Clause(q), Clause(q,r))
    )
  }

  test("Inconsistente Por Resolución Positiva") {
    assert(
      Clause.isInconsistentByPositiveResolution(Set(Clause(p), Clause(no(p),q), Clause(no(q))))
    )
  }

  test("Inconsistente Por Resolución Positiva false") {
    assert(
     ! Clause.isInconsistentByPositiveResolution(Set(Clause(p), Clause(no(q),p)))
    )
  }

  test("Inconsistente Por Resolución Positiva 2") {
    assert(
      Clause.isInconsistentByPositiveResolution(Set(Clause(p, q), Clause(no(q),no(r)), Clause(p, r), Clause(no(p))))
    )
  }

  test("Valida Por Resolución Positiva ") {
    assert(
      Clause.isValidByPositiveResolution(p -> p)
    )
  }

  test("Valida Por Resolución Positiva 2") {
    assert(
      Clause.isValidByPositiveResolution( (p -> q) OR (q -> p))
    )
  }

  test("Valida Por Resolución Positiva false") {
    assert(
     ! Clause.isValidByPositiveResolution(p -> q)
    )
  }

  test("Consecuencia Por Resolución Positiva ") {
    assert(
      Clause.isConsequenceByPositiveResolution(Set(p -> q, q -> r), p -> r)
    )
  }

  test("Consecuencia Por Resolución Positiva false ") {
    assert(
      ! Clause.isConsequenceByPositiveResolution(Set(p -> q, q -> r), p <-> r)
    )
  }

  test("Literal Negativo") {
    assert(
      !p.isNegative
    )
  }

  test("Literal Negativo false") {
    assert(
      no(p).isNegative
    )
  }

  test("Clausula negativa") {
    assert(
      Clause(no(p), no(q)).isNegative
    )
  }

  test("Clausula negativa false") {
    assert(
      ! Clause(p, no(q)).isNegative
    )
  }

  test("Resolventes negativas") {
    assert(
      Clause(no(p), q).negativeResolventes(Set(Clause(p, q), Clause(p, r), Clause(no(q), s))) == Set()
    )
  }

  test("Resolventes negativas 2") {
    assert(
      Clause(no(p), q).negativeResolventes(Set(Clause(p, q), Clause(p, r), Clause(no(q), no(s)))) == Set(Clause(no(p), no(s)))
    )
  }

  test("Inconsistente Por Resolución Negativa") {
    assert(
      Clause.isInconsistentByNegativeResolution(Set(Clause(p), Clause(no(p),q), Clause(no(q))))
    )
  }

  test("Inconsistente Por Resolución Negativa false") {
    assert(
      ! Clause.isInconsistentByNegativeResolution(Set(Clause(p), Clause(no(p),q)))
    )
  }

  test("Inconsistente Por Resolución Negativa 2") {
    assert(
      Clause.isInconsistentByNegativeResolution(Set(Clause(p, q), Clause(p, r), Clause(no(q), no(r)), Clause(no(p))))
    )
  }

  test("Valida Por Resolución Negativa ") {
    assert(
      Clause.isValidByNegativeResolution(p -> p)
    )
  }

  test("Valida Por Resolución Negativa 2") {
    assert(
      Clause.isValidByNegativeResolution( (p -> q) OR (q -> p))
    )
  }

  test("Valida Por Resolución Negativa false") {
    assert(
      ! Clause.isValidByNegativeResolution(p -> q)
    )
  }

  test("Consecuencia Por Resolución Negativa ") {
    assert(
      Clause.isConsequenceByNegativeResolution(Set(p -> q, q -> r), p -> r)
    )
  }

  test("Consecuencia Por Resolución Negativa false ") {
    assert(
      ! Clause.isConsequenceByNegativeResolution(Set(p -> q, q -> r), p <-> r)
    )
  }

  test("Clausula Unitaria") {
    assert(
      Clause(p).isUnitary
    )
  }

  test("Clausula unitaria 2") {
    assert(
      Clause(no(q)).isUnitary
    )
  }

  test("Clausula unitaria false") {
    assert(
      ! Clause(p,q).isUnitary
    )
  }

  test("Resolventes Unitarias") {
    assert(
      Clause(no(p), q).unitaryResolventes(Set(Clause(p, q), Clause(p, r), Clause(no(q), s))) == Set()
    )
  }

  test("Resolventes Unitarias 2") {
    assert(
      Clause(no(p), q).unitaryResolventes(Set(Clause(p, q), Clause(p, r), Clause(no(q)))) == Set(Clause(no(p)))
    )
  }

  test("Inconsistente Por Resolución Unitaria") {
    assert(
      Clause.isInconsistentByUnitaryResolution(Set(Clause(p), Clause(no(p),q), Clause(no(q))))
    )
  }

  test("Inconsistente Por Resolución Unitaria false") {
    assert(
      ! Clause.isInconsistentByUnitaryResolution(Set(Clause(p), Clause(no(p),q)))
    )
  }

  test("Inconsistente Por Resolución Unitaria 2") {
    assert(
      Clause.isInconsistentByUnitaryResolution(Set(Clause(p, q), Clause(p, r), Clause(no(q), no(r)), Clause(no(p))))
    )
  }

  test("Valida Por Resolución Unitaria ") {
    assert(
      Clause.isValidByUnitaryResolution(p -> p)
    )
  }

  test("Valida Por Resolución Unitaria 2") {
    assert(
      Clause.isValidByUnitaryResolution( (p -> q) OR (q -> p))
    )
  }

  test("Valida Por Resolución Unitaria false") {
    assert(
      ! Clause.isValidByUnitaryResolution(p -> q)
    )
  }

  test("Consecuencia Por Resolución Unitaria ") {
    assert(
      Clause.isConsequenceByUnitaryResolution(Set(p -> q, q -> r), p -> r)
    )
  }

  test("Consecuencia Por Resolución Unitaria false ") {
    assert(
      ! Clause.isConsequenceByUnitaryResolution(Set(p -> q, q -> r), p <-> r)
    )
  }

  test("Resolventes Entradas") {
    assert(
      Clause(no(p), q).entryResolvents(Set(), Set(Clause(p, q), Clause(p, r), Clause(no(q), s))) == Set()
    )
  }

  test("Resolventes Entradas 2") {
    assert(
      Clause(no(p), q).entryResolvents(Set(Clause(p, r)), Set(Clause(p, q), Clause(p, r), Clause(no(q), s))) == Set(Clause(q, r))
    )
  }

  test("Inconsistente Por Resolución Entrada") {
    assert(
     ! Clause.isInconsistentByEntryResolution(Set(), Set(Clause(p), Clause(no(p),q), Clause(no(q))))
    )
  }

  test("Inconsistente Por Resolución Entrada 2") {
    assert(
      Clause.isInconsistentByEntryResolution(Set(Clause(p), Clause(no(p),q), Clause(no(q))), Set(Clause(p), Clause(no(p),q), Clause(no(q))))
    )
  }

  test("Inconsistente Por Resolución Entrada false") {
    assert(
      ! Clause.isInconsistentByEntryResolution(Set(Clause(p), Clause(no(p), q)), Set(Clause(p), Clause(no(p),q)))
    )
  }

  test("Valida Por Resolución Entrada ") {
    assert(
      Clause.isValidByEntryResolution(p -> p)
    )
  }

  test("Valida Por Resolución Entrada 2") {
    assert(
      Clause.isValidByEntryResolution( (p -> q) OR (q -> p))
    )
  }

  test("Valida Por Resolución Entrada false") {
    assert(
      ! Clause.isValidByEntryResolution(p -> q)
    )
  }

  test("Consecuencia Por Resolución Entrada ") {
    assert(
      Clause.isConsequenceByEntryResolution(Set(p -> q, q -> r), p -> r)
    )
  }

  test("Consecuencia Por Resolución Entrada false ") {
    assert(
      ! Clause.isConsequenceByEntryResolution(Set(p -> q, q -> r), p <-> r)
    )
  }

  test("Inconsistente Por Resolución Linenal") {
    assert(
      Clause.isInconsistentByLinearResolution(Set( Clause(no(p), q), Clause(p, no(q)), Clause(no(p), no(q)) ), Clause(p, q))
    )
  }
}