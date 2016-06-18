import types.{Clause, Types, PropCollectionOperations}
import Types._
import PropCollectionOperations._
import org.scalatest.FunSuite

import scala.language.postfixOps

/**
  *
  */
class ResolventesTests extends FunSuite {

  val p = Atom("p")
  val q = Atom("q")
  val r = Atom("r")
  val s = Atom("s")

  test("Resolvente 1") {
    assert(
      Clause(no(p), q).resolvente(Clause(no(q), r), q) == Clause(no(p), r)
    )
  }

  test("Resolvente 2") {
    assert(
      Clause(no(p), no(q)).resolvente(Clause(q, r), no(q)) == Clause(no(p), r)
    )
  }

  test("Resolvente 3") {
    assert(
      Clause(no(p), q).resolvente(Clause(no(p), no(q)), q) == Clause(no(p))
    )
  }

  test("Resolventes 1") {
    Clause(no(p), q).resolventes(Clause(p, no(q))) == Set(Clause(q, no(q)), Clause(no(p), q))
  }

  test("Resolventes 2") {
    Clause(no(p), q).resolventes(Clause(p, q))== Set(Clause(q))
  }

  test("Resolventes conjunto 1") {
    assert(
      Clause(no(p), q).resolventes(Set(Clause(p, q), Clause(p, r), Clause(no(q), s))) == Set(Clause(q), Clause(q, r), Clause(no(p), s))
    )
  }

  test("Inconsistente por resolución") {
    assert(
      Clause.isIncosistentByResolution(Set(Clause(p), Clause(no(p), q), Clause(no(q))))
    )
  }

  test("Inconsistente por resolución 2") {
    assert(
      !Clause.isIncosistentByResolution(Set(Clause(p), Clause(no(q))))
    )
  }

  test("Inconsistente por resolución 3") {
    assert(
      Clause.isIncosistentByResolution(Set(Clause(p, q), Clause(no(p), q), Clause(no(q)), Clause(q, no(p)), Clause(no(p), no(q))))
    )
  }

  test("Inconsistente por resolución 4") {
    assert(
      Clause.isIncosistentByResolution(Set(Clause(p, q), Clause(p, r), Clause(no(q), no(r)), Clause(no(p))))
    )
  }

  test("Validez por resolución") {
    assert(
      Clause.isValidByResolution(p -> p)
    )
  }

  test("Validez por resolución 2") {
    assert(
      Clause.isValidByResolution( (p -> q) OR (q -> p) )
    )
  }

  test("Validez por resolución 3") {
    assert(
     ! Clause.isValidByResolution(p -> q)
    )
  }

  test("consecuencia por resolución") {
    assert(
      Clause.isConsequenceByResolution(Set(p -> q, q -> r), p -> r)
    )
  }

  test("consecuencia por resolución 2") {
    assert(
     ! Clause.isConsequenceByResolution(Set(p -> q, q -> r), p <-> r)
    )
  }
}