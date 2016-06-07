import types.{Types, PropCollectionOperations}
import Types._
import types.PropCollectionOperations
import PropCollectionOperations._
import org.scalatest.FunSuite

import scala.language.postfixOps

/**
  *
  */
class PropositionsTests extends FunSuite {

  val p = Atom("p")
  val q = Atom("q")
  val r = Atom("r")
  val s = Atom("s")

  test("Simbolos Proposicionales") {
    assert(( p OR q -> p).symbols == Set(p, q) )
  }

  test("Significado false") {
    assert(
      !((p OR q) AND (no(q) OR r) meaning Map(r -> true))
    )
  }

  test("Significado true") {
    assert(
      (p OR q) AND (no(q) OR r) meaning Map(r -> true, p -> true)
    )
  }

  test("Interpretaciones") {
    assert(
      (p OR q -> p).interpretations.toSet == Set(Map(p -> true, q -> true), Map(p -> true), Map(q -> true), Map())
    )
  }

  test("No es modelo ") {
    assert(
      !((p OR q) AND (no(q) OR r) isModel Map(r -> true))
    )
  }

  test("Es modelo ") {
    assert(
      p OR q AND no(q) OR r isModel Map(r -> true, p -> true)
    )
  }

  test("Todos los modelos") {
    assert(
      ((p OR q) AND (no(q) OR r)).models.toSet == Set(Map(r -> true, p -> true, q -> true), Map(p->true, r->true), Map(p -> true), Map(q->true, r->true))
    )
  }

  test("Formula valida 1") {
    assert(
      p -> p isValid
    )
  }

  test("Formula valida 2") {
    assert(
      !(p -> q isValid)
    )
  }

  test("Formula valida 3") {
    assert(
      (p -> q) OR (q -> p) isValid
    )
  }

  test("insatisfacible 1") {
    assert(
      !(p OR no(p) unSatisfiable)
    )
  }

  test("insatisfacible 2") {
    assert(
      !((p -> q) OR (q -> r) unSatisfiable)
    )
  }

  test("satisfacible 1 ") {
    assert(
      !(p AND no(p) satisfiable)
    )
  }

  test("satisfacible 2") {
    assert(
      (p->q) AND (q-> r) satisfiable
    )
  }

  test("simbolos conjunto") {
    assert(
      symbols(List(p OR q -> r, p -> s)) == Set(p,q,r,s)
    )
  }

  test("interpretaciones conjunto") {
    assert(
      interpretations(List(p->q, q->r)).toSet == Set(List(p,q,r), List(p,q), List(p,r), List(p), List(q,r), List(q), List(r), Nil).map(l => l.map(i => i -> true).toMap)
    )
  }

  test("modelo Conjunto") {
    assert(
      isModel(Map(p->true, r->true), Set((p OR q) AND (no(q) OR r), q -> r))
    )
  }

  test("no es modelo conjunto"){
    assert(
      !isModel(Map(p -> true, r -> true), Set((p OR q) AND (no(q) OR r), r -> q))
    )
  }

  test("modelos conjunto 1") {
    assert(
      models(Set((p OR q) AND (no(q) OR r), r -> q)).toSet == Set(Map(p->true, q->true, r->true), Map(p->true), Map(q->true, r->true))
    )
  }

  test("consistente true") {
    assert(
      consistent(List( (p OR q) AND (no(q) OR r), p->r))
    )
  }

  test("Consistente false") {
    assert(
      !consistent(List( (p OR q) AND (no(q) OR r), p -> r, no(r) ))
    )
  }

  test("Consecuencia logica") {
    assert(
      logicalConsequence(Set(p -> q, q -> r))(p -> r)
    )
  }

  test("no consecuencia logica") {
    assert(
      !logicalConsequence(Set(p)) (p AND q)
    )
  }

  test("Equivalencia 1") {
    assert {
      (p <-> q) equivalent ( (p -> q) AND (q -> p) )
    }
  }

  test("Equivalencia 2") {
    assert {
      (p -> q) equivalent ( no(p) OR q )
    }
  }

  test("Equivalencia 3") {
    assert {
      (p OR q) equivalent no(no(p) AND no(q))
    }
  }


}