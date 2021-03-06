import org.scalatest.FunSuite
import types.{Clause, NormalForm}
import types.Types._
import Clause._

class ClausesTests extends FunSuite {

  val p = Atom("p")
  val q = Atom("q")
  val r = Atom("r")
  val s = Atom("s")

  test("Clause 1") {
    assert(
      Clause(p,q,r) == Clause(p,q,r)
    )
  }

  test("Clause from ClausalProp") {
    assert(
      Clause.fromClausalProp( (no(p) OR r) OR (no(p) OR q) ) == Clause(q, r, NegL(p))
    )
  }

  test("Clause from Prop") {
    assert(
      Clause.fromProp(p AND (q -> r)) == Set(Clause(p), Clause(r, NegL(q)))
    )
  }

  test("Clause form neg Prop") {
    assert(
      Clause.fromProp(no(p AND (q -> r))) == Set(Clause(NegL(p), q), Clause(NegL(p), NegL(r)))
    )
  }

  test("Clause from prop double implication") {
    assert(
      Clause.fromProp(no(p <-> r)) == Set(Clause(p,r), Clause(p, NegL(p)), Clause(r, NegL(r)), Clause(NegL(p), NegL(r)))
    )
  }

  test("Clauses collection 1") {
    assert(
      Clause.clauses(List(p -> q, q -> r)) == Set(Clause(q, NegL(p)), Clause(r, NegL(q)))
    )
  }

  test("Clauses collection 2") {
    assert(
      Clause.clauses(List(p -> q, q <-> p)) == Set(Clause(q, NegL(p)), Clause(p, NegL(q)))
    )
  }

  test("Clause symbols") {
    assert(
      Clause.fromLiterals(List(p, q, NegL(p))).symbols == Set(p, q)
    )
  }

  test("Clause Collection Symbols") {
    assert(
      symbols(List(Clause(p,q), Clause(no(q), r))) == Set(p,q,r)
    )
  }

  test("Clause Interpretations") {
    assert(
      Clause.fromLiterals(List(p, q, NegL(p))).interpretations.toSet == Iterator(Map(p->true, q->true), Map(p->true), Map(q->true), Map.empty).toSet
    )
  }

  test("Clause Interpretations empty") {
    assert(
      Clause.fromLiterals(Nil).interpretations.toSet == Iterator(Map.empty).toSet
    )
  }

  test("Clause Collection Interpretations") {
    assert(
      interpretations( List(Clause(p, NegL(q)), Clause(no(p), q) )).toSet == Iterator(Map(p->true, q->true), Map(p->true), Map(q->true), Map.empty).toSet
    )
  }

  test("Clause Collection Interpretations empty") {
    assert(
      interpretations(Nil).toSet == Iterator(Map.empty).toSet
    )
  }

  test("Literal isModel") {
    assert(
      p.isModel(Map(p->true, r->true))
    )
  }

  test("Literal no isModel") {
    assert(
      !q.isModel(Map(p->true, r->true))
    )
  }

  test("Literal isModel negated") {
    assert(
      NegL(q).isModel(Map(p->true, r->true))
    )
  }

  test("Clause isModel") {
    assert(
      Clause.fromLiterals(List(p, q)).isModel(Map(p->true, r->true))
    )
  }

  test("Clause isModel 2") {
    assert(
      Clause.fromLiterals(List(p, NegL(q))).isModel(Map(r->true))
    )
  }

  test("Clause no isModel") {
    assert(
      Clause.fromLiterals(List(p, NegL(q))).isModel(Map(p->true, r->true))
    )
  }

  test("Clause models") {
    assert(
      Clause.fromLiterals(List(q, NegL(p))).models.toSet == Iterator(Map(p->true, q->true), Map(q->true), Map.empty).toSet
    )
  }

  test("Clause models 2") {
    assert(
      Clause.fromLiterals(List(p, NegL(p))).models.toSet == Iterator(Map(p->true), Map.empty).toSet
    )
  }

  test("Clause models empty") {
    assert(
      Clause.fromLiterals(Nil).models.toSet == Set.empty
    )
  }

  test("Clause Collection isModel") {
    assert(
      isModel(Map(p->true, r-> true), List(Clause(p, NegL(q)), Clause(r)))
    )
  }

  test("Clause Collection no isModel") {
    assert(
     ! isModel(Map(p->true, r->false), List(Clause(p, NegL(q)), Clause(r)))
    )
  }

  test("Clause Collection isModel empty") {
    assert(
      isModel(Map(p->true), Nil)
    )
  }

  test("Clause Collection models") {
    assert(
      models(List(Clause(NegL(p), q), Clause(NegL(q), p))).toSet == List(Map(p->true, q->true), Map.empty).toSet
    )
  }

  test("Clause Collection models none") {
    assert(
      models(List(Clause(NegL(p), q), Clause(p), Clause(NegL(q)))).toSet == List().toSet
    )
  }

  test("Clause isValid") {
    assert(
      Clause.fromLiterals(List(p, q, NegL(p))).isValid
    )
  }

  test("Clause no isValid") {
    assert(
      !Clause.fromLiterals(List(p, q, NegL(r))).isValid
    )
  }

  test("Clause empty isValid") {
    assert(
      !Clause.fromLiterals(Nil).isValid
    )
  }

  test("Clause unsatis") {
    assert(
      !Clause.fromLiterals(List(p, q, NegL(p))).unSatisfiable
    )
  }

  test("Clause unsatis 2") {
    assert(
      !Clause.fromLiterals(List(p, q, NegL(r))).unSatisfiable
    )
  }

  test("Clause empty unsatis") {
    assert(
      Clause.fromLiterals(Nil).unSatisfiable
    )
  }

  test("Clause collection isValid 1") {
    assert(
     ! isValid(List(Clause(NegL(p), q), Clause(NegL(q), p)))
    )
  }

  test("Clause collection isValid 2") {
    assert(
      isValid(List(Clause(NegL(p), p), Clause(NegL(q), q)))
    )
  }

  test("Clause collection isValid empty") {
    assert(
      isValid(Nil)
    )
  }

  test("Clause collection Consistence") {
    assert(
      isConsistent(List(Clause(NegL(p), q), Clause(NegL(q), p)))
    )
  }

  test("Clause collection Consistence 2") {
    assert(
      isConsistent(List(Clause(NegL(p), p), Clause(NegL(q), q)))
    )
  }

  test("Clause collection Consistence empty") {
    assert(
      isConsistent(Nil)
    )
  }

  test("Valid by clauses") {
    assert(
     ! (p -> q).isValidByClauses
    )
  }

  test("Valid By Clauses 2") {
    assert(
      ( (p -> q) OR (q -> p) ).isValidByClauses
    )
  }

  test("consequence between clauses") {
    assert(
      consequenceBetweenClauses(List(Clause(NegL(p), q), Clause(NegL(q), r)), List(Clause(NegL(p), r)))
    )
  }

  test("consequence between clauses 2") {
    assert(
      !consequenceBetweenClauses(List(Clause(p)), List(Clause(p), Clause(q)))
    )
  }

  test("Consequence by clause") {
    assert(
      logicalConsequenceByClauses(List(p -> q, q -> r), p -> r)
    )
  }

  test("Consequence by clause2") {
    assert(
      !logicalConsequenceByClauses(List(p), p AND q)
    )
  }
}