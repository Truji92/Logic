import types.Types
import Types._
import org.scalatest.FunSuite
import types.Sequent
import Sequent._
import scala.language.postfixOps

/**
  *
  */
class SequentTests extends FunSuite {

  val p = Atom("p")
  val q = Atom("q")
  val r = Atom("r")
  val s = Atom("s")
  val p1 = Atom("p1")
  val p2 = Atom("p2")

  test("isAxiom 1") {
    assert(
      Sequent(p AND q, r)(r -> q, p AND q).isAxiom
    )
  }

  test("isAxiom 2") {
    assert (
      ! Sequent(p AND q, r)(r -> q, p , q).isAxiom
    )
  }

  test("leftRule 1") {
    assert(
      Sequent(no(p), q)(r).leftRule(no(p)) == Set(Sequent(q)(p, r))
    )
  }

  test("leftRule 2") {
    assert(
      Sequent(r, p AND q)(s).leftRule(p AND q) == Set(Sequent(p, q, r)(s))
    )
  }

  test("leftRule 3") {
    assert(
      Sequent(r, p OR q)(s).leftRule(p OR q) == Set(Sequent(p,r)(s), Sequent(q, r)(s))
    )
  }

  test("leftRule 4") {
    assert(
      Sequent(r, p -> q)(s).leftRule(p -> q) == Set(Sequent(r)(p, s), Sequent(q, r)(s))
    )
  }

  test("leftRule 5") {
    assert(
      Sequent(r, p <-> q)(s).leftRule(p <-> q) == Set(Sequent(p, q, r)(s), Sequent(r)(p,q,s))
    )
  }

  test("rightRule 1") {
    assert(
      Sequent(q)(no(p), r).rightRule(no(p)) == Set(Sequent(p, q)(r))
    )
  }

  test("rightRule 2") {
    assert(
      Sequent(s)(p AND q, r).rightRule(p AND q) == Set(Sequent(s)(p, r), Sequent(s)(q, r))
    )
  }

  test("rightRule 3") {
    assert(
      Sequent(s)(p OR q, r).rightRule(p OR q) == Set(Sequent(s)(p,q,r))
    )
  }

  test("rightRule 4") {
    assert(
      Sequent(s)(p -> q, r).rightRule(p -> q) == Set(Sequent(p, s)(q, r))
    )
  }

  test("rightRule 5") {
    assert(
      Sequent(s)(p <-> q, r).rightRule(p <-> q) == Set(Sequent(p, s)(q, r), Sequent(q, s)(p, r))
    )
  }

  test("Copuestas izq") {
    assert(
      Sequent(no(p), q, r AND s)(no(q)).nonAtomicLefts == Set(no(p), r AND s)
    )
  }

  test("Copuestas der") {
    assert(
      Sequent(no(p), q, r AND s)(no(q), s -> p, r).nonAtomicRights == Set(no(q), s -> p)
    )
  }

  test("Provable") {
    assert(
      Sequent(p -> q, q -> r)(p -> r).isProvable
    )
  }

  test("Provable2") {
    assert(
      ! Sequent(p -> q, q -> r)(p <-> r).isProvable
    )
  }
//  esProbablePorSecuentes' ([],[p --> p]) ==> True,
  test("Provable3") {
    assert(
      Sequent()(p -> p).isProvable
    )
  }

  test("Provable4") {
    assert(
      Sequent(p AND no(p))().isProvable
    )
  }

  test("Provable by seq") {
    assert(
      isProvableBySequents( (p -> q) OR (q -> p) )
    )
  }

  test("Provable by seq 2" ) {
    assert(
      ! isProvableBySequents( (p -> q) )
    )
  }

  test("deductible by seq") {
    assert(
      isDeductibleBySequents( Set(p -> q, q -> r), p -> r )
    )
  }

  test("deductible by seq 2") {
    assert(
      ! isDeductibleBySequents( Set(p -> q, q -> r), p <-> r )
    )
  }
}