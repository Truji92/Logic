import types.Types
import Types._
import org.scalatest.FunSuite
import types.SemanticTableau._
import scala.language.postfixOps

/**
  *
  */
class TableauTests extends FunSuite {

  val p = Atom("p")
  val q = Atom("q")
  val r = Atom("r")
  val s = Atom("s")
  val p1 = Atom("p1")
  val p2 = Atom("p2")

  test("Doble Negacion 1") {
    assert(
      isDoubleNeg(no(no(p)))
    )
  }

  test("Doble Negacion 2") {
    assert(
      ! isDoubleNeg(no(p -> q))
    )
  }

  test("Components 1") {
    assert(
      components((p AND q) -> r) == Set(no(p AND q), r)
    )
  }

  test("Components 2") {
    assert(
      components(no((p AND q) -> r)) == Set(p AND q, no(r))
    )
  }

  test("Conjunto Literales 1") {
    assert(
      ! allLiterals(List(p -> q, no(r), r AND s, p))
    )
  }

  test("Conjunto Literales 2") {
    assert(
      allLiterals(List(p, no(q), r))
    )
  }

  test("Contradiccion") {
    assert(
      hasContradiction(List(r, p AND q, no(p AND q)))
    )
  }

  test("Exp DN") {
    assert(
      expDN(List(p, no(no(q)), r), no(no(q))) == Set(Set(q,p,r))
    )
  }

  test("Exp Alfa") {
    assert(
      expAlfa(List(q, p AND s, r), p AND s) == Set(Set(q,p,s,r))
    )
  }

  test("Exp Beta") {
    assert(
      expBeta(List(q, p OR s, r), p OR s) == Set(Set(p, q, r), Set(s, q, r))
    )
  }

  test("Sucesores 1") {
    assert(
      successors(List(q OR s, no(no(r)), p1 AND p2)) == Set(Set(r, q OR s, p1 AND p2))
    )
  }

  test("Sucesores 2") {
    assert(
      successors(List(r, q OR s, p1 AND p2)) == Set(Set(p1, p2, r, q OR s))
    )
  }

  test("Sucesores 3") {
    assert(
      successors(List(p1, p2, r, q OR s)) == Set(Set(q, p1, p2, r), Set(s, p1,p2,r))
    )
  }

  test("Models By tab") {
    assert(
      modelsByTableaux(List(p -> q, no(q -> p))) == Set(Set(no(p), q), Set(q, no(p)))
    )
  }

  test("Models By tab 2") {
    assert(
      modelsByTableaux(List(p -> q, no(q) -> no(p))) == Set(Set(no(p), q), Set(no(p)), Set(q), Set(q, no(p)))
    )
  }


  test("General Models ") {
    assert(
      generalModels(List(p -> q, no(q) -> no(p))) == Set(Set(no(p)), Set(q))
    )
  }

  test("theorem by tabs") {
    assert(
      isTheoremByTableaux(p -> p)
    )
  }

  test("theorem by tabs 2") {
    assert(
     ! isTheoremByTableaux(p -> q)
    )
  }

  test("deducible by tabs") {
    assert(
      isDeductibleByTableaux(List(p -> q, q -> r), p -> r)
    )
  }

  test("deducible by tabs 2") {
    assert(
      ! isDeductibleByTableaux(List(p -> q, q -> r), p <-> r)
    )
  }
}