import org.scalatest.FunSuite
import types.NormalForm
import types.Types._

class NormalFormsTests extends FunSuite {

  val p = Atom("p")
  val q = Atom("q")
  val r = Atom("r")
  val s = Atom("s")

  test("Forma Normal Negativa 1") {
    assert(
      NormalForm.negative(p <-> q) == ( (no(p) OR q) AND (no(q) OR p) )
    )
  }

  test("Forma Normal Negativa 2") {
    assert(
      NormalForm.negative((p OR no(q)) -> r) == ( (no(p) AND q) OR r )
    )
  }

  test("Forma Normal Negativa 3") {
    assert(
      NormalForm.negative( (p AND (q -> r)) -> s) == ( (no(p) OR (q AND no(r))) OR s )
    )
  }

  test("Forma Normal Conjutiva 1") {
    assert(
      NormalForm.conjunctive( p AND (q -> r) ) == ( p AND (no(q) OR r) )
    )
  }

  test("Forma Normal Conjutiva 2") {
    assert(
      NormalForm.conjunctive( no(p AND (q -> r)) ) == ( (q OR no(p)) AND (no(r) OR no(p)) )
    )
  }

  test("Forma Normal Conjutiva 3") {
    assert(
      NormalForm.conjunctive( no(p <-> r) ) equivalent ( ((p OR r) AND (p OR no(p))) AND ( (no(r) OR r) AND (no(r) OR no(p)) ) )
    )
  }

  test("Forma Normal Disyuntiva 1") {
    assert(
      NormalForm.disjunctive( p AND (q -> r) ) == ( (no(q) AND p) OR (r AND p) )
    )
  }

  test("Forma Normal Disyuntiva 2") {
    assert(
      NormalForm.disjunctive( no(p AND (q -> r)) ) == ( no(p) OR (q AND no(r)) )
    )
  }



}
