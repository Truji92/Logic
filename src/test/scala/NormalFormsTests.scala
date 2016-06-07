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



}
