import org.scalatest._
import org.scalacheck.Gen
import prop._
import polynomial.Types._


/**
  * Created by Truji on 23/06/2016.
  */
class PolynomialTests extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  /** Generators **/
  val varList = List("x", "y", "z", "a", "b", "c")
  val vars = Gen.someOf(varList)
  val monomials = vars.map(v => M(v.toSet))
  val polynomials = for {
    k <- Gen.choose(1, 3)
    ms <- Gen.listOfN(k, monomials)
  } yield Polynomial(ms.toSet)



  /** tests **/
  property("Polynomials operations") {
    forAll(polynomials, polynomials, polynomials){
      (p, q, r) => {

        p + q should equal (q + p)
        p+(q+r) should equal ((p+q)+r)
        p+Pzero should equal (p)

        p * q should equal (q * p)
        p *(q*r) should equal ((p*q)*r)
        p *Pone should equal (p)
        p * p should equal (p)
        p *(q+r) should equal ((p*q)+(p*r))
      }
    }
  }

  property("Reversible") {
    forAll(polynomials) {
      (p) => tr(theta(p)) should equal (p)
    }
  }
}
