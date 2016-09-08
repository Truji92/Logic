import polynomial.ImplicationRetraction._
import polynomial.Types.Polynomial
import types.Types.{Impl, Atom}
object testing {
////  [b, c, g] -> [a, p, r, t]
  val List(a,b,c,g,p,r,t,n,d) = List(Atom("a"),Atom("b"),Atom("c"),Atom("g"),Atom("p"),Atom("r"),Atom("t"), Atom("n"), Atom("d"))
//  val f = (b AND c AND g) -> (a AND p AND r AND t)
//
////  val fnew = CImpl(CConj(Set(b,c,g)), CConj(Set(a,p,r,t)))
//  val fnew = Set(b,c,g) -> Set(a,p,r,t)
//
//  delta(fnew, fnew, a)

  val base = Set(
    Set(g) -> Set(c),
    Set(b, c, g) -> Set(a, p, r, t),
    Set(n) -> Set(c, d),
    Set(a, b, c, p) -> Set(g, r, t),
    Set(t) -> Set(a, b, c, g, p, r),
    Set(a, b, c, d, g, p, r, t) -> Set(n),
    Set(d) -> Set(c),
    Set(a) -> Set(b, p),
    Set(r) -> Set(a, b, c, g, p, t)
  )

  base.mkString("\n","\n", "\n")

  val it1 = removeVar(base, a)
  val it2 = removeVar(it1, b)
  val it3 = removeVar(it2, c)
  val it4 = removeVar(it3, d)
  val it5 = removeVar(it4, g)
  val it6 = removeVar(it5, p)
  val it7 = removeVar(it6, n)
      it1.map(item => {
        val CImpl(l,r) = item
        val newl = l.symbols.filterNot(r.symbols.contains(_))
        val newr = r.symbols.filterNot(l.symbols.contains(_))
        CImpl(CConj(newl), CConj(newr))
      }).mkString("\n","\n", "\n")
  //  removeVar(Set(Set(g) -> Set(c), Set(b, c, g) -> Set(a, p, r, t)),a).mkString("\n","\n", "\n")
//
//  delta(Set(g) -> Set(c), Set(b, c, g) -> Set(a, p, r, t),a)
}