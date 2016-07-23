package polynomial

import polynomial.Types.Var
import types.Types._

import scala.language.implicitConversions


/**
  * Created by alejandro on 12/07/16.
  */
object ImplicationRetraction {

  case class CConj(vars: Set[Atom]) {
    def asProp: Prop =
      if (vars.isEmpty) Const(false)
      else {
        val h = vars.head
        val t = vars.tail
        if (t.isEmpty) h
        else Conj(h, CConj(t).asProp)
      }

    def without(v: Atom) = CConj(vars - v)

    def symbols = vars

    override def toString = vars.mkString("[",", ","]")

    def ->(other: CConj) = CImpl(this, other)
  }

  case class CImpl(l: CConj, r: CConj) {
    override def toString = l.toString +" => "+ r.toString
    def symbols = l.symbols ++ r.symbols
  }
  object TRUE extends CImpl(CConj(Set.empty), CConj(Set.empty))

  def delta(c1: CImpl, c2: CImpl, v: Atom): Set[CImpl] =
    if (c1 == c2) selfDelta(c1, v)
    else {

      val CImpl(l1, r1) = c1
      val CImpl(l2, r2) = c2

      if (!c2.symbols.contains(v)) {
        if (!c1.symbols.contains(v)) Set(c1,c2)
        else if (l1.symbols.contains(v)) Set(c2)
        else Set(CImpl(l1, r1.without(v)), c2)
      }

      else if (!c1.symbols.contains(v)) {
        if (l2.symbols.contains(v)) Set(c1)
        else Set(CImpl(l2, r2.without(v)), c1)
      }

      else if (l1.symbols.intersect(l2.symbols) union r1.symbols.intersect(r2.symbols) contains v) Set.empty

      else if (l2.symbols.intersect(r1.symbols).contains(v)) resolvent(c1,c2, v)
      else resolvent(c2,c1,v)
    }

  private def selfDelta(l: CImpl, v: Atom): Set[CImpl] = {
    if (!l.symbols.contains(v)) Set(l)
    else {
      val CImpl(left, right) = l
      if (left.symbols.contains(v)) Set.empty
      else Set(CImpl(left, right.without(v)))
    }
  }

  def resolvent(c1: CImpl, c2: CImpl, v: Atom): Set[CImpl] = {
    val CImpl(l1, r1) = c1
    val CImpl(l2, r2) = c2

    Set(CImpl(l1, r1.without(v)), CImpl(CConj(l1.symbols.union(l2.without(v).symbols)), r2))
  }

  def removeVar(impls: Set[CImpl], v: Atom): Set[CImpl] = {
    val h = impls.head
    val t = impls.tail
    if (t.isEmpty) selfDelta(h,v)
    else removeVar(t, v) ++ impls.flatMap(item => delta(h, item, v))
  }

  implicit def setToCConj(vars: Set[Atom]): CConj = CConj(vars)

  def main(args: Array[String]) {
    val List(a,b,c,g,p,r,t,n,d) = List(Atom("a"),Atom("b"),Atom("c"),Atom("g"),Atom("p"),Atom("r"),Atom("t"), Atom("n"), Atom("d"))

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

    println("Inicial:")
    println(s"Tamaño ${base.size}")
    println(base.mkString("\n","\n", "\n"))
    println("\n====================================\n")

    val order = List(a,b,c,d,g,p,n)

    def iterate(set: Set[CImpl], vs: List[Atom]): Set[CImpl] =
      if (vs.isEmpty) set
      else {
        val v::rest = vs
        println(s"Eliminando $v")
        val newSet = removeVar(set, v)
        println(s"Tamaño ${newSet.size}")
        println(newSet.mkString("\n","\n", "\n"))
        println("\n====================================\n")
        iterate(newSet, rest)
      }


    iterate(base, order)
  }
}
