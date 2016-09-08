package polynomial

import polynomial.Types.Var
import types.Clause._
import types.{Clause, PropCollectionOperations}
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

  class CImpl(_l: CConj, _r: CConj) {

    val l = _l
    val r = CConj(_r.vars.diff(_l.vars))

    override def toString = l.toString +" => "+ r.toString

    def symbols = l.symbols ++ r.symbols

    override def equals(o: scala.Any): Boolean = o match {
      case CImpl(ol, or) => ol.vars == l.vars && or.vars == r.vars
      case _ => false
    }

    override def hashCode(): Int = {
      val prime = 31
      var result = 1
      result = result * prime + l.hashCode()
      result = result * prime + r.hashCode()
      result
    }
  }
  object CImpl {
    def apply(l: CConj, r: CConj) = new CImpl(l, r)

    def unapply(imp: CImpl): Option[(CConj, CConj)] = Some((imp.l, imp.r))
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

//    val order = List(a,b,c,d,g,p,n)
    val order = List(a)

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


    def CImpltoProp(impl: CImpl): Prop = impl match {
      case CImpl(l, r) => l.asProp -> r.asProp
    }
    val result = iterate(base, order).foldLeft[Prop](Const(false)){
      case (acc, cimpl) => acc OR CImpltoProp(cimpl)
    }

//    val expected = Set (
//      t -> r,
//      r -> t,
//      Const( true ) -> (r AND t),
//      r -> Const( true ),
//      t -> Const( true ),
//      Const( true ) -> t,
//      Const( true ) -> r
//    ).foldLeft[Prop](Const(false)){
//      case (acc, prop) => acc OR prop
//    }
    
    val expected = Set (
      (g) -> (c),
      (d AND t) -> (n),
      (b AND c) -> (r AND t),
      (n) -> (c AND d),
      (d AND r) -> (n),
      (r) -> (b AND c AND g AND p AND t),
      (b AND c AND d AND g) -> (n),
      (c AND g) -> (p AND r AND t),
      (t) -> (b AND c AND g AND p AND r),
      Const(true) -> (g AND t),
      Const(true) -> (g AND r),
      (d) -> (c),
      (b AND c AND g) -> (p AND r AND t)
    ).foldLeft[Prop](Const(false)){
            case (acc, prop) => acc OR prop
          }


    if (expected.equivalent(result))
      println("TODO OK")
    else
      println("JODETE")




  }
}
