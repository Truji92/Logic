package polynomial

import polynomial.Types.Var
import types.Clause._
import types.{Clause, PropCollectionOperations}
import types.Types._

import scala.collection.immutable.ListSet
import scala.collection.mutable
import scala.language.implicitConversions


object ImplicationRetractor {

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

    def without(vs: Set[Atom]) = CConj(vars -- vs)

    def symbols = vars

    override def toString = vars.mkString("",", ","")

    def ->(other: CConj) = CImpl(this, other)

    def toOtter =
      if (vars.isEmpty) " $T "
      else vars.mkString(" & ")

  }

  class CImpl(_l: CConj, _r: CConj) {

    val l = _l
    val r = CConj(_r.vars.diff(_l.vars))

    override def toString = l.toString +" => "+ r.toString

    def symbols = l.symbols ++ r.symbols

    def contains(v: Atom) = l.symbols.contains(v) || r.symbols.contains(v)

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

    def removeVar(v: Atom) = CImpl(l.without(v), r.without(v))

    def removeAll(vars: Set[Atom]) = CImpl(l.without(vars), r.without(vars))

    def toOtter = "((" + l.toOtter + ") -> (" + r.toOtter + "))."
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

  private def selfDelta(l: CImpl, v: Atom): ListSet[CImpl] = {
    if (!l.symbols.contains(v)) ListSet(l)
    else {
      val CImpl(left, right) = l
      if (left.symbols.contains(v)) ListSet.empty
      else ListSet(CImpl(left, right.without(v)))
    }
  }

  def resolvent(c1: CImpl, c2: CImpl, v: Atom): Set[CImpl] = {
    val CImpl(l1, r1) = c1
    val CImpl(l2, r2) = c2

    Set(CImpl(l1, r1.without(v)), CImpl(CConj(l1.symbols.union(l2.without(v).symbols)), r2))
  }

  case class TracedImpl(parents: (Int, Int), impl: CImpl) {

    override def equals(that: Any): Boolean = that match {
      case that: TracedImpl => impl == that.impl
      case _ => false
    }

    override def canEqual(that: Any): Boolean = that.isInstanceOf[TracedImpl]
    override def hashCode = impl.hashCode()
  }

  implicit def withoutTraces(items: ListSet[TracedImpl]): ListSet[CImpl] = items.map(_.impl)
  implicit def withoutTrace(item: TracedImpl): CImpl = item.impl

  type IndexedImpl = (TracedImpl, Int)
  implicit def toIndexed(items: ListSet[TracedImpl]): ListSet[IndexedImpl] = items.zipWithIndex
  implicit def extractImpl(item: IndexedImpl): CImpl = item._1.impl


  def removeVarV1(impls: ListSet[IndexedImpl], v: Atom): ListSet[TracedImpl] = {
    var rest = impls
    val acc = ListSet.newBuilder[TracedImpl]

    while (rest.nonEmpty) {
      val (TracedImpl(_, h), id) = rest.last //en lugar de head::tail usamos init::last por razones de rendimiento de la coleccion
      val t = rest.init

      if (t.isEmpty) acc ++= selfDelta(h, v).map(item => TracedImpl((id, id), item))
      acc ++= rest.flatMap {
        case ((TracedImpl(_, item), id2)) => delta(h, item, v).map(res => TracedImpl((id, id2), res))
      }
      rest = t
    }
    acc.result
  }


  def removeVar(impls: ListSet[IndexedImpl], v: Atom): ListSet[TracedImpl] = {
    var rest = impls
    val acc = ListSet.newBuilder[TracedImpl]
    var ignorableVars = scala.collection.mutable.Set.empty[Atom]

    while (rest.nonEmpty) {
      val (TracedImpl(_, h), id) = rest.last //en lugar de head::tail usamos init::last por razones de rendimiento de la coleccion
      val t = rest.init

      val newImpls =
        if (t.isEmpty) selfDelta(h, v).map(item => TracedImpl((id, id), item))
        else rest.flatMap {
          case ((TracedImpl(_, item), id2)) => delta(h, item, v).map(res => TracedImpl((id, id2), res))
        }

      val (optimized, vars) = optimize(newImpls)
      ignorableVars ++= vars

      acc ++= optimized

      rest = t
    }

    optimizeIgnorableVarsWithoutNewOpt(acc.result(), ignorableVars.toSet)
//    optimizeIgnorableVars(acc.result(), ignorableVars)
  }

  /**
    * Elimina todas las fórmulas con consecuente vacío (ya que son ignorables) y devuelve las variables
    * presentes en fórmulas con el antecedente vacío (y las elimina) para que puedan eliminarse del resto de fórmulas.
    *
    * @param impls
    * @return
    */
  def optimize(impls: ListSet[TracedImpl]): (ListSet[TracedImpl], Set[Atom]) = {
    val (implbuilder, varBuilder) = impls.foldLeft((ListSet.newBuilder[TracedImpl], Set.newBuilder[Atom])) {
      case ((newImpls, vars), tImpl) =>
        val impl = tImpl.impl

        if (impl.l.vars.isEmpty) (newImpls, vars ++= impl.r.vars)
        else if (impl.r.vars.isEmpty) (newImpls, vars)
        else (newImpls += tImpl, vars)

    }
    (implbuilder.result, varBuilder.result)
  }

  def optimizeIgnorableVars(impls: ListSet[TracedImpl], ignorableVars: mutable.Set[Atom]) = {

    def recursiveOptimization(impls: ListSet[TracedImpl], vars: List[Atom]): ListSet[TracedImpl] = vars match {
      case Nil => impls
      case h::t =>
        val (optimized, newVars) = impls.foldLeft((ListSet.empty[TracedImpl], List.empty[Atom])) {
          case ((acc, varsAcc), TracedImpl(k, impl)) =>
            val newImpl = impl.removeVar(h)
            if (newImpl.l.vars.isEmpty ) (acc, varsAcc:::newImpl.r.vars.toList )
            else if (newImpl.r.vars.isEmpty) (acc, varsAcc)
            else (acc + TracedImpl(k, newImpl), varsAcc)
        }

        println("News = " + newVars.mkString(", "))

        val distinctVars = newVars.filterNot(t.contains(_))
        println("distinct = " + distinctVars.mkString(", "))

        recursiveOptimization(optimized, distinctVars:::t)
    }

    recursiveOptimization(impls, ignorableVars.toList)

  }

  def optimizeIgnorableVarsWithoutNewOpt(impls: ListSet[TracedImpl], ignorableVars: Set[Atom]) = {
    if (ignorableVars.isEmpty) impls
    else {
      impls.foldLeft(ListSet.empty[TracedImpl]) {
        case (acc, TracedImpl(k, impl)) =>
          val newImpl = impl.removeAll(ignorableVars)
          if (newImpl.r.vars.isEmpty) acc
          else acc + TracedImpl(k, newImpl)
      }
    }
  }

  implicit def setToCConj(vars: Set[Atom]): CConj = CConj(vars)

  sealed trait Version
  object V1 extends Version
  object V2 extends Version

  def run(input: ListSet[CImpl], vars: List[Atom], otter: Boolean, trace: Boolean, version: Version = V2) = {
    val base = input.zipWithIndex

    if (trace) {
      println("Inicial:")
      println(s"Tamaño ${base.size}")
      println(base.map{
        case (elem, index) => s"$index. \t $elem "
      }.mkString("\n","\n", "\n"))

      if (otter) {
        println("Otter")
        println(base.map{
          case (elem, index) => elem.toOtter
        }.mkString("\n", "\n", "\n"))
      }
      println("\n====================================\n")
    }

    def iterate(set: ListSet[IndexedImpl], vs: List[Atom]): ListSet[IndexedImpl] =
      if (vs.isEmpty) set
      else {
        val v::rest = vs
        val newSet = version match {
          case V1 => removeVarV1(set, v).zipWithIndex
          case V2 => removeVar(set, v).zipWithIndex
        }

        if (trace) {
          println(s"Eliminando $v")
          println(s"Tamaño ${newSet.size}")
          println(newSet.map{
            case (TracedImpl(parents, elem), index) => s"$index.  $parents \t $elem "
          }.mkString("\n","\n", "\n"))

          if (otter) {
            println("Otter")
            println(newSet.map{
              case (elem, index) => elem.toOtter
            }.mkString("\n", "\n", "\n"))
            println("\n====================================\n")
          }
        }

        iterate(newSet, rest)
      }

    val fakeTracedBase = base.map{case (item, index) => (TracedImpl((-1,-1), item), index)}

    iterate(fakeTracedBase, vars)
  }

}