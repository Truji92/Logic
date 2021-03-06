package types

import scala.language.{postfixOps, implicitConversions}

/**
  * Estructuras de datos básicas
  */
object Types {

  /**
    * Símbolo proposicional
    */
  type Symbol = String

  /**
    * Interpretación de una fórmula
    */
  type Interpretation = Map[Atom, Boolean]

  /**
    * Proposición
    */
  sealed trait Prop {

    override def toString = this match {
      case Const(bool: Boolean) => if (bool) "true" else "false"
      case Atom(symbol) => symbol
      case Neg(prop) => s"no($prop)"
      case Conj(p, q) => s"($p) AND ($q)"
      case Disj(p, q) => s"($p) OR ($q)"
      case Impl(p, q) => s"($p) => ($q)"
      case Equi(p, q) => s"($p) <=> ($q)"
    }

    def negate = this match {
      case Const(b) => Const(!b)
      case _ => Neg(this)
    }

    def AND(other: Prop) = Conj(this, other)

    def OR(other: Prop) = Disj(this, other)

    def ->(other: Prop) = Impl(this, other)

    def <->(other: Prop) = Equi(this, other)

    /**
      * Símbolos que componen una fórmula
      *
      * @return
      */
    def symbols: Set[Atom] = this match {
      case Const(_) => Set.empty
      case Atom(symbol) => Set(Atom(symbol))
      case Neg(prop) => prop.symbols
      case Conj(p, q) => p.symbols ++ q.symbols
      case Disj(p, q) => p.symbols ++ q.symbols
      case Impl(p, q) => p.symbols ++ q.symbols
      case Equi(p, q) => p.symbols ++ q.symbols
    }

    /**
      * Significado de una formula y un significado
      *
      * El valor de los Símbolos que no aparezcan en la interpretación se tomará como FALSE
      *
      * @param interp interpretación
      * @return
      */
    def meaning(interp: Interpretation): Boolean = this match {
        case Const(value) => value
        case Atom(symbol) => interp.getOrElse(Atom(symbol), false)
        case Neg(prop) =>  !prop.meaning(interp)
        case Conj(p, q) => p.meaning(interp) && q.meaning(interp)
        case Disj(p, q) => p.meaning(interp) || q.meaning(interp)
        case Impl(p, q) => (!(p meaning interp)) || (q meaning interp)
        case Equi(p, q) => {
          lazy val pm = p.meaning(interp)
          lazy val qm = q.meaning(interp)
          (!pm || qm) && (!qm || pm)
        }
      }


    def isModel(interpretation: Interpretation) = meaning(interpretation)

    /**
      * Todas las interpretaciones de una fórmula
      *
      * @return
      */
    def interpretations = symbols.subsets().map( interp => {
      interp.map(elem => (elem, true)).toMap
    })

    def models = interpretations.filter(isModel)

    def isValid = interpretations sameElements models

    def unSatisfiable = models.isEmpty

    def satisfiable = !unSatisfiable

    def equivalent(other: Prop) = (this <-> other) isValid

    def isLiteral = this match {
      case _: Atom => true
      case Neg(Atom(_)) => true
      case _ => false
    }

    /** Operations using clauses**/
    def isValidByClauses = Clause.isValid(Clause.fromProp(this))

  }

  sealed trait Literal {

    def symbols: Set[Atom]

    def isModel(interpretation: Interpretation): Boolean

    def complementary = this match {
      case l: Atom => NegL(l)
      case NegL(atom) => atom
    }

    def isPositive = this match {
      case _: Atom => true
      case _ => false
    }

    def isNegative = this match {
      case NegL(atom) => true
      case _ => false
    }

    def toProp: Prop = this match {
      case a: Atom => a
      case NegL(l) => Neg(l)
    }
  }

  case class NegL(atom: Atom) extends Literal {

    override def symbols: Set[Atom] = Set(atom)
    override def isModel(interpretation: Interpretation) =
      !interpretation.getOrElse(atom, false)
  }

  implicit def toLiteral(prop: Prop): Literal = prop match {
    case atom: Atom => atom
    case Neg(Atom(s)) => NegL(Atom(s))
    case _ => throw new Exception(s"$prop is not a Literal")
   }

  /**
    * Constante lógica
    *
    * @param boolean valor
    */
  case class Const(boolean: Boolean) extends Prop

  /**
    * Formula atómica
    *
    * @param symbol Símbolo que compone la formula
    */
  case class Atom(symbol: Symbol) extends Prop with Literal {
    //Overrides para evitar problemas con herencia multiple
    override def symbols = Set(this)
    override def isModel(interpretation: Interpretation) =
      interpretation.getOrElse(this, false)
  }

  /**
    * Fórmula de Negación
    *
    * @param prop proposición negada
    */
  case class Neg(prop: Prop) extends Prop

  /**
    * Conjunción
    *
    * @param left Fórmula izquierda de la conjunción
    * @param right Fórmula derecha de la conjunción
    */
  case class Conj(left: Prop, right: Prop) extends Prop

  /**
    * Disjunción
    *
    * @param left Fórmula izquierda de la disjunción
    * @param right Fórmula derecha de la disjunción
    */
  case class Disj(left: Prop, right: Prop) extends Prop

  /**
    * Implicación
    *
    * @param left Fórmula izquierda de la implicación
    * @param right Fórmula derecha de la implicación
    */
  case class Impl(left: Prop, right: Prop) extends Prop

  /**
    * Equivalencia
    *
    * @param left Fórmula izquierda de la equivalencia
    * @param right Fórmula derecha de la equivalencia
    */
  case class Equi(left: Prop, right: Prop) extends Prop

  /**
    * Conversión implicita de símbolos en formulas atómicas
    *
    * @param s Símbolo proposicional
    * @return fórmula Atom(s)
    */
  implicit def symbolToAtom(s: Symbol): Prop = {
    Atom(s)
  }

  /**
    * Negación de una Proposición
    *
    * @param prop proposición a negar
    * @return
    */
  def no(prop: Prop) = prop.negate

}
