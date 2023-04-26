import java.net.URI

import scala.collection.MultiSet


case class Space(ts: MultiSet[Term])
object Space:
  def apply(ts: Term*) = new Space(MultiSet.from(ts))

sealed trait Term

case class Expr(ts: Vector[Term]) extends Term
object Expr:
  def apply(t: Term, ts: Term*): Expr = new Expr(t +: ts.toVector)
  def nest(t: Term, ts: Term*): Term = (t +: ts).reduceRight((x, y) => Expr(Vector(x, y)))

sealed trait Atom extends Term

case class Var(name: String) extends Atom

case class Symbol(name: String) extends Atom

sealed trait Builtin extends Atom
case object transform extends Builtin
case object === extends Builtin
case object addAtom extends Builtin
case object remAtom extends Builtin

sealed trait Ground extends Atom
case object Mul extends Ground  // not in doc
case class BoolLiteral(value: Boolean) extends Ground
case class DoubleLiteral(value: Double) extends Ground
case class LongLiteral(value: Long) extends Ground
case class StringLiteral(value: String) extends Ground
