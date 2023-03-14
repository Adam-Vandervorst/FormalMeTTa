import java.net.URI

import scala.collection.MultiSet


case class Space(ts: MultiSet[Term])
object Space { def apply(t: Term, ts: Term*) = new Space(MultiSet.from(t +: ts)) }

sealed trait Term

case class Expr(ts: Vector[Term]) extends Term
object Expr:
  def apply(t: Term, ts: Term*): Expr = new Expr(t +: ts.toVector)

sealed trait Atom extends Term

case class Var(name: String) extends Atom

sealed trait Builtin extends Atom
case object transform extends Builtin
case object === extends Builtin
case object addAtom extends Builtin
case object remAtom extends Builtin

sealed trait Ground extends Atom
case class BoolLiteral(value: Boolean) extends Ground
case class LongLiteral(value: Long) extends Ground
case class StringLiteral(value: String) extends Ground
case class URILiteral(value: URI) extends Ground
