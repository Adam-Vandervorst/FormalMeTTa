abstract class Context:
  def apply(t: Term): Term
  def unapply(t: Term): Option[Term]


object Context:
  def fromTerm(lens: Term): Context = new Context:
    override def apply(t: Term): Term =
      substitute(lens, Knowledge.empty.modBind("HOLE", t))

    override def unapply(t: Term): Option[Term] =
      Unification.unify(lens)(t).flatMap(_.lookup("HOLE"))

  val HOLE: Var = Var("HOLE")


case class Query(K: Context) extends RewriteRule:
  def isDefinedAt(x: State): Boolean =
    val State(i, k, w, o) = x
    i.ts.exists{ case K(t_) => !insensitive(t_, k); case _ => false }

  def apply(x: State): State =
    val State(i, k, w, o) = x

    val (Some(K(t_)), i_) = i.partitionFirst{ case K(t_) => !insensitive(t_, k); case _ => false }
    val applied = new Space(k.ts.collect{
      case Expr(Vector(`===`, ti, ui)) if !disjoint(t_, ti) =>
        K(substitute(ui, (t_ unify ti).get))
    })
    State(i_, k, applied ++ w, o)


case class Chain(K: Context) extends RewriteRule:
  def isDefinedAt(x: State): Boolean =
    val State(i, k, w, o) = x
    w.ts.exists{ case K(u) => !insensitive(u, k); case _ => false }


  def apply(x: State): State =
    val State(i, k, w, o) = x

    val (Some(K(u)), w_) = w.partitionFirst{ case K(u) => !insensitive(u, k); case _ => false }
    val applied = new Space(k.ts.collect {
      case Expr(Vector(`===`, ti, ui)) if !disjoint(u, ti) =>
        K(substitute(ui, (u unify ti).get))
    })
    State(i, k, applied ++ w_, o)

/*
case object TRANSFORM extends RewriteRule:
  def isDefinedAt(x: State): Boolean =
    // sigma_i = unify(t, t_i)
    // k = S(t1, ..., tn) ++ k'
    // insensitive(t, k')
    val State(i, k, w, o) = x

    i.ts.exists{
      case Expr(Vector(`transform`, t, u)) =>
        k.ts.exists(ti => !disjoint(t, ti))
      case _ => false
    }

  def apply(x: State): State =
    // State({(transform t y)} ++ i, k, w, o) -->
    // State(i, k, {u_1 sigma_1} ++ ... ++ {u_1 sigma_1} ++ w, o)
    val State(i, k, w, o) = x

    val (Some(Expr(Vector(_, t, u))), i_) = i.partitionFirst{
      case Expr(Vector(`transform`, t, u)) =>
        k.ts.exists(ti => !disjoint(t, ti))
      case _ => false
    }
    val applied = new Space(k.ts.collect {
      case ti if !disjoint(t, ti) =>
        substitute(u, (t unify ti).get)
    })
    State(i_, k, applied ++ w, o)


case object ADDATOM1 extends RewriteRule:
  def isDefinedAt(x: State): Boolean =
    // -
    val State(i, k, w, o) = x
    i.ts.exists{
      case Expr(Vector(`addAtom`, _)) => true
      case _ => false
    }

  def apply(x: State): State =
    // State({(addAtom t)} ++ i, k, w, o) -->
    // State(i, {t} ++ k, w, {()} ++ o)
    val State(i, k, w, o) = x

    val (Some(Expr(Vector(_, t))), i_) = i.partitionFirst{
      case Expr(Vector(`addAtom`, _)) => true
      case _ => false
    }
    State(i_, k, Space(t) ++ w, Space(??? : Term) ++ o)


case object BOOLMUL1 extends RewriteRule:
  def isDefinedAt(x: State): Boolean =
    // -
    val State(i, k, w, o) = x
    i.ts.exists {
      case Expr(Vector(`Mul`, _: BoolLiteral, _: BoolLiteral)) => true
      case _ => false
    }

  def apply(x: State): State =
    // State({(Mul b1 b2)} ++ i, k, w, o) -->
    // State(i, k, w, {b1 & b2} ++ o)
    val State(i, k, w, o) = x

    val (Some(Expr(Vector(_, BoolLiteral(b1), BoolLiteral(b2)))), i_) = i.partitionFirst {
      case Expr(Vector(`Mul`, _: BoolLiteral, _: BoolLiteral)) => true
      case _ => false
    }
    State(i_, k, w, Space(BoolLiteral(b1 & b2)) ++ o)


case object BOOLMUL2 extends RewriteRule:
  def isDefinedAt(x: State): Boolean =
    // w = {(Mul b1 b2)} ++ w'
    val State(i, k, w, o) = x
    w.ts.exists {
      case Expr(Vector(`Mul`, _: BoolLiteral, _: BoolLiteral)) => true
      case _ => false
    }

  def apply(x: State): State =
    // State(i, k, w, o) -->
    // State(i, k, w', {b1 & b2} ++ o)
    val State(i, k, w, o) = x

    val (Some(Expr(Vector(_, BoolLiteral(b1), BoolLiteral(b2)))), w_) = w.partitionFirst {
      case Expr(Vector(`Mul`, _: BoolLiteral, _: BoolLiteral)) => true
      case _ => false
    }
    State(i, k, w_, Space(BoolLiteral(b1 & b2)) ++ o)


case object DOUBLEMUL1 extends RewriteRule:
  def isDefinedAt(x: State): Boolean =
    // -
    val State(i, k, w, o) = x
    i.ts.exists {
      case Expr(Vector(`Mul`, _: DoubleLiteral, _: DoubleLiteral)) => true
      case _ => false
    }

  def apply(x: State): State =
    // State({(Mul d1 d2)} ++ i, k, w, o) -->
    // State(i, k, w, {d1 * d2} ++ o)
    val State(i, k, w, o) = x

    val (Some(Expr(Vector(_, DoubleLiteral(d1), DoubleLiteral(d2)))), i_) = i.partitionFirst {
      case Expr(Vector(`Mul`, _: DoubleLiteral, _: DoubleLiteral)) => true
      case _ => false
    }
    State(i_, k, w, Space(DoubleLiteral(d1 * d2)) ++ o)


case object DOUBLEMUL2 extends RewriteRule:
  def isDefinedAt(x: State): Boolean =
    // w = {(Mul b1 b2)} ++ w'
    val State(i, k, w, o) = x
    w.ts.exists {
      case Expr(Vector(`Mul`, _: DoubleLiteral, _: DoubleLiteral)) => true
      case _ => false
    }

  def apply(x: State): State =
    // State(i, k, w, o) -->
    // State(i, k, w', {b1 * b2} ++ o)
    val State(i, k, w, o) = x

    val (Some(Expr(Vector(_, DoubleLiteral(d1), DoubleLiteral(d2)))), w_) = w.partitionFirst {
      case Expr(Vector(`Mul`, _: DoubleLiteral, _: DoubleLiteral)) => true
      case _ => false
    }
    State(i, k, w_, Space(DoubleLiteral(d1 * d2)) ++ o)


case object OUTPUT extends RewriteRule:
  def isDefinedAt(x: State): Boolean =
    // insensitive(u, k)
    val State(i, k, w, o) = x
    w.ts.exists(u => insensitive(u, k))

  def apply(x: State): State =
    // State(i, k, {u} ++ w, o) -->
    // State(i, k, w, {u} ++ o)
    val State(i, k, w, o) = x

    val (Some(u), w_) = w.partitionFirst(u => insensitive(u, k))
    State(i, k, w_, Space(u) ++ o)
*/
