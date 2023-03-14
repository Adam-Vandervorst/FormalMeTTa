case class State(i: Space, k: Space, w: Space, o: Space)

sealed trait RewriteRule extends PartialFunction[State, State]


case object QUERY extends RewriteRule:
  def isDefinedAt(x: State): Boolean =
    // sigma_i = unify(t', t_i)
    // k = S(L(===, t_1, u_1), ..., L(===, t_n, u_n)) ++ k'
    // insensitive(t', k')
    val State(i, k, w, o) = x
    i.ts.exists(t_ => !insensitive(t_, k))

  def apply(x: State): State =
    // State({t'} ++ i, k, w, o) -->
    // State(i, k, {u_1 sigma_1} ++ ... ++ {u_1 sigma_1} ++ w, o)
    val State(i, k, w, o) = x

    val (Some(t_), i_) = i.partitionFirst(!insensitive(_, k))
    val applied = new Space(k.ts.collect{
      case Expr(Vector(`===`, ti, ui)) if !disjoint(t_, ti) =>
        substitute(ui, (t_ unify ti).get)
    })
    State(i_, k, applied ++ w, o)


case object CHAIN extends RewriteRule:
  def isDefinedAt(x: State): Boolean =
    // sigma_i = unify(u, t_i)
    // k = S(L(===, t_1, u_1), ..., L(===, t_n, u_n)) ++ k'
    // insensitive(u, k')
    val State(i, k, w, o) = x
    w.ts.exists(u => !insensitive(u, k))


  def apply(x: State): State =
    // State(i, k, {u} ++ w, o) -->
    // State(i, k, {u_1 sigma_1} ++ ... ++ {u_1 sigma_1} ++ w, o)
    val State(i, k, w, o) = x

    val (Some(u), w_) = w.partitionFirst(u => !insensitive(u, k))
    val applied = new Space(k.ts.collect {
      case Expr(Vector(`===`, ti, ui)) if !disjoint(u, ti) =>
        substitute(ui, (u unify ti).get)
    })
    State(i, k, applied ++ w_, o)


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
    // State(i, {t} ++ k, w, o)
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
    // State({(+ b1 b2)} ++ i, k, w, o) -->
    // State(i, k, w, {b1 | b2} ++ o)
    val State(i, k, w, o) = x

    val (Some(Expr(Vector(_, BoolLiteral(b1), BoolLiteral(b2)))), i_) = i.partitionFirst {
      case Expr(Vector(`Mul`, _: BoolLiteral, _: BoolLiteral)) => true
      case _ => false
    }
    State(i_, k, w, Space(BoolLiteral(b1 & b2)) ++ o)


case object BOOLMUL2 extends RewriteRule:
  def isDefinedAt(x: State): Boolean =
    // w = {(* b1 b2)} ++ w'
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
