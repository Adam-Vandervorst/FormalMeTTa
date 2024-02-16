// based on https://github.com/Adam-Vandervorst/PicoUnify

case class Knowledge(map: Map[String, Term]):
  /**
   * Modify or introduce a binding `v <- t`
   */
  def modBind(v: String, t: Term): Knowledge =
    copy(map.updated(v, t))

  /**
   * Recursively looks up the value of variable `v` (if any)
   */
  def lookup(v: String): Option[Term] =
    val r = map.get(v)
    if r.contains(Var(v)) then Some(Var(v))
    else r.flatMap(walk)

  /**
   * If `t` is a var or contains a var look it up recursively.
   */
  private def walk(t: Term): Option[Term] = t match
    case Expr(ts) => Some(Expr(ts.map(x => walk(x).getOrElse(x))))
    case Var(s) => lookup(s)
    case t: (Builtin | Ground | Symbol) => Some(t)

object Knowledge:
  def empty: Knowledge = Knowledge(Map.empty[String, Term])


object Unification:
  /**
   * Unify t1 with t2 assuming no knowledge.
   */
  extension (t1: Term)
    infix def unify(t2: Term): Option[Knowledge] =
      unifyWith(t1, t2, Knowledge.empty)

  /**
   * Unify t1 with t2 using/improving the passed knowledge.
   */
  def unifyWith(t1: Term, t2: Term, knowledge: Knowledge): Option[Knowledge] = (t1, t2) match
    case (Var(i), Var(j)) =>
      if i == j then Some(knowledge) // does global, variable equality
      else bind(i, t2, knowledge) // left-biased binding
    case (Var(i), _) =>
      bind(i, t2, knowledge)
    case (_, Var(j)) =>
      bind(j, t1, knowledge)
    case (i: (Builtin | Ground | Symbol), j: (Builtin | Ground | Symbol)) =>
      if i == j then Some(knowledge)
      else None
    case (Expr(ls), Expr(rs)) if ls.length == rs.length =>
      (ls zip rs).foldLeft(Option(knowledge)){
        case (mk, (l, r)) => mk.flatMap(k => unifyWith(l, r, k))
      }
    case _ => None
  end unifyWith

  /**
   * Trying to unify `$x` with `Expr($x, b, c)` should fail as this is equivalent to infinite regress.
   */
  def bind(v: String, t: Term, knowledge: Knowledge): Option[Knowledge] =
    if occursCheck(v, knowledge, t) then
      None
    else
      knowledge.lookup(v) match
        case None =>
          Some(knowledge.modBind(v, t))
        case Some(otherT) =>
          unifyWith(t, otherT, knowledge).map(_.modBind(v, t))
  end bind

  /**
   * Recursively checks if `v` occurs in `t` using the supplied knowledge.
   */
  def occursCheck(v: String, knowledge: Knowledge, t: Term): Boolean =
    def reachlist(l: Vector[String]): Vector[String] =
      l ++ l.flatMap(reachable)
    def reachable(v: String): Vector[String] =
      reachlist(knowledge.lookup(v).fold(Vector.empty)(vars))

    // v may be aliased to some other variable
    val aliased = knowledge.lookup(v) match
      case Some(Var(i)) => i
      case _ => v

    reachlist(vars(t)).contains(aliased)
  end occursCheck

  private def vars(t: Term): Vector[String] = t match
    case Expr(ts) => ts.flatMap(vars)
    case Var(i) => Vector(i)
    case _ => Vector.empty
end Unification
