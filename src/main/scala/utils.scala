export Unification.unify


def disjoint(t1: Term, t2: Term): Boolean =
  (t1 unify t2).isEmpty

def insensitive(t: Term, s: Space): Boolean =
  s.ts.forall{ case Expr(Vector(`===`, t_, _)) => disjoint(t, t_); case _ => true }

def substitute(t: Term, k: Knowledge): Term = t match
  case Expr(ts) => Expr(ts.map(substitute(_, k)))
  case Var(name) => k.lookup(name).fold(t)(substitute(_, k))
  case _ => t

def holes(t: Term): Set[Term] = Set(Context.HOLE) union (t match
  case Expr(ts) => for (t, j) <- ts.zipWithIndex.toSet; t_ <- holes(t) yield
    Expr(for (ot, i) <- ts.zipWithIndex yield if i == j then t_ else ot)
  case _ => Set())

def execute(initial: State, rules: Seq[RewriteRule]): State =
  var state = initial
  while true do
    rules.find(_.isDefinedAt(state)) match
      case Some(rule) => state = rule(state)
      case None => return state
  throw IllegalStateException()

extension (s: Space)
  def ++ (os: Space): Space = Space(s.ts concat os.ts)

  def partition(p: Term => Boolean): (Space, Space) =
    val (l, r) = s.ts.partition(p)
    (Space(l), Space(r))

  def partitionFirst(p: Term => Boolean): (Option[Term], Space) =
    s.ts.find(p) match
      case Some(v) => (Some(v), Space(s.ts.filter(_ != v)))
      case None => (None, s)