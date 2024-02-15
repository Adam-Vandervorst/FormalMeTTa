export Unification.unify


def disjoint(t1: Term, t2: Term): Boolean =
  (t1 unify t2).isEmpty

def insensitive(t: Term, s: Space): Boolean =
  s.ts.forall{ case Expr(Vector(`===`, t_, _)) => disjoint(t, t_); case _ => true }

def substitute(t: Term, k: Knowledge): Term = t match
  case Expr(ts) => Expr(ts.map(substitute(_, k)))
  case Var(name) => k.lookup(name).fold(t)(substitute(_, k))
  case _ => t

def holes(t: Term): Set[Term] = Set(TContext.HOLE) union (t match
  case Expr(ts) => for (t, j) <- ts.zipWithIndex.toSet; t_ <- holes(t) yield
    Expr(for (ot, i) <- ts.zipWithIndex yield if i == j then t_ else ot)
  case _ => Set())

def execute(initial: State, rules: Iterable[RewriteRule]): State =
  var state = initial
  while true do
    rules.find(_.isDefinedAt(state)) match
      case Some(rule) => state = rule(state)
      case None => return state
  throw IllegalStateException()

def executeWithContext(initial: State, contextRules: State => Iterable[RewriteRule], debug: Boolean = false, limit: Long = 10000): State =
  var state = initial
  if debug then
    println("initial state")
    println(state.overview)
  var i = 1
  while i < limit do
    contextRules(state).find(_.isDefinedAt(state)) match
      case Some(rule) => 
        state = rule(state)
        if debug then
          println(f"--> $rule")
          println(state.overview)
        i += 1
      case None => return state
  throw RuntimeException(f"Reached $limit Rule Applications")

extension (s: Space)
  def ++ (os: Space): Space = Space(s.ts concat os.ts)

  def partition(p: Term => Boolean): (Space, Space) =
    val (l, r) = s.ts.partition(p)
    (Space(l), Space(r))

  def partitionFirst(p: Term => Boolean): (Option[Term], Space) =
    s.ts.find(p) match
      case Some(v) => (Some(v), Space(s.ts.filter(_ != v)))
      case None => (None, s)

  def possibleContexts: Set[TContext] =
    for t <- s.ts.toSet; lens <- holes(t)
      yield TContext(lens)

  def view: String =
    if s.ts.isEmpty then "/\n"
    else s.ts.map(_.pretty).mkString("\n", "\n", "\n")

extension (t: Term)
  def pretty: String = t match
    case Expr(ts) => ts.map(_.pretty).mkString("(", " ", ")")
    case Var(name) => s"$$$name"
    case Symbol(name) => name

    case `transform` => "transform"
    case `===` => "="
    case `addAtom` => "addAtom"
    case `remAtom` => "remAtom"

    case Mul => "*"
    case BoolLiteral(value) => value.toString
    case DoubleLiteral(value) => value.toString + "D"
    case LongLiteral(value) => value.toString + "L" 
    case StringLiteral(value) => s"\"$value\"" 

extension (s: State)
  def overview: String =
    "input: " + s.i.view +
    "knowledge: " + s.k.view +
    "working: " + s.w.view +
    "output: " + s.o.view
