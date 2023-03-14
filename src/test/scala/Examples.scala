import munit.FunSuite


class Nats extends FunSuite:
  test("basic") {
    val S = StringLiteral("Successor")
    val Z = StringLiteral("Zero")

    val initial = State(
      Space(Expr(StringLiteral("Add"), Expr.nest(S, S, S, Z), Expr.nest(S, Z))),
      Space(
        Expr(===, Expr(StringLiteral("Add"), Var("xz"), Z), Var("xz")),
        Expr(===, Expr(StringLiteral("Add"), Var("xs"), Expr(S, Var("ys"))), Expr(StringLiteral("Add"), Expr(S, Var("xs")), Var("ys"))),
      ),
      Space(),
      Space()
    )

    val resulting = State(
      Space(),
      initial.k,
      Space(),
      Space(Expr.nest(S, S, S, S, Z))
    )

     assert(execute(initial, ALL) == resulting)
  }


class FrogUnification extends FunSuite:
  test("basic") {
    val initial = State(
      Space(Expr(StringLiteral("if"), Expr(StringLiteral("croaks"), Var("x")), Var("x"))),
      Space(
        Expr(===, Expr(StringLiteral("if"), BoolLiteral(true), Var("then")), Var("then")),
        Expr(===, Expr(StringLiteral("frog"), Var("f")), Expr(Mul, Expr(StringLiteral("croaks"), Var("f")), Expr(StringLiteral("eat_flies"), Var("f")))),
        Expr(===, Expr(StringLiteral("croaks"), StringLiteral("Fritz")), BoolLiteral(true)),
        Expr(===, Expr(StringLiteral("eat_flies"), StringLiteral("Fritz")), BoolLiteral(true)),
        Expr(===, Expr(StringLiteral("green"), Var("g")), Expr(StringLiteral("frog"), Var("g")))
      ),
      Space(),
      Space()
    )

    // execute(initial, ALL)
  }
