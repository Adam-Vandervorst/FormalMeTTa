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

     assert(execute(initial, allRules) == resulting)
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

    val resulting = State(
      Space(),
      initial.k,
      Space(),
      Space(StringLiteral("Fritz"))
    )

    assertEquals(executeWithContext(initial, allInContext), resulting)
  }


class Socrates extends FunSuite:
  test("basic") {
    val TV = StringLiteral("TruthValue")
    val Human = StringLiteral("Human")

    val initial = State(
      Space(Expr(StringLiteral("And"), Expr(Human, StringLiteral("Socrates")), Expr(Human, StringLiteral("Sam")))),
      Space(
        Expr(===, Expr(Human, StringLiteral("Socrates")), Expr(TV, DoubleLiteral(0.9))),
        Expr(===, Expr(Human, StringLiteral("Sam")), Expr(TV, DoubleLiteral(0.7))),

        Expr(===, Expr(StringLiteral("And"), Expr(TV, Var("p1")), Expr(TV, Var("p2"))),
          Expr(TV, Expr(Mul, Var("p1"), Var("p2")))),
      ),
      Space(),
      Space()
    )

    lazy val resulting = State(
      Space(),
      initial.k,
      Space(),
      Space(Expr(TV, DoubleLiteral(0.9 * 0.7)))
    )

    assertEquals(executeWithContext(initial, allInContext), resulting)
  }
