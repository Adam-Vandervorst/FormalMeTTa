import munit.FunSuite



class QueryTest extends FunSuite:
  test("simple pos") {
    val state1 = State(
      Space(StringLiteral("Input invariant 1"), Expr(StringLiteral("Some"), StringLiteral("To process 1"))),
      Space(StringLiteral("Knowledge invariant 1"), Expr(===, Expr(StringLiteral("Some"), Var("x")), Expr(StringLiteral("Simulated output"), Var("x")))),
      Space(StringLiteral("Work invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Work invariant 2"))),
      Space(StringLiteral("Output invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Output invariant 2"))),
    )

    val state1_output_option1 = State(
      Space(StringLiteral("Input invariant 1")),
      state1.k,
      Space(StringLiteral("Work invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Work invariant 2")), Expr(StringLiteral("Simulated output"), StringLiteral("To process 1"))),
      state1.o
    )


    assert(QueryRule.isDefinedAt(state1))
    assert(QueryRule(state1) == state1_output_option1)
  }

  test("context") {
    val QUERY = QueryKRule(TContext(Expr(StringLiteral("Wrapper"), TContext.HOLE)))

    val state1 = State(
      Space(StringLiteral("Input invariant 1"), Expr(StringLiteral("Wrapper"), Expr(StringLiteral("Some"), StringLiteral("To process 1")))),
      Space(StringLiteral("Knowledge invariant 1"), Expr(===, Expr(StringLiteral("Some"), Var("x")), Expr(StringLiteral("Simulated output"), Var("x")))),
      Space(StringLiteral("Work invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Work invariant 2"))),
      Space(StringLiteral("Output invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Output invariant 2"))),
    )

    val state1_output_option1 = State(
      Space(StringLiteral("Input invariant 1")),
      state1.k,
      Space(StringLiteral("Work invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Work invariant 2")),
        Expr(StringLiteral("Wrapper"), Expr(StringLiteral("Simulated output"), StringLiteral("To process 1")))),
      state1.o
    )


    assert(QUERY.isDefinedAt(state1))
    assert(QUERY(state1) == state1_output_option1)
  }
end QueryTest


class ChainTest extends FunSuite:
  test("simple pos") {
    val state1 = State(
      Space(StringLiteral("Input invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Input invariant 2"))),
      Space(StringLiteral("Knowledge invariant 1"), Expr(===, Expr(StringLiteral("Some"), Var("x")), Expr(StringLiteral("Simulated output"), Var("x")))),
      Space(StringLiteral("Work invariant 1"), Expr(StringLiteral("Some"), StringLiteral("To process 1"))),
      Space(StringLiteral("Output invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Output invariant 2"))),
    )

    val state1_output_option1 = State(
      state1.i,
      state1.k,
      Space(StringLiteral("Work invariant 1"), Expr(StringLiteral("Simulated output"), StringLiteral("To process 1"))),
      state1.o
    )


    assert(ChainRule.isDefinedAt(state1))
    assert(ChainRule(state1) == state1_output_option1)
  }

  test("auto redundant context") {
    val state1 = State(
      Space(StringLiteral("Input invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Input invariant 2"))),
      Space(StringLiteral("Knowledge invariant 1"), Expr(===, Expr(StringLiteral("Some"), Var("x")), Expr(StringLiteral("Simulated output"), Var("x")))),
      Space(StringLiteral("Work invariant 1"), Expr(StringLiteral("Some"), StringLiteral("To process 1"))),
      Space(StringLiteral("Output invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Output invariant 2"))),
    )

    val state1_output_option1 = State(
      state1.i,
      state1.k,
      Space(StringLiteral("Work invariant 1"), Expr(StringLiteral("Simulated output"), StringLiteral("To process 1"))),
      state1.o
    )

    for K <- state1.w.possibleContexts
        state <- ChainKRule(K).unapply(state1) do
      assert(state == state1_output_option1)
  }
end ChainTest


class TransformTest extends FunSuite:
  test("simple pos") {
    val state1 = State(
      Space(StringLiteral("Input invariant 1"), Expr(transform, Expr(StringLiteral("Some"), Var("x")), Expr(StringLiteral("Simulated output"), Var("x")))),
      Space(StringLiteral("Knowledge invariant 1"), Expr(StringLiteral("Some"), StringLiteral("To process 1"))),
      Space(StringLiteral("Work invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Work invariant 2"))),
      Space(StringLiteral("Output invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Output invariant 2"))),
    )

    val state1_output_option1 = State(
      Space(StringLiteral("Input invariant 1")),
      state1.k,
      Space(StringLiteral("Work invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Work invariant 2")), Expr(StringLiteral("Simulated output"), StringLiteral("To process 1"))),
      state1.o
    )

    assert(TransformRule.isDefinedAt(state1))
    assert(TransformRule(state1) == state1_output_option1)
  }
end TransformTest


class OutputTest extends FunSuite:
  test("simple pos") {
    val state1 = State(
      Space(StringLiteral("Input invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Input invariant 2"))),
      Space(StringLiteral("Knowledge invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Knowledge invariant 2"))),
      Space(StringLiteral("To output option 1"), Expr(StringLiteral("Some"), StringLiteral("To output option 2"))),
      Space(StringLiteral("Output invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Output invariant 2"))),
    )

    val state1_output_option1 = State(
      state1.i,
      state1.k,
      Space(Expr(StringLiteral("Some"), StringLiteral("To output option 2"))),
      Space(StringLiteral("Output invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Output invariant 2")), StringLiteral("To output option 1")),
    )

    val state1_output_option2 = State(
      state1.i,
      state1.k,
      Space(StringLiteral("To output option 1")),
      Space(StringLiteral("Output invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Output invariant 2")), Expr(StringLiteral("Some"), StringLiteral("To output option 2"))),
    )

    assert(OutputRule.isDefinedAt(state1))
    assert(OutputRule(state1) == state1_output_option1 || OutputRule(state1) == state1_output_option2)
  }

  test("simple neg") {
    val state1 = State(
      Space(StringLiteral("Input invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Input invariant 2"))),
      Space(StringLiteral("Knowledge invariant 1"), Expr(===, Expr(StringLiteral("Some"), Var("x")), StringLiteral("Simulated output"))),
      Space(Expr(StringLiteral("Some"), StringLiteral("To output option 2"))),
      Space(StringLiteral("Output invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Output invariant 2"))),
    )

    assert(!OutputRule.isDefinedAt(state1))
  }
end OutputTest
