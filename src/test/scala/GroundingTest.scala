import munit.FunSuite



class BoolTest extends FunSuite:
  test("simple pos 1") {
    val state1 = State(
      Space(StringLiteral("Input invariant 1"), Expr(Mul, BoolLiteral(false), BoolLiteral(true))),
      Space(StringLiteral("Knowledge invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Knowledge invariant 2"))),
      Space(StringLiteral("Work invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Work invariant 2"))),
      Space(StringLiteral("Output invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Output invariant 2"))),
    )

    val state1_output_option1 = State(
      Space(StringLiteral("Input invariant 1")),
      state1.k,
      state1.w,
      Space(StringLiteral("Output invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Output invariant 2")), BoolLiteral(false))
    )


    assert(BOOLMUL1.isDefinedAt(state1))
    assert(BOOLMUL1(state1) == state1_output_option1)
  }

  test("simple pos 2") {
    val state1 = State(
      Space(StringLiteral("Input invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Input invariant 2"))),
      Space(StringLiteral("Knowledge invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Knowledge invariant 2"))),
      Space(StringLiteral("Work invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Work invariant 2")), Expr(Mul, BoolLiteral(false), BoolLiteral(true))),
      Space(StringLiteral("Output invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Output invariant 2"))),
    )

    val state1_output_option1 = State(
      state1.i,
      state1.k,
      Space(StringLiteral("Work invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Work invariant 2"))),
      Space(StringLiteral("Output invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Output invariant 2")), BoolLiteral(false))
    )


    assert(BOOLMUL2.isDefinedAt(state1))
    assert(BOOLMUL2(state1) == state1_output_option1)
  }
end BoolTest


class DoubleTest extends FunSuite:
  test("simple pos 1") {
    val state1 = State(
      Space(StringLiteral("Input invariant 1"), Expr(Mul, DoubleLiteral(2.5), DoubleLiteral(10.0))),
      Space(StringLiteral("Knowledge invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Knowledge invariant 2"))),
      Space(StringLiteral("Work invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Work invariant 2"))),
      Space(StringLiteral("Output invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Output invariant 2"))),
    )

    val state1_output_option1 = State(
      Space(StringLiteral("Input invariant 1")),
      state1.k,
      state1.w,
      Space(StringLiteral("Output invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Output invariant 2")), DoubleLiteral(25.0))
    )


    assert(DOUBLEMUL1.isDefinedAt(state1))
    assert(DOUBLEMUL1(state1) == state1_output_option1)
  }

  test("simple pos 2") {
    val state1 = State(
      Space(StringLiteral("Input invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Input invariant 2"))),
      Space(StringLiteral("Knowledge invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Knowledge invariant 2"))),
      Space(StringLiteral("Work invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Work invariant 2")), Expr(Mul, DoubleLiteral(2.5), DoubleLiteral(10.0))),
      Space(StringLiteral("Output invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Output invariant 2"))),
    )

    val state1_output_option1 = State(
      state1.i,
      state1.k,
      Space(StringLiteral("Work invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Work invariant 2"))),
      Space(StringLiteral("Output invariant 1"), Expr(StringLiteral("Some"), StringLiteral("Output invariant 2")), DoubleLiteral(25.0))
    )


    assert(DOUBLEMUL2.isDefinedAt(state1))
    assert(DOUBLEMUL2(state1) == state1_output_option1)
  }
end DoubleTest
