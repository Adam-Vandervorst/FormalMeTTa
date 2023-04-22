import munit.FunSuite



class UtilTest extends FunSuite:
  test("holes") {
    import Context.HOLE
    assertEquals(holes(Expr(StringLiteral("a"), StringLiteral("b"))), Set(
      Expr(HOLE, StringLiteral("b")),
      Expr(StringLiteral("a"), HOLE),
      HOLE
    ))
    assertEquals(holes(Expr(StringLiteral("p"), Expr(StringLiteral("a"), StringLiteral("b"), StringLiteral("c")))), Set(
      Expr(Vector(StringLiteral("p"), Expr(Vector(StringLiteral("a"), StringLiteral("b"), HOLE)))),
      Expr(Vector(StringLiteral("p"), Expr(Vector(HOLE, StringLiteral("b"), StringLiteral("c"))))),
      HOLE,
      Expr(Vector(StringLiteral("p"), Expr(Vector(StringLiteral("a"), HOLE, StringLiteral("c"))))),
      Expr(Vector(StringLiteral("p"), HOLE)),
      Expr(Vector(HOLE, Expr(Vector(StringLiteral("a"), StringLiteral("b"), StringLiteral("c"))))),
    ))
  }
