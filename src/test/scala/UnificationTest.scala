import munit.FunSuite


class UnificationTest extends FunSuite:
  import Unification.*

  test("basic") {
    assert((Expr(Var("x"), Var("y")) unify
            Expr(Var("x'"), StringLiteral("a")))
      .contains(Knowledge(Map("x" -> Var("x'"), "y" -> StringLiteral("a")))))
    assert((Expr(Var("x"), StringLiteral("a"), Var("x")) unify
            Expr(Expr(StringLiteral("a"), StringLiteral("b")), Var("y"), Expr(Var("y"), StringLiteral("b"))))
      .contains(Knowledge(Map("x" -> Expr(Var("y"), StringLiteral("b")), "y" -> StringLiteral("a")))))
    assert((Expr(StringLiteral("A"), Expr(StringLiteral("B"), Var("v")), Expr(StringLiteral("C"), Var("u"), Var("v"))) unify
            Expr(StringLiteral("A"), Expr(StringLiteral("B"), Var("w")), Expr(StringLiteral("C"), Var("w"), Expr(StringLiteral("f"), Var("x"), Var("y")))))
      .contains(Knowledge(Map("v" -> Expr(StringLiteral("f"), Var("x"), Var("y")), "u" -> Var("w")))))
  }
end UnificationTest