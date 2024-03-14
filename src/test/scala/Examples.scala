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

class VitalyEq extends FunSuite:
  test("basic") {
    val ite = StringLiteral("if")
    val eq = StringLiteral("make-eq")
    val set = StringLiteral("set-value")
    val empty = StringLiteral("empty")

    val initial = State(
      Space(
        Expr(ite, Expr(eq, Var("x"), Var("y")),
          Expr(ite, Expr(set, Var("x")),
            Var("y"),
            empty
          ),
          empty
        )
      ),
      Space(
        Expr(===, Expr(eq, Var("a"), Var("a")), BoolLiteral(true)),

        Expr(===, Expr(ite, BoolLiteral(true), Var("tt"), Var("tf")), Var("tt")),
        Expr(===, Expr(ite, BoolLiteral(false), Var("ft"), Var("ff")), Var("ff")),

        Expr(===, Expr(set, LongLiteral(1)), BoolLiteral(true)),
        Expr(===, Expr(set, LongLiteral(2)), BoolLiteral(true)),
      ),
      Space(),
      Space()
    )
    
    assert(executeWithContext(initial, allInContext).o == Space(LongLiteral(1), LongLiteral(2)))
  }

class NilLambda extends FunSuite:
  test("small") {

    val initial = State(
      Space(parseAtoms("(($l $lx) $ly)"): _*),
      Space(parseAtoms("(= (((λ $x) $x) $y) $y)"): _*),
      Space(),
      Space()
    )

    assert(executeWithContext(initial, allInContext) == initial)
  }

  test("larger") {
    val initial = State(
      Space(parseAtoms("((λ $f) (($f $z) $g))"): _*),
      Space(parseAtoms("(= (((λ $x) $x) $y) $y)"): _*),
      Space(),
      Space()
    )

    assert(executeWithContext(initial, allInContext) == initial)
  }

class StackBased extends FunSuite:
  test("put") {
    val initial = State(
      Space(parseAtoms("(E (put 2))\n((E (put 2)) (put 3))"): _*),
      Space(parseAtoms("(= ($s (put $x)) ($s $x))"): _*),
      Space(),
      Space()
    )

    assert(executeWithContext(initial, allInContext).o == Space(parseAtoms("(E 2)\n((E 2) 3)"): _*))
  }


  test("apply3 malformed") {
    val initial = State(
      Space(parseAtoms("((((E (put 2)) (put 3)) (put (func ($as $ax) ($as (T $ax))))) apply3)"): _*),
      Space(parseAtoms("(= ($ps (put $px)) ($ps $px))\n(= (($fs (func $fs $fr)) apply3) $fr)"): _*),
      Space(),
      Space()
    )

//    println(executeWithContext(initial, allInContext).overview)
  }

  test("apply") {
    val initial = State(
      Space(parseAtoms("(((E 6.0) (Cons dup (Cons mul Nil))) apply)"): _*),
      Space(parseAtoms(
        "(= ((($s $x) $y) mul) ($s (* $x $y)))" +
        "(= (($s $x) dup) (($s $x) $x))" +
        "(= (($s (Cons $f $r)) apply) ((($s $f) $r) apply))" +
        "(= (($s Nil) apply) $s)"): _*),
      Space(),
      Space()
    )

    assert(executeWithContext(initial, allInContext).o == Space(Expr(Symbol("E"), DoubleLiteral(36.0))))
  }

class AuntKG extends FunSuite:
  test("github simple") {

    val kb = Space(parseAtoms("(parent Tom Bob)\n(parent Pam Bob)\n(parent Tom Liz)\n(parent Bob Ann)\n(parent Bob Pat)\n(parent Pat Jim)\n(female Pam)\n(male Tom)\n(male Bob)\n(female Liz)\n(female Pat)\n(female Ann)\n(male Jim)"): _*)

    val bob_parents = State(Space(parseAtoms("(transform (parent $p Bob) $p)"): _*), kb, Space(), Space())

    assert(executeWithContext(bob_parents, allInContext).o == Space(Symbol("Pam"), Symbol("Tom")))

    val bob_mother = State(Space(parseAtoms("(transform (parent $x Bob) (transform (female $x) $x))"): _*), kb, Space(), Space())

    assert(executeWithContext(bob_mother, allInContext).o == Space(Symbol("Pam")))

    val ann_sister = State(Space(parseAtoms("(transform (parent $x Ann) (transform (parent $x $y) (transform (female $y) $y)))"): _*), kb, Space(), Space())

    assert(executeWithContext(ann_sister, allInContext).o == Space(Symbol("Pat"), Symbol("Ann")))
    // Ann could be filtered by difference or by set intersection

    val jim_aunt = State(Space(parseAtoms("(transform (parent $p Jim) (transform (parent $gp $p) (transform (parent $gp $q) (transform (female $q) $q))))"): _*), kb, Space(), Space())

    assert(executeWithContext(jim_aunt, allInContext).o == Space(Symbol("Pat"), Symbol("Ann")))
    // Pat could be filtered by difference or by set intersection

    val ann_pred = State(Space(parseAtoms("(pred Ann)"): _*), kb ++ Space(parseAtoms("(= (pred $x1) (transform (parent $p1 $x1) $p1))\n(= (pred $x2) (transform (parent $p2 $x2) (pred $p2)))"): _*), Space(), Space())

    assert(executeWithContext(ann_pred, allInContext).o == Space(Symbol("Tom"), Symbol("Pam"), Symbol("Bob")))
  }

