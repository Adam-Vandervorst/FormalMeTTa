import munit.FunSuite


class ParserTest extends FunSuite:
  def parseAtoms(program: String, tokenizer: Tokenizer = Tokenizer()): List[Term] =
    val parser = SExprParser(program)
    var result = List.empty[Term]
    var break = false
    while !break do
      parser.parse(tokenizer) match
        case Some(value) => result = result :+ value
        case None => break = true
    result

  test("test_text_var") {
    assertEquals(parseAtoms("$n"), List(Var("n")))
  }

  test("test_text_sym") {
    assertEquals(parseAtoms("test"), List(Symbol("test")))
  }

  test("test_text_quoted_string") {
    assertEquals(parseAtoms("\"te st\""), List(Symbol("\"te st\"")))
  }

  test("test_text_recognize_full_token") {
    val tokenizer = Tokenizer()
    tokenizer.registerToken{ case "b" => Symbol("Test") }

    val parser = SExprParser("ab")

    assertEquals(parser.parse(tokenizer), Some(Symbol("ab")))
    assertEquals(parser.parse(tokenizer), None)
  }

  test("test_text_gnd") {
    val tokenizer = Tokenizer()
    tokenizer.registerToken{ case l if l.toLongOption.isDefined => LongLiteral(l.toLong) }

    val parser = SExprParser("(3d 42)")

    assertEquals(parser.parse(tokenizer), Some(Expr(Symbol("3d"), LongLiteral(42))))
    assertEquals(parser.parse(tokenizer), None)
  }

  test("test_text_expr") {
    assertEquals(
      parseAtoms("(= (fac $n) (* $n (fac (- $n 1))))"),
      List(Expr(Symbol("="), Expr(Symbol("fac"), Var("n")), Expr(Symbol("*"), Var("n"), Expr(Symbol("fac"), Expr(Symbol("-"), Var("n"), Symbol("1"))))))
    )
  }

  test("test_text_few_expr") {
    assertEquals(parseAtoms("(a) (b)"), List(Expr(Symbol("a")), Expr(Symbol("b"))))
  }

  test("test_next_token") {
    val it = "n)".iterator.buffered

    assertEquals(it.nextToken(), "n")
    assertEquals(it.nextOption(), Some(')'))
  }

  test("test_panic_on_unbalanced_brackets") {
    val parser = SExprParser("(a))")
    intercept[IllegalStateException] {
      while (parser.parse(Tokenizer()).isDefined) {}
    }
  }

  test("test_comment_base") {
    val program = ";(a 4)\n      (b 5)"
    val expected = List(Expr(Symbol("b"), Symbol("5")))
    val res = parseAtoms(program)
    assertEquals(res, expected)
  }

  test("test_comment_in_sexpr") {
    val program = " (a ; 4)\n    5)"
    val expected = List(Expr(Symbol("a"), Symbol("5")))
    val res = parseAtoms(program)
    assertEquals(res, expected)
  }

  test("test_comment_endl") {
    val program = " (a 4);\n      (b 5)"
    val expected = List(Expr(Symbol("a"), Symbol("4")), Expr(Symbol("b"), Symbol("5")))
    val res = parseAtoms(program)
    assertEquals(res, expected)
  }

  test("test_panic_on_lattice_in_var_name") {
    val parser = SExprParser("$a#")
    intercept[IllegalStateException] {
      while (parser.parse(Tokenizer()).isDefined) {}
    }
  }

  test("override_token_definition") {
    val tokenizer = Tokenizer()
    tokenizer.registerToken{ case "A" => Symbol("A") }
    assertEquals(tokenizer.translateToken("A").get, Symbol("A"))
    tokenizer.registerToken{ case "A" => Symbol("B") }
    assertEquals(tokenizer.translateToken("A").get, Symbol("B"))
  }

  test("full") {
    val tokenizer = Tokenizer.FormalMeTTa
    val program =
      """
        |(= (TV $x)
        |   (transform
        |     (.tv (Implication $y $x)
        |          (stv $s $c))
        |     (stv (* $s (s-tv (TV $y)))
        |          (* $c (c-tv (TV $y))))
        |   )
        |)
        |
        |(.tv (Evaluation (Predicate P) (Concept A))
        |     (stv 0.5 0.8))
        |""".stripMargin

    assertEquals(parseAtoms(program, tokenizer),
      Expr(===, Expr(Symbol("TV"), Var("x")),
        Expr(transform,
          Expr(Symbol(".tv"), Expr(Symbol("Implication"), Var("y"), Var("x")),
            Expr(Symbol("stv"), Var("s"), Var("c"))),
          Expr(Symbol("stv"), Expr(Mul, Var("s"), Expr(Symbol("s-tv"), Expr(Symbol("TV"), Var("y")))),
            Expr(Mul, Var("c"), Expr(Symbol("c-tv"), Expr(Symbol("TV"), Var("y")))))
        )
      )::
      Expr(Symbol(".tv"), Expr(Symbol("Evaluation"), Expr(Symbol("Predicate"), Symbol("P")), Expr(Symbol("Concept"), Symbol("A"))),
        Expr(Symbol("stv"), DoubleLiteral(0.5), DoubleLiteral(0.8)))::Nil)

  }
end ParserTest
