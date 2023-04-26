import scala.collection.mutable
import scala.util.matching.Regex


class Tokenizer:
  private val tokens: mutable.ListBuffer[PartialFunction[String, Term]] = mutable.ListBuffer.empty

  def registerToken(constr: PartialFunction[String, Term]): Unit =
    tokens.prepend(constr)

  def translateToken(token: String): Option[Term] =
    tokens.find(_.isDefinedAt(token)).map(_(token))

object Tokenizer:
  val FormalMeTTa: Tokenizer = {
    val t = Tokenizer()

    t.registerToken{ case "transform" => transform }
    t.registerToken{ case "=" => === }
    t.registerToken{ case "addAtom" => addAtom }
    t.registerToken{ case "remAtom" => remAtom }

    t.registerToken{ case "*" => Mul }
    t.registerToken{ case d if d.toDoubleOption.isDefined => DoubleLiteral(d.toDouble) }
    t.registerToken{ case d if d.toLongOption.isDefined => LongLiteral(d.toLong) }
    t.registerToken{ case d if d.toBooleanOption.isDefined => BoolLiteral(d.toBoolean) }
    t.registerToken{ case d if d.head == '"' && d.last == '"' && d.length > 1 => StringLiteral(d.init.tail) }

    t
  }


class SExprParser(text: String):
  private val it = text.iterator.buffered

  def parse(tokenizer: Tokenizer): Option[Term] =
    while it.hasNext do
      it.head match
        case ';' =>
          skipLine()

        case c if c.isWhitespace =>
          it.next()

        case '$' =>
          it.next()
          val token = it.nextVar()
          return Some(Var(token))

        case '(' =>
          it.next()
          return Some(parseExpr(tokenizer))

        case ')' =>
          throw IllegalStateException("Unexpected right bracket")

        case _ =>
          return Some(parseAtom(tokenizer))
    end while
    None

  private def skipLine(): Unit =
    while it.hasNext && it.head != '\n' do
      it.next()

  private def parseAtom(tokenizer: Tokenizer): Term =
    val token = it.nextToken()
    tokenizer.translateToken(token).getOrElse(Symbol(token))

  private def parseExpr(tokenizer: Tokenizer): Term =
    val children = mutable.ListBuffer.empty[Term]

    while it.hasNext do
      it.head match
        case c if c.isWhitespace =>
          it.next()

        case ')' =>
          it.next()
          return Expr(children.toVector)

        case _ =>
          children += parse(tokenizer).getOrElse(throw IllegalStateException("Unexpected end of expression member"))
    end while
    throw IllegalStateException("Unexpected end of expression")
end SExprParser


extension (it: scala.collection.BufferedIterator[Char])
  def nextToken(): String =
    it.headOption match
      case Some('"') => nextString()
      case _ => nextWord()

  def nextString(): String =
    val token = StringBuilder()
    assert(it.next() == '"', "Double quote expected")
    token += '"'
    var break = false

    while it.hasNext && !break do
      val n = it.next()
      if n == '"' then
        token += '"'
        break = true
      else
        if n == '\\' && !it.hasNext then
          throw IllegalStateException("Escaping sequence is not finished")
        token += n
    token.toString()

  def nextWord(): String =
    val token = StringBuilder()
    while it.hasNext && !it.head.isWhitespace && it.head != '(' && it.head != ')' do
      token += it.next()
    token.toString()

  def nextVar(): String =
    val token = StringBuilder()
    while it.hasNext && !it.head.isWhitespace && it.head != '(' && it.head != ')' do
      if it.head == '#' then
        throw IllegalStateException("'#' char is reserved for internal usage")
      token += it.next()
    token.toString()
