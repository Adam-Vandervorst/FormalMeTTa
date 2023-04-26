import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.matching.Regex.Match


case class TokenDescr(regex: Regex, constr: AtomConstr)

@FunctionalInterface
trait AtomConstr:
  def apply(s: String): Term

class Tokenizer:
  private val tokens: mutable.ListBuffer[TokenDescr] = mutable.ListBuffer.empty

  def registerToken(regex: Regex, constr: String => Term): Unit =
    tokens += TokenDescr(regex, constr(_))

  def findToken(token: String): Option[AtomConstr] =
    tokens.reverseIterator.find(_.regex.findFirstMatchIn(token) match
      case Some(m) => m.start == 0 && m.end == token.length
      case None => false
    ).map(_.constr)


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
          return parseAtom(tokenizer)
    end while
    None

  private def skipLine(): Unit =
    while it.hasNext && it.head != '\n' do
      it.next()

  private def parseAtom(tokenizer: Tokenizer): Option[Term] =
    val token = it.nextToken()
    tokenizer.findToken(token) match
      case Some(constr) => Some(constr(token))
      case None => Some(Symbol(token))

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
