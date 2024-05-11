import scala.util.Try
import scala.util.Success
sealed trait Token

enum OperatorType:
  case Or, And, Negation


enum Bracket extends Token:
    case Close, Open

case class Proposition(val letter: Char, var negated: Boolean) extends Token

case class Operator(val ttype: OperatorType) extends Token

object Tokenizer {
    def parse_str(s: String): Array[Token] =
        s
        .toCharArray 
        .filter(validChars)
        .map(_.toChar)
        .map {
            case 'v' => Operator(OperatorType.Or)
            case '^' => Operator(OperatorType.And)
            case '~' => Operator(OperatorType.Negation)
            case ')' => Bracket.Close
            case '(' => Bracket.Open
            case c => Proposition(c, false)
        }

    private def validChars(c: Int) = c match
        case c if c <= 'z' && c >= 'a' => true
        case c if Seq('v', '^', '~', ')', '(').contains(c) => true
        case _ => false 
}
