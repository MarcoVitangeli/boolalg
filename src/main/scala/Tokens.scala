import scala.util.Try
import scala.util.Success
import scala.collection.mutable.ArrayBuffer
sealed trait Token

enum OperatorType:
  case Or, And, Negation, Implication


enum Bracket extends Token:
    case Close, Open

case class Proposition(val name: String, var negated: Boolean) extends Token

case class Operator(val ttype: OperatorType) extends Token

object Tokenizer {
    val validSpecialChars = Seq('v', '^', '&', '~', ')', '(')

    def parse_str(s: String): Array[Token] =
        val buff = ArrayBuffer[Char]()
        val it = s
        .toCharArray 
        .filter(validChars)
        .map(_.toChar)
        .iterator

        val tBuff = ArrayBuffer[Token]()

        while (it.hasNext) {
            val curr = it.next
            if (validSpecialChars.contains(curr)) {
                if (buff.size > 0) {
                    val s = buff.mkString("")
                    buff.clear()
                    tBuff.addOne(Proposition(s, false))
                }

                val op = curr match
                    case 'v' => Operator(OperatorType.Or)
                    case '^' => Operator(OperatorType.And)
                    case '~' => Operator(OperatorType.Negation)
                    case '&' => Operator(OperatorType.Implication)
                    case ')' => Bracket.Close
                    case '(' => Bracket.Open
                    case x: Char => throw new RuntimeException("invalid char encountered" + x)

                tBuff.addOne(op)
            } else {
                if (buff.size == 30) {
                    throw new RuntimeException("invalid proposition name size")
                }
                buff.addOne(curr)
           }
        }

        if (buff.size > 0) {
            val s = buff.mkString("")
            buff.clear()
            tBuff.addOne(Proposition(s, false))
        }

        tBuff.toArray 

    private def validChars(c: Int) = c match
        case c if (c <= 'z' && c >= 'a') || (c <= 'Z' && c >= 'A') => true
        case c if validSpecialChars.contains(c) => true
        case _ => false 
}
