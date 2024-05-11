import Tokenizer.parse_str
import scala.util.{Try, Success}
import scala.collection.mutable.Stack
import javax.swing.text.AbstractDocument.Content
import scala.sys.Prop
import scala.collection.mutable.Queue
import scala.annotation.tailrec

case class ParserState(
    var currProp: Option[Proposition] = None,
    var currOp: Option[OperatorType] = None,
    var hasNegated: Boolean = false,
    var flagHasFirst: Boolean = true,
    var currValue: Boolean = false) {

    def resetVariables(): Unit =
        currOp = None
        currProp = None

    def negate(): Unit =
        hasNegated != hasNegated

    def applyNegateToCurrent(): Unit =
        if (hasNegated) {
            hasNegated = false
            currValue != currValue
        }
}

sealed trait Parser {
    def parse(input:String, params: Map[Char, Boolean]): Try[Boolean]
}

case class ExprNode(var content: Seq[ExprNode|Token])

object SimpleParser extends Parser {
    def parse(input: String, params: Map[Char, Boolean]): Try[Boolean] =
        val tokens = parse_str(input) 

        val res = generate_tree(tokens.iterator)

        Try(dfs(res, params)) 

    /// here, we'll create the expression tree
    /* 
    What we want to achieve, is to create a tree in the form:
        - Each node represents a single unit of execution (anything inside parenthesis xd)
        - Each node may contain:
            - 0-level executions (meaning, direct operations happening inside the parenthesis)
            - n-deep executions (other parenthesis inside them)
            - everytime we see a parenthesis, we'll recursively operate upon it

    To achieve that, in the parser we must translate from Tokens to an easier to traverse
    tree-like structure for this to traverse and then execute on-the-fly

     */
    private def generate_tree(input: Iterator[Token]): ExprNode =
        val nodeSt = Stack[ExprNode]()
        val resultStack = Queue[ExprNode|Token]()

        nodeSt.push(ExprNode(Seq()))
        while (input.hasNext) {
            input.next match {
                case Bracket.Open => {
                    if (!resultStack.isEmpty && !nodeSt.isEmpty) {
                        nodeSt.top.content = nodeSt.top.content.appendedAll(
                            resultStack.removeAll
                        )
                    }
                    nodeSt.push(ExprNode(Seq()))
                }
                case Bracket.Close => {
                    val currNode = nodeSt.pop()
                    currNode.content = currNode.content.appendedAll(resultStack.removeAll)
                    resultStack.enqueue(currNode)
                }
                case t: Token => 
                    resultStack.enqueue(t) 
            }
        }

        if (nodeSt.size != 1) {
            throw new RuntimeException("parsing error")
        }

        if (!resultStack.isEmpty) {
            nodeSt.top.content = nodeSt.top.content.appendedAll(resultStack.removeAll)
        }

        nodeSt.top
    
    
    private def dfs(root: ExprNode, m: Map[Char, Boolean]): Boolean =  {
        val cIter = root.content.iterator
        val parserState = ParserState()
        
        while (cIter.hasNext) {
            cIter.next match {
                case t: Token => t match
                    case Bracket.Close | Bracket.Open => throw new RuntimeException("Invalid proposition input")
                    case p: Proposition => 
                        parserState.currProp match
                            case None => {
                                parserState.currProp = Some(p)
                                if (parserState.hasNegated) {
                                    parserState.hasNegated = false
                                    parserState.currProp.get.negated = true
                                }
                                if (parserState.flagHasFirst) {
                                    parserState.currValue = m.get(p.letter).get
                                    if (parserState.currProp.get.negated) {
                                        parserState.currValue != parserState.currValue
                                    }
                                    parserState.flagHasFirst = false
                                } else {
                                    val cVal = m.get(p.letter).get
                                    if (parserState.currProp.get.negated) {
                                        cVal != cVal
                                    }

                                    parserState.currValue = applyOperator(parserState.currValue, cVal, parserState.currOp)
                                    parserState.resetVariables()
                                }
                            }
                            case Some(_) => 
                                parserState.currOp match {
                                    case None => throw new RuntimeException("Invalid proposition input")
                                    case Some(value2) =>
                                        val v2 = m.get(p.letter).get
                                        parserState.currValue = applyOperator(parserState.currValue, v2, parserState.currOp)
                                        parserState.resetVariables()
                                }
                    case Operator(ttype) =>
                        ttype match {
                            case OperatorType.Negation => parserState.negate()
                            case OperatorType.And => {
                                parserState.currOp = Some(OperatorType.And)
                            }
                            case OperatorType.Or => {
                                parserState.currOp = Some(OperatorType.Or)
                            }
                        }
                case en: ExprNode => {
                    parserState.currProp match {
                        case None =>
                            if (parserState.flagHasFirst) {
                                val cv = dfs(en, m)
                                parserState.currValue = cv
                                parserState.applyNegateToCurrent()
                                parserState.flagHasFirst = false
                            } else {
                                // since it has an operator defined but no
                                // proposition and we know it is not the first element
                                // we assume it as that there is implicitly
                                // an expression before or otherwise we throw an error
                                if (!parserState.currOp.isDefined) {
                                    throw new RuntimeException("invalid prop")
                                }
                                val cv = dfs(en, m)
                                parserState.currValue = applyOperator(parserState.currValue, cv, parserState.currOp)
                                parserState.resetVariables()
                            }

                        case Some(_) => {
                            val cv = dfs(en, m)
                            parserState.resetVariables()
                            parserState.currValue = applyOperator(parserState.currValue, cv, parserState.currOp)
                        }
                    }
                }
            }
        }
    
        parserState.currValue 
    }
    
    def applyOperator(currValue: Boolean, result: Boolean, op: Option[OperatorType]): Boolean = op match
        case None => throw new RuntimeException("invalid prop")
        case Some(op) => op match
            case OperatorType.And => currValue && result
            case OperatorType.Or => currValue || result
            case OperatorType.Negation => throw new RuntimeException("invalid prop")
}