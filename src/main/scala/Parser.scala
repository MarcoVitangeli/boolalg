import Tokenizer.parse_str
import scala.util.{Try, Success}
import scala.collection.mutable.Stack
import javax.swing.text.AbstractDocument.Content
import scala.sys.Prop

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
        val resultStack = Stack[ExprNode|Token]()

        nodeSt.push(ExprNode(Seq()))
        while (input.hasNext) {
            input.next match {
                case Bracket.Open => {
                    if (!resultStack.isEmpty && !nodeSt.isEmpty) {
                        nodeSt.top.content = nodeSt.top.content.appendedAll(
                            resultStack.removeAll.reverse
                        )
                    }
                    nodeSt.push(ExprNode(Seq()))
                }
                case Bracket.Close => {
                    val currNode = nodeSt.pop()
                    currNode.content = currNode.content.appendedAll(resultStack.removeAll.reverse)
                    resultStack.push(currNode)
                }
                case t: Token => 
                    resultStack.push(t) 
            }
        }

        if (nodeSt.size != 1) {
            throw new RuntimeException("parsing error")
        }

        if (!resultStack.isEmpty) {
            nodeSt.top.content = nodeSt.top.content.appendedAll(resultStack.removeAll.reverse)
        }

        nodeSt.top
    
    private def dfs(root: ExprNode, m: Map[Char, Boolean]): Boolean =  {
        val cIter = root.content.iterator
        var currProp: Option[Proposition] = None
        var currOp: Option[OperatorType] = None
        var hasNegated = false
        var flagHasFirst = true
        var currValue = false
        
        while (cIter.hasNext) {
            cIter.next match {
                case t: Token => t match
                    case Bracket.Close | Bracket.Open => throw new RuntimeException("Invalid proposition input")
                    case p: Proposition => 
                        currProp match
                            case None => {
                                currProp = Some(p)
                                if (hasNegated) {
                                    hasNegated = false
                                    currProp.get.negated = true
                                }
                                if (flagHasFirst) {
                                    currValue = m.get(p.letter).get
                                    if (currProp.get.negated) {
                                        currValue != currValue
                                    }
                                    flagHasFirst = false
                                } else {
                                    val cVal = m.get(p.letter).get
                                    if (currProp.get.negated) {
                                        cVal != cVal
                                    }

                                    currValue = applyOperator(currValue, cVal, currOp)
                                    currProp = None
                                    currOp = None
                                }
                            }
                            case Some(_) => 
                                currOp match {
                                    case None => throw new RuntimeException("Invalid proposition input")
                                    case Some(value2) =>
                                        val v2 = m.get(p.letter).get
                                        currValue = applyOperator(currValue, v2, currOp)
                                        currOp = None
                                        currProp = None
                                }
                    case Operator(ttype) =>
                        ttype match {
                            case OperatorType.Negation => hasNegated = !hasNegated
                            case OperatorType.And => {
                                currOp = Some(OperatorType.And)
                            }
                            case OperatorType.Or => {
                                currOp = Some(OperatorType.Or)
                            }
                        }
                case en: ExprNode => {
                    currProp match {
                        case None =>
                            if (flagHasFirst) {
                                val cv = dfs(en, m)
                                currValue = cv
                                if (hasNegated) {
                                    hasNegated = false
                                    currValue != currValue
                                }
                                flagHasFirst = false
                            } else {
                                // since it has an operator defined but no
                                // proposition and we know it is not the first element
                                // we assume it as that there is implicitly
                                // an expression before or otherwise we throw an error
                                if (!currOp.isDefined) {
                                    throw new RuntimeException("invalid prop")
                                }
                                val cv = dfs(en, m)
                                currValue = applyOperator(currValue, cv, currOp)
                                currOp = None
                            }

                        case Some(_) => {
                            val cv = dfs(en, m)
                            currProp = None
                            currValue = applyOperator(currValue, cv, currOp)
                        }
                    }
                }
            }
        }
    
        currValue 
}

    def applyOperator(currValue: Boolean, result: Boolean, op: Option[OperatorType]): Boolean = op match
        case None => throw new RuntimeException("invalid prop")
        case Some(op) => op match
            case OperatorType.And => currValue && result
            case OperatorType.Or => currValue || result
            case OperatorType.Negation => throw new RuntimeException("invalid prop")
}