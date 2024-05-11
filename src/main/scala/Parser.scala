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
    def parse(input: String, params: Map[Char, Boolean]): Try[Boolean] = {
        val tokens = parse_str(input) 

        val res = generate_tree(tokens.iterator)

        Try(dfs(res, params)) 
    }

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

        assert(nodeSt.size == 1)

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
        var hasExpr = false
        
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
                                } else if (!hasExpr) {
                                    val cVal = m.get(p.letter).get
                                    if (currProp.get.negated) {
                                        cVal != cVal
                                    }
                                    currOp match
                                        case None => throw new RuntimeException("Invalid proposition input") // invalid proposition in that case
                                        case Some(value) => value match
                                            case OperatorType.And =>
                                                currValue = currValue && cVal
                                            case OperatorType.Or =>
                                                currValue = currValue || cVal
                                            case OperatorType.Negation => throw new RuntimeException("Invalid proposition input") // invalid proposition in that case
                                } else {
                                    hasExpr = false
                                    var v = m.get(p.letter).get
                                    if (p.negated) {
                                        v != v
                                    }

                                    currOp match
                                        case None => throw new RuntimeException("Invalid proposition input") // invalid proposition in that case
                                        case Some(value) => value match
                                            case OperatorType.And => 
                                                currValue = currValue && v
                                            case OperatorType.Or =>
                                                currValue = currValue || v
                                            case OperatorType.Negation => throw new RuntimeException("Invalid proposition input") // invalid proposition in that case
                                    
                                    
                                    currProp = None
                                }
                            }
                            case Some(_) => 
                                currOp match {
                                    case None => throw new RuntimeException("Invalid proposition input") // invalid proposition in that case
                                    case Some(value2) =>
                                        val v1 = currValue
                                        val v2 = m.get(p.letter).get

                                        value2 match
                                            case OperatorType.And => {
                                                currOp = None
                                                currProp = None
                                                currValue = v1 && v2
                                            }
                                            case OperatorType.Or => {
                                                currOp = None
                                                currProp = None
                                                currValue = v1 || v2
                                            }
                                            case OperatorType.Negation => throw new RuntimeException("invalid proposition input")
                                }
                            //
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
                            }
                            if (hasExpr || currOp.isDefined) {
                                val cv = dfs(en, m)
                                hasExpr = false
                                currOp match
                                    case None => throw new RuntimeException("invalid prop")
                                    case Some(value) => value match
                                        case OperatorType.And => currValue = currValue && cv
                                        case OperatorType.Or => currValue = currValue || cv
                                        case OperatorType.Negation => throw new RuntimeException("invalid prop")
                                currOp = None
                            }


                        case Some(_) => {
                            val cv = dfs(en, m)
                            currProp = None
                            hasExpr = false
                            currOp match
                                case None => throw new RuntimeException("invalid prop")
                                case Some(value) => value match
                                    case OperatorType.And => currValue = currValue && cv
                                    case OperatorType.Or => currValue = currValue || cv
                                    case OperatorType.Negation => throw new RuntimeException("invalid prop")
                        }
                    }
                }
            }
        }

        currValue 
    }
}

