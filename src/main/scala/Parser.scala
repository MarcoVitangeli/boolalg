import Tokenizer.parse_str
import scala.util.{Try, Success}
import scala.collection.mutable.Stack
import scala.collection.mutable.Queue

type ExprElement = ExprNode | Token

class ParserState(
    val valueSet: Map[String, Boolean],
    var currProp: Option[Proposition] = None,
    var currOp: Option[OperatorType] = None,
    var hasNegated: Boolean = false,
    var flagHasFirst: Boolean = true,
    var currValue: Boolean = false) {

    def resetVariables(): Unit =
        currOp = None
        currProp = None

    def negate(): Unit =
        hasNegated = !hasNegated

    def applyNegateToCurrent(): Unit =
        if (hasNegated) {
            hasNegated = false
            currValue = !currValue
        }

    def applyNegated(v: Boolean): Boolean = {
        if (hasNegated) {
            hasNegated = false
            !v
        } else {
            v
        }
    }
}

sealed trait Parser {
    def parse(input:String, params: Map[String, Boolean]): Try[Boolean]
}

case class ExprNode(var content: Seq[ExprElement])

object SimpleParser extends Parser {
    def parse(input: String, params: Map[String, Boolean]): Try[Boolean] =
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
        val resultStack = Queue[ExprElement]()

        nodeSt.push(ExprNode(Seq())) // backup node in case it does not start with parenthesis
        for (elem <- input) {
            elem match {
                case Bracket.Open =>
                    if (!resultStack.isEmpty && !nodeSt.isEmpty) {
                        nodeSt.top.content = nodeSt.top.content.appendedAll(
                            resultStack.removeAll
                        )
                    }
                    nodeSt.push(ExprNode(Seq()))

                case Bracket.Close =>
                    val currNode = nodeSt.pop()
                    currNode.content = currNode.content.appendedAll(resultStack.removeAll)
                    resultStack.enqueue(currNode)

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
    
    // dfs performs DFS (Depth-First-Search) over the expression tree and consumes
    // the tokens or expressions present
    private def dfs(root: ExprNode, valueSet: Map[String, Boolean]): Boolean =  {
        val cIter = root.content.iterator
        val parserState = ParserState(valueSet)

        for (node <- cIter) {
            if (parserState.flagHasFirst) {
                consumeFirst(node ,parserState)
            } else {
                consume(node, parserState)
            }
        }
    
        parserState.currValue 
    }
    
    private def applyOperator(currValue: Boolean, result: Boolean, op: Option[OperatorType]): Boolean = op match
        case None => throw new RuntimeException("invalid prop")
        case Some(op) => op match
            case OperatorType.And => currValue && result
            case OperatorType.Or => currValue || result
            case OperatorType.Implication => !currValue || result /// p => q is equivalent to ~p v q
            case OperatorType.Negation => throw new RuntimeException("invalid prop")

    // defines how to consume and set up state for the first element in the sequence
    private def consumeFirst(node: ExprElement, ps: ParserState) = node match
        case t: Token => t match
            case Bracket.Close | Bracket.Open => throw new RuntimeException("Invalid proposition input")
            case p: Proposition =>
                ps.currValue = ps.valueSet.get(p.name).get
                ps.applyNegateToCurrent() 
                ps.flagHasFirst = false
            case Operator(ttype) => ttype match
                case OperatorType.And | OperatorType.Or | OperatorType.Implication =>
                    ps.currOp = Some(ttype)
                    ps.flagHasFirst = false
                case OperatorType.Negation =>
                    ps.negate() // negate does not count in our model as the first operator
        case en: ExprNode =>
            val cv = dfs(en, ps.valueSet)
            ps.currValue = cv
            ps.applyNegateToCurrent()
            ps.flagHasFirst = false


    // defines how to consume and set up state for a random element in the sequence.
    // it makes optimizations based on the premise that some operation/operator happened before
    private def consume(node: ExprElement, ps: ParserState) = node match
        case t: Token => t match
            case Bracket.Close | Bracket.Open => throw new RuntimeException("Invalid proposition input")
            case p: Proposition => 
                ps.currProp match
                    case None =>
                        ps.currProp = Some(p)
                        val cVal = ps.applyNegated(ps.valueSet.get(p.name).get)
                        ps.currValue = applyOperator(ps.currValue, cVal, ps.currOp)
                        ps.resetVariables()
                    case Some(_) => 
                        ps.currOp match
                            case None => throw new RuntimeException("Invalid proposition input")
                            case Some(value2) =>
                                val v2 = ps.applyNegated(ps.valueSet.get(p.name).get)
                                ps.currValue = applyOperator(ps.currValue, v2, ps.currOp)
                                ps.resetVariables()
            case Operator(ttype) =>
                ttype match
                    case OperatorType.Negation => ps.negate()
                    case OperatorType.And =>
                        ps.currOp = Some(OperatorType.And)
                    case OperatorType.Or =>
                        ps.currOp = Some(OperatorType.Or)
                    case OperatorType.Implication =>
                        ps.currOp = Some(OperatorType.Implication)
        case en: ExprNode =>
            ps.currProp match
                case None =>
                    // since it has an operator defined but no
                    // proposition and we know it is not the first element
                    // we assume it as that there is implicitly
                    // an expression before or otherwise we throw an error
                    if (!ps.currOp.isDefined) {
                        throw new RuntimeException("invalid prop")
                    }
                    val cv = ps.applyNegated(dfs(en, ps.valueSet))
                    ps.currValue = applyOperator(ps.currValue, cv, ps.currOp)
                    ps.resetVariables()

                case Some(_) =>
                    val cv = ps.applyNegated(dfs(en, ps.valueSet))
                    ps.currValue = applyOperator(ps.currValue, cv, ps.currOp)
                    ps.resetVariables()
}