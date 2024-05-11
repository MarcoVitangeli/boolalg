# Simple boolean algebra propositions parser

Given
```
~p v q ^ (p v r) v (p ^ q v (r ^ p))
```

It is able to parse and evaluate it with determined parameters for each single proposition

Proposition names can be entire words up to 30 characters (lowercase and uppercase english letters)

## example

```scala
// nested proposition with long names
val prop = "~canAccess v (userLogguedIn v userIsAdmin) ^ (~isEnabled & ~userLogguedIn)"

// boolean values to evaluate the composed proposition
val params = Map(("canAccess", true), ("userLogguedIn", false), ("userIsAdmin", true), ("isEnabled", true)

val res = SimpleParser.parse(prop, params)

res match
    case Failure(x) => throw new RuntimeException("error during parsing", x)
    case Success(x) => // result boolean value 
        println("Result:", x)
```

## Supported Operators:
- Brackets: expression can be encapsulated using ()
- v -> Or operator
- ^ -> And operator
- ~ -> Negation operator (it can be repeated)
- & -> logical implication (or logical conditional)

using `sbt run` there is a sample main function that generates a truth table for a given example proposition and storing it in a CSV, the code is in a function-first aproach so in this files you'll find:
- Tokens.scala: Token generation functions and types
- Parser.scala: recursive expression parser functions. The parser makes a DFS over an expression tree recursively executing complex expressions
- Helpers.scala: functions used in Main. It has examples of printing the result of a truth table or generating a CSV

## Examples:

### Printing truth table

small helper function for table generation
```scala
def generateTruthTable(N: Int): Seq[Seq[Boolean]] = {
        // Generate all possible combinations of truth values for N propositions
        val numCombinations = math.pow(2, N).toInt
        (0 until numCombinations).map { i =>
            (0 until N).map(j => ((i >> j) & 1) == 1)
        }
    }
```

```scala
    println("COLS:\n")
    cols.foreach(println)
    println()

    for (comb <- m) {
        val params = cols.zip(comb).toMap
        val res = SimpleParser.parse(s, params) // params is a dictionary where the key is the proposition name and the value its boolean value

        res match
            case Failure(x) => throw RuntimeException("error parsing proposition", x)
            case Success(value) =>
                val rowStr = comb
                    .appended(value)
                    .map{_.toString()}
                    .mkString("\t")

                println("|" + rowStr + "   |")
```