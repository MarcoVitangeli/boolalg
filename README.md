# Simple boolean algebra propositions parser

Given
```
~p v q ^ (p v r) v (p ^ q v (r ^ p))
```

It is able to parse and evaluate it with determined parameters for each single proposition

## Supported Operators:
- Brackets: expression can be encapsulated using ()
- v -> Or operator
- ^ -> And operator
- ~ -> Negation operator (it can be repeated)

using `sbt run` there is a sample main function that generates a truth table for a given example proposition