# Sink-context worries read as rakudo's

rakudo lists a unit's compile-time worries under a single
`Potential difficulties:` header. A sunk operator expression is named by its
source text, and each worry points at the construct:

```
Potential difficulties:
    Useless use of 1  +   2 in sink context
    at -e:1
    ------> <BOL><HERE>1  +   2; say 1
```

mutsu printed `Useless use of "+" in expression "1+2" in sink context`: no
header, the expression re-rendered without its spaces or quotes, and no
pointer line (#18951). The sink pass now reads the statement's own text back
from its recorded span. Each worry is printed under the shared header,
together with the `------>` line. A type object reads `Useless use of Int`.
An array constructor counts as a useless operand, so `[] + 1` is reported.
An expression the pass cannot read back, such as the body of a statement
modifier, is approximated as `$_ * 2`.

rakudo warns for any pure operator expression, even one whose operands call
something (`f() + 1`). mutsu still reports only operands without side
effects, as before.
