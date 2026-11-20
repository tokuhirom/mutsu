# `.origin` of a `for` loop's destructuring parameters

The parser now records where a `for` loop's destructuring pattern is written:
`-> ($a, $b)`, `-> [$a, $b]`, a typed `-> Pair (:key($k), :value($v))`,
several patterns, nested ones, and a sigiled parameter followed by one
(`-> Pair $p (:$key)`). The RakuAST view of such a loop places each
`Parameter`, its `sub-signature` and the parameters inside it as rakudo 2026.11
does. A renamed named parameter (`:key($k)`) now takes its target's range from
the variable at the end of its alias chain (#15780).
