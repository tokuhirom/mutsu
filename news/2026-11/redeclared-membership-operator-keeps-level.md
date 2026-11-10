# A redeclared `∈` / `∋` / subset operator keeps its chaining level

A `multi sub infix:<∈>` that redeclared a core membership or subset operator took the default user-operator
level, so `$a ∪ $b ∈ $c` read as `$a ∪ ($b ∈ $c)`. Like `∪`/`∩` (set-intersection-precedence), the core parser
now keeps the token and its chaining precedence, and the operator's user candidates are still tried under the
`∈`/`∋` spelling when the core `(elem)`/`(cont)` operator runs (Qwiratry `t/operator/precedence.rakutest`, #13370).
