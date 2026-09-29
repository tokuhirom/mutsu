# A method call directly after a zero-operand reduction now parses

`[+].^name` used to die with "Two terms in a row" because the reduction parser
demanded whitespace and an operand after the closing `]`. A `.method` postfix
(but not the `..` range operator) directly after a symbolic `[op]` now makes it a
zero-operand reduction and the postfix applies to the identity element, so
`[+].^name` is `Int` as in Rakudo. Pinned by `t/oo/method/reduction-zero-arg-method-call.t`.
