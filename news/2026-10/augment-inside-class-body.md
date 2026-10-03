# `augment class` inside a class body augments the named type

An `augment class Int { method m {...} }` written inside another class's body
(Int::polydiv does it from `unit class Int::polydiv`) lost its methods to the
enclosing class. The parser's nested-method hoisting treats a nested block's
methods as belonging to the surrounding package, and it looked into the
`augment` block as though it were an ordinary nested block. An `augment` now
owns its scope like a class, role or package declaration does, so
`5.polydiv(…)` reaches the core `Int` and Int::polydiv's suite passes.
