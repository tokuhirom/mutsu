# `has $a ||= EXPR` in a method body no longer dies with "Variable '$a' is not declared"

The assignment half of a compound attribute initializer on an alias attribute (`has $a ||= 5`, no twigil)
assigned to the bare lexical `$a`, which does not resolve where the declaration sits (a method body). It now
assigns to the attribute `$!a` for every spelling, so the declaration compiles and `^attributes` lists `$!a`
as rakudo does. Pinned by `t/oo/class/class-has-alias-compound.t`.
