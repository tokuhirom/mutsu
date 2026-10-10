# RakuAST preserves declaring signatures and role arguments

The RakuAST frontend now retains optional and defaulted elements in declaring
signatures, literal postconstraints, and the default of a bare grouped
declaration. Literal matchers expose Rakudo's `Term::Declaration` tree and
lower through the existing compiler expansion. Constructed parameters retain
their literal value and writable-default fields too.

Loose logical tails keep their declaration as an operand instead of erasing
its source form. Bare invocant type captures retain their binding role, and
anonymous methods recognize folded sigilless invocants as declared terms.
Parameterized role arguments share the ordinary colonpair conversion, covering
boolean, variable and bracketed pairs without splitting expression text.

The bidirectional regression suite checks these contracts against Rakudo.
Initialized grouped declarations losing `is default` in ordinary execution
are tracked separately in #12547; nested declarator groups still need a
representation that preserves their structure.
