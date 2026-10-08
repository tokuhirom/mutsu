# A class may compose a role its parent class already composes

`class R does C is M` where `class M does C` died with "Inconsistent class hierarchy".
The composed role was listed ahead of `M` in the local precedence order, contradicting
`M`'s own `M, C` linearization. A role that a sibling parent already carries is no longer
an ordering constraint. Found via Graphviz::DOT::Grammar, whose three test files now pass.
