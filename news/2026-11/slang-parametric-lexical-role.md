# `refine_slang` accepts an instance of a lexical parametric role

`$*LANG.refine_slang('MAIN', role { }, Numish[%handlers])`, with `Numish` a
`my role Numish[%handlers]` of the module, died at activation with
`'(Numish[Hash])' is not a known role`: the role instance was looked up by its
printed name instead of the declaration it was instantiated from. The members
are now read from that declaration, so a slang can pass run-time arguments into
its actions role. overload::constant now activates; its handlers are routines of
the unit being parsed, which the activation does not receive yet (#17055).
