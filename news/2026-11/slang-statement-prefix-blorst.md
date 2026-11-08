# Slang host: `statement-prefix` productions, `<blorst>`, `key-origin` arguments

A slang can add a `statement-prefix:sym<...>` token (Qwiratry's `WINDOWFIRST`, `FRAMEADD`, ... phasers),
tried where a statement or a term starts like the other production categories. The host grammar has
`<blorst>` (a `RakuAST::Block` for `{ ... }`, otherwise one `RakuAST::Statement::Expression`), and
`<statement>` answers the `Statement::Expression` node itself, as rakudo's action makes it, instead of its
bare expression. `key-origin('rule', 'keyword')` passes the keyword to the rule it calls, so a slang's own
`rule aggsub-def($declarator)` works. Underneath, a grammar rule with a signature can be called as a
method with arguments (`self.thing('kw')`, `self."$name"('kw')`, rakudo's behaviour); it used to die with
"No such method" as soon as it took a parameter (#13370, #16888).
