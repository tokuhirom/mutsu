# A routine declared in a block is lexical to it

`{ sub foo { say "hi" } }; foo` printed nothing and exited 0; rakudo stops at
compile time with `Undeclared routine: foo used at line 1`. The undeclared-
routine scan scoped a routine only to the enclosing `sub`/`method`/`proto`
body; every other statement list (bare, `if` and loop blocks, package bodies)
was unit-wide. Each statement list below the unit's top level is now a scope
of its own (`Visit::visit_stmts` in `undeclared_routines/scan.rs`), so a call
is explained by a routine of its own scope or an enclosing one, before or
after the declaration (routines hoist). An `is export` routine stays
unit-wide because an import reaches it from outside its scope. Issue #15861.
