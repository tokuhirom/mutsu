# Named `anon` declarations cross the RakuAST boundary

`anon sub g { }`, `anon class Foo { }`, `anon role R { }` and
`anon grammar G { }` all stopped at `.AST` and under `MUTSU_RAKUAST=1`. The
converter did not know the parser's `__anon_decl` marker. Measured on rakudo
2026.09, each declaration is the ordinary node with `scope => "anon"`
before its name, and that is how they are rendered and lowered back now.

A statement-level `anon class C { }` used to take a separate route. The
parser registered `C` and then emitted a call to a
`__MUTSU_UNREGISTER_CLASS__` builtin to remove it again. Now it is the same
marked declaration the expression form builds, with the name given a site
suffix so nothing can look the class up by it. The builtin and the
compiler's special case for its block are gone, and an `anon` class or role
no longer gets a compile-time package shell.
