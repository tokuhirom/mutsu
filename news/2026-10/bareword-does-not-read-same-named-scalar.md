# A bare word no longer reads a same-named `$` scalar

`my $bar = 3; say bar` printed `(Any)` — the bare word `bar` read the scalar's stale `env`
copy — where Rakudo stops with `Undeclared routine: bar used at line 1` (#11898). `$bar` and
the term `bar` are different symbols, but mutsu keeps a `$` scalar sigil-stripped under the
very `env` key a sigilless binding (`my \bar`, `-> \bar`) uses, so the run-time resolver could
not tell the two entries apart.

The compiler can: it knows which of the two a local is. A bare word spelling a `$` local of
the same scope now compiles to `GetBareWordOverScalar`, which resolves exactly as `GetBareWord`
does minus the `env[name]` read, and raises `X::Undeclared::Symbols` when nothing else (a sub,
a class, an enum key, a sigilless binding, a core term) claims the name.

That only holds if the compiler's knowledge is complete, and two desugarings had dropped it:
`given EXPR -> \ex` declared `ex` with no sigilless marker, and `while`/`until COND -> \r`
declared `r` as an assigned `$` scalar. Both now bind the term the way `with`/`if` already
did — `while` through a scalar temporary and a per-iteration `my \r = $tmp`, which also gives
each iteration its own term for closures to capture and makes `r` read-only like Rakudo's.
