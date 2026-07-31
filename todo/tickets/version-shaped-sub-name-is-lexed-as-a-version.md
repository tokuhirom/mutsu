# A `v`-plus-digits sub name is lexed as a Version literal

A routine whose name looks like a version literal cannot be called:

```raku
sub v1($x) { "called-$x" }
say v1(5);      # raku: called-5
                # mutsu: No such method 'CALL-ME' for invocant of type 'Version'
```

The declaration itself is accepted; only the *call* misparses. `v1` at a term
position is lexed as the `Version` literal `v1`, and the following `(5)` then
reads as an invocation of that Version, hence the `CALL-ME` error. `v2`, `v10`
and every other `v` + digits spelling behave the same.

Raku's version literal is a term, so a bare `v1` in term position genuinely *is*
`Version.new("1")` there too — but a name that has been *declared as a routine*
wins, and an identifier followed by `(` is a call regardless. mutsu decides
before consulting the declared-routine set.

Names of this shape are not exotic: `v1`/`v2` are natural for a versioned API
helper, and a test that names its subject `v1`/`v2`/`v3` (a very common habit
when writing several small variants of the same probe) silently fails in a way
that looks like a runtime bug rather than a lexing one. Found while reducing
the Template::Mustache `visit` failure, where three probe subs named `v1`-`v3`
all died with `CALL-ME` and briefly looked like a `^can` problem of their own.

The neighbouring lexer trap is already documented in
`news/2026-07/tls-openssl-battery.md`: the version lexer treats `-` as a
trailing marker only, so `v4-split` is an identifier. This is the same
term-vs-identifier decision made too early, on the digits side.

Affected: the version-literal term in the parser's term dispatch (`v` followed
by digits) — it needs to lose to a declared routine, and to a following `(`.
