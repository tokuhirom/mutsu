# `&CALLER::LEXICAL::("infix:<+>")` resolves — the last blocker on the real `Test` module

An indirect (computed-name) code lookup through a pseudo-package now resolves,
including an operator's categorical name, and a name that genuinely is not there
now answers with an undefined `Failure` instead of throwing. This was the only
assertion of rakudo's unmodified `Test.rakumod` still blocked
(`todo/tickets/vendor-real-test-module.md`):

```raku
my $matcher = nqp::istype($op, Callable) ?? $op
    !! &CALLER::LEXICAL::("infix:<$op>")                                     #1
        // &CALLER::LEXICAL::("infix:«$op»")                                 #2
        // &CALLER::LEXICAL::("infix:<$op.subst(/<?before <[<>]>>/, "\\", :g)>"); #3
```

The three spellings exist because `#1` cannot express `<` and `>`; each has to
*fail softly* so the next is tried. Driving the vendored upstream file under its
temporary alias, `cmp-ok` now produces output byte-identical to `raku`'s — down
to the `#  matcher: 'infix:«<»'` line of a failing assertion.

## Three independent gaps, two of them in the parser

**The `&`-sigil parser did not accept a qualified chain before `::(`.** Only
`&CALLER::(` and `&::(` were recognised, so `&CALLER::LEXICAL::("infix:<+>")`
fell through to the plain code-var parser, which consumed `CALLER` and left
`::LEXICAL::("infix:<+>")` behind — **splitting the statement in two**:

```
VarDecl { name: "m", expr: CodeVar("CALLER") }          # statement 1
Expr(IndirectTypeLookup("LEXICAL::" ~ "infix:<+>"))     # statement 2
```

The reported `No such symbol 'LEXICAL::infix:<+>'` came from that unintended
second statement, not from the lookup anyone wrote. `&` now consumes any
`Ident::Ident::…::(` head and builds the same `SymbolicDeref` the `$`-sigil form
uses, reusing `parse_symbolic_deref_segments` so a trailing `::Ident` or a second
`::(…)` segment composes as well. `&CALLER::($pkg)::name` — where the
parenthesised expression names the *package*, not the routine — keeps its own
meaning, distinguished by the `::name` tail.

**The pseudo-package prefix reached `resolve_code_var` as part of the name.**
That function already strips pseudo-packages and already understands
`infix:<…>` (which is why the static term `&infix:<+>` has always worked), but
it tested for the operator shape *before* stripping, so
`CALLER::LEXICAL::infix:<+>` never took the operator path. Stripping first is the
whole of the fix, plus `LEXICAL`, `CALLERS` and `OUTERS` joining the
pseudo-package list. Nothing new was needed for operator names, and `LEXICAL::`
reaching past the current pad to the setting follows from the same routing.

**`infix:«op»` is now normalised to `infix:<op>`.** The parser bakes the `<>`
spelling in at compile time (`&infix:«<»` is `CodeVar("infix:<<>")`), so a name
arriving at runtime as a string had to agree; spelling `#2` above depends on it.
The `infix:<<op>>` form is deliberately left alone — as a bare string it is
ambiguous with a single-angle name whose body is itself bracketed, and only the
parser has the context to tell them apart.

**Absence is a `Failure`.** `&::("nosuch")` used to answer a bare `Nil`, so
`.defined` was already `False`, but the value carried no exception and using it
gave `Impossible coercion from 'Int' into 'Any'` rather than `X::NoSuchSymbol`.
It now goes through the same `no_such_symbol_failure` the direct `::('&nosuch')`
form has always used.

Pinned by `t/indirect-code-lookup.t`, which passes under `raku` too.
`roast/S02-names/pseudo-6e.t` improves from 77 to 74 failing subtests.

## Still open

Two neighbouring spellings are unaffected and are tracked in
`todo/tickets/bare-package-symbolic-deref-and-stash-routines.md`: the bare-term
form `MY::("x")` does not parse at all, and the `MY`/`LEXICAL` pseudo-stash is
built from `code.locals` plus `env`, so a registered `sub f` is absent from
`MY::{'&f'}`. Neither blocks the `Test` module, which uses only the `&` form.
