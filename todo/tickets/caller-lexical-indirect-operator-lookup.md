# `&CALLER::LEXICAL::("infix:<+>")` — indirect symbol lookup of an operator

An indirect (computed-name) symbol lookup through the `CALLER::LEXICAL::`
pseudo-package throws instead of resolving, and throws instead of returning a
`Failure` when the symbol genuinely does not exist.

```
$ mutsu -e 'my $m = &CALLER::LEXICAL::("infix:<+>"); say $m.WHAT; say $m(2,3)'
No such symbol 'LEXICAL::infix:<+>'
  in block <unit> at -e line 1

$ raku -e 'my $m = &CALLER::LEXICAL::("infix:<+>"); say $m.WHAT; say $m(2,3)'
(Sub+{is-pure})
5
```

Two separate things are missing:

1. **Resolution.** `&CALLER::LEXICAL::("<string>")` should look the name up in
   the caller's lexical scope, and an operator's canonical name
   (`infix:<+>`, `infix:«le»`, …) has to resolve to the routine that implements
   it — including mutsu's built-in operators, which are not ordinary registered
   subs.
2. **Absence is a `Failure`, not a throw.** raku returns an undefined `Failure`
   for a name that is not there, which is what makes the `//` fallback chain
   below work at all. mutsu raises immediately, so the chain never gets to try
   its second and third forms.

## Why it matters

It is how rakudo's `Test.rakumod` turns `cmp-ok`'s string operator into a
callable:

```raku
my $matcher = nqp::istype($op, Callable) ?? $op
    !! &CALLER::LEXICAL::("infix:<$op>")                                     #1
        // &CALLER::LEXICAL::("infix:«$op»")                                 #2
        // &CALLER::LEXICAL::("infix:<$op.subst(/<?before <[<>]>>/, "\\", :g)>"); #3
```

The three forms exist precisely because #1 cannot express `<` and `>`; each is
expected to *fail softly* so the next is tried. On mutsu the first one throws:

```
$ mutsu -I tmp/core -e 'use Test2; plan 1; cmp-ok 3, "<", 5, "cmp-ok"'
No such symbol 'LEXICAL::infix:<\<>'
  in sub cmp-ok at tmp/core/Test2.rakumod line 266
```

`cmp-ok` is the only assertion of the genuine upstream module that is blocked on
this — everything else in the happy path already runs
(`todo/tickets/vendor-real-test-module.md`).

## Note on scope

The pseudo-package machinery mutsu already has (`CALLER::`, `OUTER::`,
`SETTING::`, `DYNAMIC::` are recognized in `is_interpreter_handled_function`)
handles *static* names. This ticket is about the `::("...")` indirect form and
about operator names specifically; a general `&CALLER::LEXICAL::($name)` for
ordinary sub names is the easier half and is worth doing first.

## Measured (2026-08-01): three independent pieces, and one of them is free

`&`-sigil indirect lookup through a pseudo-package does not work *at all* today
— not just for operators. It does not even reach the "no such symbol" answer:

```
$ mutsu -e 'sub f($a,$b) {$a*$b}; my $m = &MY::("f"); say $m(2,3)'
Impossible coercion from 'Int' into 'Any': no acceptable coercion method found
$ raku  -e 'sub f($a,$b) {$a*$b}; my $m = &MY::("f"); say $m(2,3)'
6
```

So the work splits into three:

1. **`&PSEUDO::("name")` must resolve at all.** The `&` + `SymbolicDeref` path
   yields something that is not callable, hence the coercion error above. This
   is the "easier half" named earlier and is a prerequisite for the rest.
   `Interpreter::resolve_indirect_type_name`
   (`src/runtime/accessors_stash.rs:142`) already strips a leading `&` and
   returns `resolve_code_var`, so the gap is upstream of it: the pseudo-package
   prefix is passed through as part of the name (`LEXICAL::infix:<+>`) instead
   of selecting a scope.
2. **Operator names must resolve — but the callable already exists.** `&infix:<+>`
   as a *static* term works today (`my &f = &infix:<+>; say f(2,3)` prints 5), so
   there is a mechanism that turns a built-in operator into a `Sub` value; the
   indirect form only has to reach it. This is much cheaper than the original
   note implied.
3. **`LEXICAL::` has to include the setting.** `&MY::("infix:<+>")` is correctly
   "no such symbol" in raku too — `infix:<+>` is not in the caller's own pad, it
   is in CORE, and `LEXICAL::` reaches it because the lexical chain ends at the
   setting. mutsu's `&LEXICAL::(...)` currently answers `(Any)` rather than
   walking out that far.

Absence-is-a-`Failure` is *already* implemented for the direct form
(`no_such_symbol_failure` in the same file returns an `X::NoSuchSymbol` Failure,
which is why `raku`'s own `say $m` on a missing name throws while `$m.defined`
answers False). Once (1) routes through that function the third requirement
comes along for free.

## Where it actually breaks: it is mostly a PARSER gap

Dumping the AST turns this from "a runtime lookup does not resolve" into three
concrete sites. The runtime half is nearly done already.

**The `::(...)` postfix does not accept a qualified chain.**
`src/parser/expr/postfix/loop_.rs:1372` desugars `expr::(key)` to
`expr.WHO{key}`, but it only fires when the very next characters are `::(`. For
`&CALLER::LEXICAL::("infix:<+>")` the tail is `::LEXICAL::(`, so the postfix
declines and the statement **splits in two**:

```
$ mutsu --dump-ast -e 'my $m = &CALLER::LEXICAL::("infix:<+>")'
VarDecl { name: "m", expr: CodeVar("CALLER") }          # <-- statement 1
Expr(IndirectTypeLookup("LEXICAL::" ~ "infix:<+>"))     # <-- statement 2
```

which is where the bogus `No such symbol 'LEXICAL::infix:<+>'` comes from — it
is a *separate* lookup of a name that was never meant to exist. The fix is to
let that postfix consume `::Ident` segments before the final `::(`.

**A non-`$` head has no symbolic-deref path at all.** `Pkg::("name")` in term
position does not parse:

```
$ mutsu -e 'say MY::("x")'
Confused. expected statement: expected identifier after '::' ...
$ raku  -e 'say GLOBAL::("Int")'
No such symbol 'GLOBAL::Int'          # parses; resolves at runtime
```

The `$`-sigil form is fine (`parse_symbolic_deref_segments`,
`src/parser/primary/var/scalar.rs:32`, is reached from the scalar-var parser and
`$MY::("x")` works). Only the `&`/bare heads lack it.

**The `&` sigil is dropped from the key.** `&MY::("f")` reaches
`MY.WHO{"f"}`, but a code symbol lives in the stash as `&f` — raku's own error
message spells it `'LEXICAL::&f'`. So the desugar has to sigil-prefix the key
when the head carried `&`.

## What is left on the runtime side

- The `MY`/`LEXICAL` pseudo-stash (`src/vm/vm_var_assign_local.rs:468`) is built
  from `code.locals` plus `env`, so a `sub f` — which lives in the routine
  registry, not in `env` — is simply absent: `MY.WHO{"&f"}` answers `(Any)`
  where raku answers `&f`. Registered routines have to be folded in.
- Operator names then need **nothing new**:
  `Interpreter::resolve_indirect_type_name` already strips a leading `&` and
  calls `resolve_code_var` (`src/runtime/accessors_resolve.rs:181`), which
  already understands `infix:<…>` / `prefix:<…>` / `postfix:<…>` — that is why
  the static term `&infix:<+>` works today. Routing the indirect form to the
  same function is the whole of requirement (2).
- `LEXICAL::` still has to reach past the current pad to the setting for
  `infix:<+>`; `resolve_code_var`'s builtin-operator fallback is what supplies
  that, so it follows from the same routing.

Suggested order: parser chain + sigil first (that alone turns the error into an
honest "no such symbol"), then the stash contents, then confirm `cmp-ok` end to
end against `raku`:

```
$ raku  -e 'use Test; plan 1; cmp-ok 3, "<", 5, "cmp-ok"'
$ mutsu -I tmp/core -e 'use Test2; plan 1; cmp-ok 3, "<", 5, "cmp-ok"'
```
