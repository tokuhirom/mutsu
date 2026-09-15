# The immutable-topic receiver oracle now tracks a variable's own binding

`Compiler::for_iterable_yields_bare_items` decides whether a `for` loop's
implicit topic, or a `.map`/`.grep`/`.first` callback's `$_`, is assignable —
it answers `true` only for receivers a `for`/`.map`/`.grep` written directly
against can PROVE bare from their own syntax (a list literal, a `Range`, a
`%` variable, `.List`/`.pairs`/...). Two receiver shapes could never be
decided that way, because the tell-tale syntax is at the variable's
DECLARATION, not at the `.map`/`for` call site that uses it:

```raku
my @a := (1,2,3); @a.map({$_=5}).eager      # raku: X::AdHoc   mutsu (before): (5 5 5)
my @a := (1,2,3); for @a { $_ = 5 }         # raku: X::AdHoc   mutsu (before): silently OK
my $s = (1,2,3).Seq; $s.map({$_=5}).eager   # raku: X::AdHoc   mutsu (before): (5 5 5)
```

Measuring also turned up a third, simpler gap in the same oracle: `.Seq`
written directly against a bare receiver (no intermediate variable at all)
wasn't handled either — `(1,2,3).Seq.map({$_=5}).eager` produced `(5 5 5)`
too, because no arm recognized a `.Seq` method call at all.

## The fix

`.Seq` reifies whatever items its target already has — it mints no fresh
ones — so it now inherits the target's own verdict:
`for_iterable_yields_bare_items` grew one arm,
`Expr::MethodCall { target, name: "Seq", args: [] } => for_iterable_yields_bare_items(target)`,
with no restriction to `@`/`%` variables (unlike the existing `.List`/`.keys`/
`.pairs`/... arm, which restricts to those because it mints fresh items).
That alone closes the direct-chain case.

The variable case needed an actual compile-time fact about the VARIABLE,
tracked from its own declaration: `Compiler::provably_bare_receiver_vars`
records, for the current lexical scope, which `@`(`:=`-bound)/`$`-sigiled
names currently denote a value with no container behind its own items.
`Compiler::receiver_provably_yields_bare_items` layers this on top of the
existing syntactic oracle for a bare `ArrayVar`/`Var` receiver, and both
`control_for.rs`'s `ForLoopSpec::source_items_are_bare` and
`method_binds_immutable_topic` (the `.map`/`.grep`/`.first` gate) now consult
it instead of the syntax-only function.

The fact is populated at every `Stmt::VarDecl` AND at every later
`Stmt::Assign` (a plain scalar `=`/`:=`, or a bare `@`-name `:=` rebind with
no `my` — legal in raku: `my @a := (1,2,3); @a := (4,5,6);` rebinds the same
lexical) that reaches the same name, so a rebind or reassignment away from a
bare value clears a stale `true` rather than leaving a false positive behind
— the one direction this oracle must never take. It is scoped like the
existing `user_listop_shadows`/`dynamic_scope_names` fields in
`LexicalScopeSnapshot`: cloned on block entry so a nested block still sees an
outer fact, and restored on exit so a shadowing inner declaration cannot leak
it back out.

## Two things measurement caught that a first pass would have missed

**A `%`-sigil name is deliberately excluded from the tracking**, even though
`is_bound_container_vardecl` (the existing flag it borrows the "was this a
`:=` bind" test from) covers `%` too. Nothing on the lookup side
(`Expr::ArrayVar`/`Expr::Var`) ever queries a `%`-keyed entry, and including
it risked colliding with an unrelated `$name`/`@name` sharing the same bare
spelling (mutsu's AST stores a variable's name WITHOUT its sigil for
`ArrayVar`/`Var`, so the map keys on `sigil + bare-name` to keep `$h` and
`@h`/`%h` from colliding with each other).

**A plain scalar's own literal value must never mark the variable bare** —
this one reached the full local+related-test suite as a real regression
before being caught. The natural first attempt reused
`for_iterable_yields_bare_items` verbatim for a `$`-declaration's RHS, which
answers `true` for any non-Array/Hash literal (correct for `for 5 { $_ = 1 }`,
where the literal itself is `$_`). But `my $a = 1;` is not that: `$a` always
has its OWN `Scalar` container, and `for $a -> $x is rw { ... }` binds to
THAT container, never to "a bare 1". Marking `$a` bare after any plain
literal assignment broke `t/control/for-scalar-source-alias.t`,
`t/control/topic-assign-in-nested-block.t` and
`t/routines/signature/for-param.t` — every scalar ever initialized from a
literal lost its `is rw`/`<->` alias binding. The fix is a second, narrower
predicate, `Compiler::value_expr_denotes_bare_receiver`, used only when
populating the variable fact: it forces a bare `Expr::Literal` to `false` and
otherwise defers to the shared oracle, so only a genuine multi-item
collection shape (a list literal, a `Range`, `.Seq`/`.List`/...) — never a
plain stored scalar value — makes the variable itself provably bare.

`t/control/immutable-topic-receiver-oracle.t` gains 10 new assertions: the
two direct `.Seq`-chain rows, a control confirming `@a.Seq` over a real
array still writes through, the `:=`-bound-`@a` and `Seq`-holding-`$s` rows
for `.map`/`.grep`/`for`, and two rebind/reassignment controls proving the
fact does not survive a later rebind to something not provably bare. All
verified against `raku` directly (the whole file is designed to pass under
`raku` too).

Section D of the same survey — a `gather` sequence's element store — remains
open; its own diagnosis says it needs an `array_context`/`list_context`
distinction that is separate work, out of scope for this slice.
