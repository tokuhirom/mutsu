# A postcircumfix `{...}` chains onto any term, not onto an allow-list of `Expr` shapes

Found while evaluating the `LogP6` logging module, whose
`create-and-store-loggers` uses the idiom

```raku
(%cliches-to-traits{$cliche.name} //= SetHash.new){$trait} = True;
```

mutsu could not parse it. At statement level it was a hard
`Confused. expected statement`; as a `say` argument it silently mis-parsed,
taking `(%h{"a"} //= "x")` as the whole argument and leaving `{"k"}` behind as a
disconnected bare block (with a bogus "Useless use of constant string" warning).
Replacing the hash-subscript lvalue with a plain scalar (`($x //= 1){"k"}`)
worked, which is what made the trigger look so specific.

## Root cause

The postfix loop's `{...}` subscript arm
(`src/parser/expr/postfix/loop_.rs`) was gated on an **allow-list of `Expr`
variants**: `HashVar`, `Var`, `BareWord`, `Index`, `MethodCall`,
`HyperMethodCall`, `Call`, `Literal`, `DoStmt`, `Grouped`, `Binary`, `Ternary`,
`Hash`, `Whatever`. A `gdb` breakpoint on that arm showed the term arriving as
`Expr::DoBlock` — the shape `build_compound_assign_expr` produces for a
*subscripted* compound-assignment lvalue, because it has to hoist the index into
a temporary. `DoBlock` was not on the list, so the subscript was dropped. A
scalar lvalue lowers to `Expr::Ternary` instead, which was on the list — hence
the "only with a hash-subscript LHS" symptom.

The allow-list itself was the defect. In Raku a `{` glued directly onto a term
(no intervening whitespace) is `postcircumfix:<{ }>`, unconditionally — the
whitespace is the only thing that distinguishes a following bare block. Each of
`Binary`, `Ternary`, `Hash`, `Whatever` and the hyper-method-call variants had
been appended to the list by hand after someone hit exactly this failure mode,
and the failure is silent (the `{...}` becomes a stray block) rather than an
error, so the next shape to reach that point would have been lost the same way.

## Fix

The list is inverted into `brace_is_postcircumfix`, so the Raku rule is the
default. The exception is not a set of shapes but a lexical fact: **the term's
own parser ate the whitespace**. Normally the remainder carries the
distinction — with a space it starts with the space, not with `{` — but an
inline `my $x` declaration and the `gather EXPR` statement prefix both consume
their trailing whitespace, so the space is gone by the time the postfix loop
looks. The loop now tracks whether the span consumed for the current term ended
on whitespace (recomputed as each postfix op moves the term along, and cleared
by an unspace `\`, which deliberately glues) and feeds that to the guard. That
covers every whitespace-eating parser at once instead of naming them.

One shape-based exception remains, spelled out in the guard: an inline
`my`/`our`/`state` declaration is never subscripted directly in expression
context (`(my %h){key}` is how you write that).

The whitespace tracking was found the hard way — the first attempt used a
plain deny-list and broke `for gather trip(5) { ... }`, which the `t/` suite
caught in three files.

## Notes on what this does and does not fix

The parse is now identical to raku's: `say (%h{"a"} //= "x"){"k"}` produces
raku's own `Type Str does not support associative indexing.` runtime error, and
`(%h{"a"} //= {}){"k"} = 1` writes through. Two *unrelated*, pre-existing gaps
were noticed while verifying and are NOT addressed here:

- a `SetHash` (or other QuantHash) stored as a hash element is not mutated in
  place by `%h<a>{"k"} = True` — the write is lost. The same write works when
  the `SetHash` is held in a scalar. This is an element-container issue, not a
  parser one.
- assigning into a `Str`'s associative index silently succeeds in mutsu where
  raku throws (already tracked by
  `todo/tickets/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md`).

Pinned by `t/parser-expression-gaps.t`.
