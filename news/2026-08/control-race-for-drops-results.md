# `race` is a statement prefix in expression position too

```raku
my $r = race for ^10 -> $n { $n if $n %% 2 };
say $r.elems;
```

`raku` says `5`. mutsu warned *"Useless use of $n in sink context"* and said `1`.
Found by the doc-diff harness on `Language/control.rakudoc:717`.

## Root cause

`--dump-ast` made it immediate, and it was not a collection bug at all: the
whole statement parsed as **two** statements.

```
VarDecl { name: "r", expr: BareWord("race") },
For { ... }                                   # sunk, its value thrown away
```

mutsu recognises `race for` / `hyper for` as statement prefixes at *statement*
level (`race_for_stmt` / `hyper_for_stmt`, giving `ForMode::Race` / `Hyper`), and
recognises `lazy`, `hyper`, and `eager` as prefixes in *expression* position
(`src/parser/expr/postfix/loop_.rs`). `race` was missing from the expression list
entirely, so in a value position it fell through to the bare listop-term path,
`$r` got the string `race`, and the `for` loop became a separate sunk statement —
hence both the sink warning and the `1`.

## Fix

A `race` arm was added directly alongside the existing `hyper` one: the prefix
parses its operand and wraps it in a `.race` method call. That makes `race`
behave in expression position exactly as `hyper` already did, and it fixes the
general `race LIST` spelling at the same time (`my $x = race 1..5` was
*"Undeclared routine: race"*; it is a `RaceSeq` now, as in raku).

Because `race` gives no ordering guarantee, the regression test asserts
order-insensitively (`$r.sort.join(',')`) rather than pinning an iteration order.

Pinned by `t/control-constructs-in-expression-position.t`.
