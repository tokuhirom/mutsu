# `$str ~~ $_` returns `Bool` instead of the `Match`

Found by the 2026-09-06 doc-diff sweep (`raku-doc/doc/Language/structures.rakudoc:220`)
and narrowed the same day against raku v2026.07.

## Repro

```raku
for (/a/,) { say ("ab" ~~ $_).raku }
# raku:  Match.new(:orig("ab"), :from(0), :pos(1))
# mutsu: Bool::True
```

The doc block it came from is the same defect one layer up:

```raku
my @regex-check = ( /<alnum>/, /<alpha>/, /<punct>/ );
say @regex-check.map: "33af" ~~ *;
# raku:  (｢3｣ alnum => ｢3｣  ｢a｣ alpha => ｢a｣  Nil)
# mutsu: (True True True)
```

## Narrowed — it is the topic, not the regex, the block or `.map`

Every other spelling of "smartmatch a string against a regex held in a variable"
answers with the `Match`:

| Program | raku | mutsu |
|---|---|---|
| `say ("ab" ~~ /a/).raku` | `Match` | `Match` |
| `my $r = /a/; say ("ab" ~~ $r).raku` | `Match` | `Match` |
| `sub f($x) { "ab" ~~ $x }; say f(/a/).raku` | `Match` | `Match` |
| `say ("ab" ~~ *)(/a/).raku` | `Match` | `Match` |
| `say (1,).map({ "ab" ~~ /a/ }).raku` | `Match` | `Match` |
| `my $r = /a/; say (1,).map({ "ab" ~~ $r }).raku` | `Match` | `Match` |
| `say (1,).map({ my $m = "ab" ~~ /a/; $m }).raku` | `Match` | `Match` |
| **`for (/a/,) { say ("ab" ~~ $_).raku }`** | `Match` | **`Bool::True`** |
| **`say (/a/,).map({ "ab" ~~ $_ }).raku`** | `Match` | **`Bool::True`** |

So `.map`, the block, the `WhateverCode` spelling and regex-in-a-variable are
all fine. The one condition that changes the answer is the right-hand side being
the **topic** `$_`.

Raku's rule is that `$x ~~ $y` returns whatever `$y.ACCEPTS($x)` returns — a
`Regex` returns its `Match`. Something on the `$_` path is instead asking for a
boolean.

## Root cause — found 2026-09-06, and it is not a special case for `$_`

**The RHS is evaluated with `$_` already overwritten by the LHS**, so
`"ab" ~~ $_` really evaluates `"ab" ~~ "ab"` — a string-vs-string smartmatch,
which is correctly `True`. Nothing is coercing a `Match` to a `Bool`; the match
that would produce the `Match` never happens.

`exec_smart_match_expr_op` (`src/vm/vm_smartmatch_ops.rs`) does this before
running the RHS instruction range:

```rust
if topic_cell.is_none() {
    self.env_mut().insert("_".to_string(), left.clone());
}
...
let rhs_run = self.run_range(code, rhs_start, rhs_end, compiled_fns);
```

The two bytecode streams are otherwise identical — the working spelling's RHS is
`GetLocal(0)` and the broken one's is `GetGlobal("_")`, and both run under the
same `SmartMatchExpr` with the same flags. Confirmed by elimination: reading the
topic into a name *first* (`my $r2 = $_; "ab" ~~ $r2`) answers `Match`, and
`$_.WHAT.^name` outside the match is `Regex`, so the topic holds the right value
right up to the moment the RHS runs.

## Why the overwrite is there, and why the fix needs a decision

It is deliberate and load-bearing: `$x ~~ s///` must topicalize `$x` so the
substitution has something to write through, and the surrounding code has a
careful `topic_cell` / `topic_ro_override` dance for the aliasing cases
(`t/smartmatch-subst-topic.t`, ADR-0045). Raku's own rule is the other way
round for a *value* RHS — `$x ~~ EXPR` evaluates `EXPR` and then calls
`EXPR.ACCEPTS($x)`, with the topicalization happening inside `ACCEPTS`, not
around the evaluation.

So the fix is to separate the two: an RHS that must run *as* the match
(`s///`, `tr///`, a regex literal, a block) needs the topic installed first; an
RHS that is merely an expression to be evaluated must see the enclosing topic.
The opcode already carries `rhs_is_match_regex` / `rhs_pure_regex` flags, so the
compiler may already know enough to make that distinction — check what they mean
before adding a third flag.

Both spellings in the table above compile to `rhs_pure_regex: false`, so those
existing flags do **not** currently separate these two cases.

## Where to look

`src/vm/vm_smartmatch_ops.rs` (`exec_smart_match_expr_op`, the `env_mut().insert("_")`
above and the `saved_topic` restore after it), the compiler's `SmartMatchExpr`
emission and what `rhs_is_match_regex` / `rhs_pure_regex` are set from, and
`t/smartmatch-subst-topic.t` — the pin that the overwrite exists to satisfy and
that any fix must keep green.

## Neighbourhood to check when fixing

`$_ ~~ $_`; the topic holding a `Junction`, a type object, a `Callable`, a
`Range`, a `Set` or a plain `Str` (each has its own `ACCEPTS` return value, and
only some of them are `Bool` in raku); `!~~` against the topic; `when $_ { }`;
`given`/`for` topics vs a `-> $_` block parameter; and `$/` being set correctly
after the match in every one of those (a `Match`-returning smartmatch must also
populate `$/`).
