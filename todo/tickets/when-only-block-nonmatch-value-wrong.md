# A block whose only statement is a non-matching `when`/`default` evaluates to the wrong value

## Discovered while fixing

`todo/tickets/map-grep-inline-block-swallows-succeed.md` (now
`news/2026-08/map-grep-inline-block-succeed-signal.md`) fixed `.map`/`.grep`
losing an item entirely when an inline block's `when`/`default` raises a
succeed signal on a MATCH. While verifying that fix against inputs that
also contain non-matching items, a separate, pre-existing bug surfaced: the
value an inline map/grep block produces for an item that matches NO
`when`/`default` branch is wrong.

## Repro

```
$ mutsu -e 'my @a = (1,"a",2).map({ when Int { "int" } }); say @a.raku'
["int", "a", "int"]
$ raku  -e 'my @a = (1,"a",2).map({ when Int { "int" } }); say @a.raku'
["int", 0, "int"]
```

```
$ mutsu -e 'my @a = (1,"a",2,"b").grep({ when Int { True } }); say @a.join(",")'
1,a,2,b
$ raku  -e 'my @a = (1,"a",2,"b").grep({ when Int { True } }); say @a.join(",")'
1,2
```

mutsu's non-matching item falls back to the ORIGINAL topic value (`"a"`
stays `"a"` in the map case; `"a"`/`"b"` are treated as truthy in the grep
case, so nothing is filtered at all). raku's non-matching item evaluates
to something else entirely (`0` in the map probe — not obviously `Nil`,
worth re-checking against `raku-doc/doc/Language/control.rakudoc`'s
`when`/`given` semantics before assuming what the "right" value is; a
third probe, a raw non-block-call `{ when 2 { True } }` invoked directly
per-item, printed `False` for non-matching items, suggesting a boolean
coercion is involved somewhere in the real semantics, not a bare `Nil`
passthrough).

## Root cause (partial — needs more investigation)

In the map/grep inline fast-path `Ok(())` arms (`eval_map_over_items` in
`src/runtime/resolution_map_grep.rs`, `eval_grep_over_items_with_mutated`
and `eval_map_over_items_rw` in `src/runtime/resolution_map_grep_rw.rs`),
the block's result value is computed as:

```rust
vm.last_stack_value().cloned().or_else(|| vm.env().get("_").cloned()).unwrap_or(Value::NIL)
```

When a `when`/`default` chain is the block's only statement and it does
NOT match, nothing is pushed to the stack (`last_stack_value()` is
`None`), so this falls back to `vm.env().get("_")` — the CURRENT TOPIC,
i.e. the original unfiltered item — instead of whatever raku's real
non-match value is. This `.or_else` fallback exists for a different
reason (reading back a `$_`-mutated value for `<->`/rw map semantics, per
the `writeback` closures nearby) and is firing incorrectly here.

This bug was previously unobservable through map/grep + `when`, because
ANY match in the same call would abort the whole map/grep with the (now
separately fixed) "swallows succeed" crash before this fallback path's
wrongness could surface. Fixing that crash made this second, independent
bug newly visible.

## Fix direction

1. Determine raku's actual non-match value for a when-only block in
   value/list context (re-run the three probes above, plus a plain
   `given`/`when` outside any map/grep, and read
   `raku-doc/doc/Language/control.rakudoc`'s `given`/`when` section).
2. Distinguish "the block genuinely produced no value" (falls through a
   non-matching `when` chain) from "the block's last statement is a bare
   `$_`, meant to reflect back a possibly-mutated topic for rw semantics"
   — the current shared `.or_else` fallback conflates the two. This likely
   needs compile-time knowledge (does the block contain a `when`/`default`
   chain as its tail statement?) or a distinct runtime marker separate
   from "nothing on the stack".
3. Apply consistently across all four `Ok(())` arms found by
   `grep -n "or_else(|| vm.env().get(\"_\")" src/runtime/resolution_map_grep*.rs`.

Risk: medium — this fallback is load-bearing for rw map/grep (`<->`,
`.=map`), so the fix must not break that (`t/` has existing rw map/grep
pins — run them after any change).

## Verification

- The two repro one-liners above should match raku's actual (re-verified)
  output.
- Existing rw map/grep pins (`grep -rl "map_over_items_rw\|<->" t/`)
  still pass.
- Add a `t/` pin covering the non-match case for both `.map` and `.grep`
  with a `when`-only block.

## Deep-dive investigation (2026-08-10)

Step 1 of the fix direction is now DONE. Everything below was verified
against Rakudo v2026.06 on this machine and against the current source
tree (main, `52f217429`). The remaining work is purely mechanical; the
implementation plan below names every insertion point.

### Probe matrix (raku = Rakudo v2026.06 oracle; mutsu = current main)

| # | Probe (`raku -e '...'`) | raku output | mutsu today |
|---|---|---|---|
| 1 | `say (1..5).map({ when 2 { "two" } }).raku` | `(Bool::False, "two", Bool::False, Bool::False, Bool::False).Seq` | `(1, "two", 3, 4, 5).Seq` |
| 2 | `say (1..5).map({ when 2 { "two" } }).elems` | `5` | 5 |
| 3 | `my @a = (1,"a",2).map({ when Int { "int" } }); say @a.raku` | `["int", 0, "int"]` | `["int", "a", "int"]` |
| 4 | `say (1..5).grep({ when 2 { True } }).raku` | `(2,).Seq` | filters nothing |
| 5 | `my @a = (1,"a",2,"b").grep({ when Int { True } }); say @a.join(",")` | `1,2` | `1,a,2,b` |
| 6 | `say (1,2,3).first({ when 5 { True } }).raku` | `Nil` | `1` |
| 7 | `my $b = { when 2 { "two" } }; say $b(3).raku; say $b(2).raku` | `Bool::False` / `"two"` | `Nil` / `"two"` |
| 8 | `$b(3).WHAT` / `?($b(3))` / `so $b(3)` | `Bool` / `False` / `False` | — |
| 9 | `my $b = { when "b" { "hit" } }; say $b("a").raku` | `Bool::False` | — |
| 10 | `my $b = { when /x/ { "hit" } }; say $b("abc").raku` | `Bool::False` | — |
| 11 | `my $b = { when * > 5 { "big" } }; say $b(3).raku` | `Bool::False` | — |
| 12 | `my $b = { when 2\|3 { "j" } }; say $b(5).raku` | `Bool::False` | — |
| 13 | `my $b = { when Str { "s" } }; say $b(3).raku` | `0` | — |
| 14 | `my $b = { when Int:D { "d" } }; say $b("a").raku` | `0` | — |
| 15 | `class Foo {}; my $b = { when Foo { "f" } }; say $b(3).raku` | `0` | — |
| 16 | `constant T = Int; my $b = { when T { "t" } }; say $b("a").raku` | `0` | — |
| 17 | `subset Even of Int where * %% 2; { when Even { "e" } }` on 3 | `0` | — |
| 18 | `{ when Positional { "p" } }` on 3 | `0` | — |
| 19 | `enum Color <R G B>; my $b = { when R { "r" } }; say $b(5).raku` | `Bool::False` | — |
| 20 | `my $b = { default { "d" } }; say $b(3).raku` | `"d"` | — |
| 21 | `{ when 2 { "two" }; default { "d" } }` on 3 / on 2 | `"d"` / `"two"` | — |
| 22 | `{ when 2 { proceed }; when Int { "int" } }` on 2 / 9 / `"a"` | `"int"` / `"int"` / `0` | — |
| 23 | `{ when Int { succeed "S" } }` on 2 / `"a"` | `"S"` / `0` | — |
| 24 | `{ when 2 { "two" }; "after" }` on 3 / on 2 | `"after"` / `"two"` | same (verified) |
| 25 | `{ my $x = 42; when 2 { "two" } }` on 3 / on 2 | `Bool::False` / `"two"` | — |
| 26 | `for 1..4 { when 2 { say "two" } }; say "done"` | `two` then `done` | same |
| 27 | `(1..4).map(-> $x { when 2 { "two" } }).raku` | `(0, 0, 0, 0).Seq` (!) | — |
| 28 | `my @a = 1..4; @a.=map(<-> $x { when 2 { "two" } }); say @a.raku` | `[0, 0, 0, 0]` | — |
| 29 | `my $b = { ; }; say $b(3).raku` | `Nil` | — |
| 30 | `my $b = { when 2 { proceed } }; say $b(2).raku` | crash: `No such method 'raku' for ... VMNull` | — |
| 31 | `$_ = False; my $a = do { when .so { "foo" } }; say $a.raku` | `Bool::False` | `Any` |
| 32 | `say (given 3 { when 2 { "two" } }).raku` | `Bool::False` | `Nil` |
| 33 | `say ("a" ~~ Int).raku` (plain smartmatch, for contrast) | `Bool::False` | — |

Notes on the odd rows:

- Row 27/28: a pointy block with an explicit parameter does NOT bind `$_`
  to the parameter — `when 2` tests the *outer* `$_` (`Any`), so every
  element yields the non-match value, `"two"` never fires, and even the
  0-vs-False flavor flips (a Rakudo internal artifact on an undefined
  topic). Pathological shape; do not chase the flavor here.
- Row 30: a matched `when` whose body consists only of `proceed`, falling
  off the end of the block, produces literal VMNull in Rakudo — i.e.
  Rakudo itself has no defined value for that shape. Any defined falsy
  value we produce there is acceptable.
- Row 33 is the key to the flavor split: plain `"a" ~~ Int` returns
  `Bool::False`, but the failed `when Int` yields Int `0` — because
  Rakudo's optimizer compiles a type-object `when` matcher down to
  `nqp::istype`, whose native-int 0 result boxes as `Int` and leaks out
  as the statement value. It is an implementation artifact, but it is
  observable, stable across type shapes (classes, roles, subsets,
  `:D`/`:U` smileys, `constant` aliases — rows 13-18), and it is what the
  ticket's own repro pins, so we reproduce it.

### The confirmed rakudo rule (crisp)

A non-matching `when` statement does not "produce nothing": it evaluates
to the falsy result of its own condition test, and control simply falls
through to the next statement. If the `when` (or a `when`/`default`
chain) is the block's final statement and no branch matches, that falsy
test result IS the block's value — canonically `Bool::False`, except
that Rakudo yields Int `0` when the matcher is a type object (any class,
role, subset, or smiley-suffixed type — the `nqp::istype` boxing
artifact). The value is defined, is not `Nil`, is not `Empty` (`.elems`
of the mapped Seq stays the full length, row 2), and is never the topic.
A matched `when` instead `succeed`s out of the enclosing block with its
body's value. `raku-doc/doc/Language/control.rakudoc` confirms this
explicitly (lines 537-546): with `$_ = False`, `$a = do when .so
{ "foo" }` leaves `$a == False` — "the block is not abandoned since the
comparison is false, so `$a` will actually get a value."

This dissolves the ticket's earlier "contradiction": the `0` seen in the
map probe was the type-matcher flavor (`when Int` → istype → Int 0); the
`False` seen in the direct-call probe was the value-matcher flavor
(`when 2` → Bool::False). Same rule, two matcher kinds — there is no
extra boolean coercion in map/grep.

### mutsu today — where the wrong value comes from

- `exec_when_op` (`src/vm/vm_given_when_ops.rs:324-412`): on non-match
  it pops the cond value, pushes NOTHING, and falls to `*ip = end;
  Ok(())` (line 410). On match it raises a succeed signal carrying the
  body value (line 404-407); the four fast-path sites already absorb
  that in their `is_succeed` arms (that was the sibling fix).
- The four `Ok(())` fallback sites (`grep -n 'or_else(|| vm.env().get' src/runtime/resolution_map_grep*.rs`):
  - A. `eval_map_over_items` — `src/runtime/resolution_map_grep.rs:462-466`
  - B. `try_first_match_batched` — `src/runtime/resolution_map_grep.rs:725-729`
  - C. `eval_map_over_items_rw` — `src/runtime/resolution_map_grep_rw.rs:271-275`
  - D. `eval_grep_over_items_with_mutated` — `src/runtime/resolution_map_grep_rw.rs:536-540`
  Each computes `vm.last_stack_value().cloned().or_else(|| vm.env().get("_").cloned()).unwrap_or(Value::NIL)`.
  `last_stack_value()` (`src/vm/vm_core_helpers.rs:248`) is `Some` only
  when the stack holds exactly one value. A non-matching tail `when`
  leaves the stack empty (the block body is compiled by
  `Compiler::compile` → `compile_unit`, whose tail-statement match at
  `src/compiler/mod.rs:2595-2687` has no `Stmt::When` arm — it falls to
  plain `compile_stmt`, and `OpCode::When` pushes nothing on non-match).
  So the `.or_else` topic fallback fires and the ORIGINAL ITEM leaks out.
- The topic fallback is genuinely load-bearing for other tail shapes
  (blocks whose tail compiles as a sink statement and whose meaning is
  "reflect the possibly-mutated `$_`", plus the rw `<->`/`.=map`
  writeback machinery around it), so it cannot simply be replaced.
  Note the rw WRITEBACK itself (`topic_key`/`rw_cell`/`topic_source_key`
  env entries) is independent of this value fallback — the fix below
  does not touch it.
- The same wrong-value disease exists OUTSIDE map/grep (rows 7/31/32:
  direct block call → `Nil`, `do { when ... }` → `Any`, bare
  `given`+`when` → `Nil`; raku gives `False` in all three), but through
  different fallback paths (closure-call result handling,
  `compile_block_inline`'s trailing `LoadNil` at
  `src/compiler/helpers_block_inline.rs:387`). Out of scope here — see
  "Follow-up" below.

### Mechanism decision

**Compile-time tail gate + runtime value marker.** Two pieces, both tiny:

1. A compile-time predicate answers "is this block a bare `when`/`default`
   chain in tail position?" — computable at the four sites directly from
   the AST they already hold (`normalized_body`), no new opcode, no
   compiler flag, no change to compiled code. This gates the carve-out so
   the topic fallback keeps firing for every other tail shape (the rw
   safety constraint).
2. A runtime marker field supplies the exact non-match VALUE. Only
   `exec_when_op` knows the failed matcher's runtime value (`cond_val`),
   which is what decides the Int-0-vs-False flavor exactly — including
   `constant T = Int` aliases (row 16) that no AST inspection can
   classify. On non-match it records
   `Some(0 or False)`; the four sites consume it only when the gate says
   the tail is a when chain and the stack is empty.

Why not the alternatives:

- *Pure compile-time value* (classify the tail `When`'s cond AST:
  `BareWord("Int")` → 0, else False): cannot get rows 16-18 right
  (`constant` type aliases, subsets resolved at runtime), and would
  misclassify enum values (`when R`, row 19 — `BareWord` but NOT a type).
- *The general fix* (make `exec_when_op` push the falsy value on the
  stack on non-match, fixing given/do/direct-call too): architecturally
  the right end state, but its blast radius is every statement-sequence
  compile site: `compile_unit:2679-2686` already Pops after a non-last
  `when` (currently a no-op because nothing was pushed — it would become
  load-bearing), `compile_block_inline` does NOT pop non-last whens and
  appends `LoadNil` after a tail when (helpers_block_inline.rs:387, which
  would bury the pushed value), and loop bodies would accumulate one
  value per non-matching iteration unless every loop op truncates the
  stack. That is a separate, careful campaign — see Follow-up.

### Implementation plan (step by step)

All line numbers refer to main @ `52f217429`; re-locate with the quoted
context if drifted.

**Step 1 — new Interpreter field.**
In `src/runtime/mod.rs`, directly after `when_matched: bool,` (line
1142), add:

```rust
    /// The falsy value the most recent non-matching `when` evaluated to:
    /// rakudo yields Int 0 for a type-object matcher (nqp::istype boxing)
    /// and Bool::False otherwise. Consumed (and cleared) only by the
    /// inline map/grep/first fast paths, gated on the block's tail
    /// statement being a `when`/`default` chain — see
    /// `resolution_map_grep::tail_is_when_chain`.
    pub(crate) when_nonmatch_value: Option<Value>,
```

Add `when_nonmatch_value: None,` to both struct initializers: next to
`when_matched: false,` at `src/runtime/runtime_init.rs:1871` and
`src/runtime/runtime_thread.rs:459`. (Pattern precedent: the
`pub(crate) state_scope_id` field, mod.rs:2143, set directly on `vm` by
the same fast paths.)

**Step 2 — record the value in `exec_when_op`.**
`src/vm/vm_given_when_ops.rs`: the function ends (lines 381-411) as
`if matches { ... } ; *ip = end; Ok(())`. Give the `if` an `else` so the
marker is set ONLY on the non-match path (not on match+`proceed`):

```rust
        if matches {
            /* ... existing body unchanged ... */
        } else {
            // A failed `when` evaluates to the falsy result of its test
            // (control.rakudoc: "the block is not abandoned since the
            // comparison is false"). Rakudo boxes a type-object matcher's
            // nqp::istype result as Int 0; everything else is Bool::False.
            // Nothing is pushed (stack discipline unchanged); the inline
            // map/grep/first fast paths read this to distinguish "tail
            // when matched nothing" from "no value produced".
            self.when_nonmatch_value = Some(if cond_val.is_package_value() {
                Value::int(0)
            } else {
                Value::FALSE
            });
        }
        *ip = end;
        Ok(())
```

`cond_val` is the local popped at the top of the function and is still
live. `is_package_value()` (`src/value/view.rs:634`) is the tag probe for
`ValueView::Package` type objects; `when Int:D` parses as
`BareWord("Int:D")` and evaluates to a Package value too (the smartmatch
`(_, ValueView::Package(type_name))` arm at
`src/runtime/seq_helpers/smart_match.rs:1411` parses the smiley suffix
off the package name, so the representation is settled). Enum values are
not Package values, so row 19's False comes out right. Do NOT touch
`exec_default_op` (a `default` has no non-match path).

**Step 3 — tail predicate helper.**
In `src/runtime/resolution_map_grep.rs`, after
`normalize_tail_stmt_for_value` (line 162), add:

```rust
/// Whether the body's last non-`SetLine` statement is a `when`/`default`
/// — i.e. the block is a bare when-chain in tail position. When such a
/// block matches NO branch, it evaluates to the failed test's falsy
/// value (`Interpreter::when_nonmatch_value`), NOT to the topic: the
/// topic fallback exists only to reflect a possibly-mutated `$_` for rw
/// semantics and must not fire for a when-tail block.
pub(super) fn tail_is_when_chain(body: &[crate::ast::Stmt]) -> bool {
    use crate::ast::Stmt;
    body.iter()
        .rev()
        .find(|s| !matches!(s, Stmt::SetLine(_)))
        .is_some_and(|s| matches!(s, Stmt::When { .. } | Stmt::Default(_)))
}
```

**Step 4 — the four sites.** At each site, right after
`let (code, compiled_fns) = compiler.compile(&normalized_body);`
(map_grep.rs:356 and 662; map_grep_rw.rs:150 and 414), add:

```rust
let tail_is_when = tail_is_when_chain(&normalized_body);
```

(in the rw file: `super::resolution_map_grep::tail_is_when_chain(...)`,
same qualification style as its `normalize_tail_stmt_for_value` call).

Per iteration, clear the marker so a value recorded by an unrelated
`when` (a nested `given` in a previous iteration, an enclosing chain)
cannot leak in — insert `vm.when_nonmatch_value = None;` immediately
before each `let saved_when_matched = vm.when_matched();` /
`match vm.run_reuse(...)` (map_grep.rs:459, map_grep.rs:722,
map_grep_rw.rs:268, map_grep_rw.rs:533; site B has no
saved_when_matched-free spot issues — put it just above line 722).

Then change each `Ok(())` value computation from

```rust
let val = vm
    .last_stack_value()
    .cloned()
    .or_else(|| vm.env().get("_").cloned())
    .unwrap_or(Value::NIL);
```

to

```rust
let val = vm
    .last_stack_value()
    .cloned()
    .or_else(|| tail_is_when.then(|| vm.when_nonmatch_value.take().unwrap_or(Value::FALSE)))
    .or_else(|| vm.env().get("_").cloned())
    .unwrap_or(Value::NIL);
```

(sites B and D bind the variable as `pred` instead of `val`; edit
identically at map_grep.rs:462-466, map_grep.rs:725-729,
map_grep_rw.rs:271-275, map_grep_rw.rs:536-540). The
`unwrap_or(Value::FALSE)` inside covers the only way to reach Ok(()) with
an empty stack, a when tail, and no marker: a MATCHED tail `when` whose
body `proceed`s off the end of the block — the shape where Rakudo itself
returns VMNull garbage (row 30), so a defined `False` is the sane choice.
The rw writeback closures / `topic_source_key` reads at these sites are
NOT touched — they run independently of the value computation, so
`<->`, `.=map`, and `$_`-mutation writeback are structurally unaffected.

**Step 5 — tests** (see test plan below), then `cargo fmt`,
`cargo clippy -- -D warnings`, `make test`, targeted roast files, PR per
the normal workflow.

### Test plan

`t/when-only-block-nonmatch-value.t` (new; `.join` is used deliberately —
it distinguishes the flavors, since `0.Str` is `"0"` and `False.Str` is
`"False"`):

```raku
use Test;

plan 11;

# Rakudo rule: a block whose tail when-chain matches nothing evaluates to
# the failed test's falsy value — Int 0 for a type-object matcher,
# Bool::False otherwise. Never the topic, never Nil/Empty.

is (1, "a", 2).map({ when Int { "int" } }).join(","), "int,0,int",
    'map: non-matching item yields Int 0 for a type matcher';

is (1..5).map({ when 2 { "two" } }).join(","), "False,two,False,False,False",
    'map: non-matching item yields False for a value matcher';

is (1, "a", 2, "b").grep({ when Int { True } }).join(","), "1,2",
    'grep: when-only block filters items matching no branch';

is (1..5).grep({ when 2 { True } }).join(","), "2",
    'grep: value-matcher when-only block filters too';

nok (1, 2, 3).first({ when 5 { True } }).defined,
    'first: a when-only predicate matching nothing finds nothing';

is (1, "a").map({ when Int { "int" }; default { "other" } }).join(","), "int,other",
    'map: a default branch supplies the non-match value instead';

is (2, 3).map({ my $x = 42; when 2 { "two" } }).join(","), "two,False",
    'map: a statement before the tail when does not change the rule';

is (2, 3).map({ when 2 { "two" }; "after" }).join(","), "two,after",
    'map: a when followed by another statement falls through to it';

{
    my @out;
    for 1..4 { when 2 { @out.push("two") } }
    is @out.join(","), "two", 'for: when-only loop body still fires only on the match';
}

{
    my @a = (1, 2, 3);
    @a .= map({ when Int { $_ * 10 } });
    is @a.join(","), "10,20,30", 'rw .=map with a matching when still writes back';
}

todo "direct block call goes through the closure-call fallback, not the map/grep fast path; needs the general exec_when_op fix (see follow-up)";
is-deeply { when 2 { "two" } }(3), False,
    'direct call: non-matching when-only block evaluates to False';

done-testing;
```

Expected-output cross-checks (all verified against raku on 2026-08-10):
rows 1-8 of the plan correspond to probe rows 3, 1, 5, 4, 6, 21, 25, 24.

### Regression hazards — run these after the change

`t/` (all touch when-in-block or the same fast paths):

- `t/map-when-succeed.t` — the sibling succeed-signal fix's pin; its 5
  tests all go through the same four Ok/succeed arms. Test 3
  (`when Int { "int" }; default { "other" }`) and test 4 (rw `.=map`)
  are the closest to this change.
- `t/proceed-succeed.t`, `t/when-succeed-innermost-block.t`,
  `t/when-block-value-not-sunk.t` (sink-warning analysis — compiler
  untouched, but it pins when-block value semantics),
  `t/given-when-tail-assign-value.t`, `t/given-when-tail-if-value.t`,
  `t/when-value-through-block-local.t`.
- rw/writeback pins: `t/eager-map-grep-captured-writeback-coherence.t`,
  `t/for-multiparam-copy-rw.t`, `t/method-rw-param-writeback-coherence.t`,
  plus the full `grep -rl "<->" t/` set.
- `t/whatever-map-topic.t` (outer-topic binding in map blocks — probe
  row 27's shape).

Whitelisted roast (run with `MUTSU_FUDGE=1 prove -e 'target/debug/mutsu'`):

- `roast/S04-statements/when.t` (96 lines, whitelisted)
- `roast/S04-statements/given.t` (whitelisted)

then let CI's full `make roast` be the comprehensive net.

Known cosmetic divergence accepted: probe row 27/28's pointy-with-param
shape (`-> $x { when 2 ... }` testing the outer `Any` topic) yields
False-flavored values in mutsu where Rakudo shows 0 — an undefined-topic
Rakudo artifact on a pathological shape; both are falsy.

### Follow-up (separate ticket when implementing)

The same non-match value is still wrong OUTSIDE map/grep: a direct block
call gives `Nil`, `do { when ... }` gives `Any`, and a bare
`given`/`when` gives `Nil`, where raku gives `False`/`0` (probe rows 7,
31, 32). The correct general fix is to make `exec_when_op` deliver the
falsy value everywhere (e.g. push it on the stack on non-match), which
requires a stack-hygiene sweep: `compile_unit:2679`'s after-when `Pop`
becomes load-bearing, `compile_block_inline` needs the same non-last
`Pop` plus suppression of its trailing `LoadNil`
(helpers_block_inline.rs:387) after a when tail, and loop bodies must not
accumulate one value per non-matching iteration. File it as
`todo/tickets/when-nonmatch-value-outside-map-grep.md` (the probe rows
above are its repro) when this ticket's fix lands; the
`when_nonmatch_value` marker added here is a stepping stone it can reuse.
