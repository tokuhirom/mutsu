# The `do for LIST -> $a, $b { ... }` EXPRESSION form never populates `ForLoopSpec::multi_param_names`

## TL;DR

`src/compiler/helpers_do_expr.rs` (the compiler path for `for` used as an
expression — `do for ...`, or a `for` that is the tail value of `try`/`do`/a
sub body) hardcodes `multi_param_names: Vec::new()` and `rw_param_names:
Vec::new()` (`helpers_do_expr.rs:336,342`) when emitting `OpCode::ForLoop`,
regardless of how many pointy-block params the loop actually declared. The
STATEMENT form (`src/compiler/stmt.rs:2349-2352`) correctly collects
`multi_param_names` from the parsed `params` list. This means every
multi-param `do for LIST -> $x, $y { ... }` loses its parameter-name
metadata entirely.

## Impact

Any VM mechanism keyed off `spec.multi_param_names` is silently inert for
the `do for` expression form:

- The cross-thread bare-name-lane masking added for multi-param loops
  (`masked_multi_params`, #6081) never runs — so a `do for @list -> $x, $y {
  start { ... $x ... } }` spawning sibling threads is exposed to the same
  bare-name-lane collision class of bug as
  `todo/deep/concurrent-for-loop-siblings-cannot-share-a-bare-loop-param-name.md`
  / `docs/adr/0023-binding-provenance-spawn-capture.md`, and ADR-0023's fix
  (which also reads `spec.multi_param_names`, see `vm/vm_for_loop_body.rs`'s
  `loop_param_names` construction) cannot help either, because the names
  never reach `ForLoopSpec` at all on this path.
- Loop-parameter type constraints
  (`multi_param_type_constraints: Vec::new()` at the same call site) are
  similarly dropped for `do for`.

## Repro

```raku
class Widget { has $.id; }
my $a1 = Widget.new(id => 'A1');
my $b1 = Widget.new(id => 'B1');
my $a2 = Widget.new(id => 'A2');
my $b2 = Widget.new(id => 'B2');

my @promises = do for $a1, $b1, $a2, $b2 -> $x, $y {
    start {
        my @a;
        for 1..5 -> $i {
            await Promise.in(0.01);
            @a.push($x.id ~ '/' ~ $y.id);
        }
        @a.join(',');
    }
}
say (await @promises).join(' | ');
```

- `raku`: `A1/B1,A1/B1,A1/B1,A1/B1,A1/B1 | A2/B2,A2/B2,A2/B2,A2/B2,A2/B2`
- `mutsu` (main, `65844e560`, and unchanged by ADR-0023): `A2/B2,A2/B2,A2/B2,A2/B2,A2/B2 | A2/B2,A2/B2,A2/B2,A2/B2,A2/B2`
  (both threads converge on the LAST iteration's values).

The equivalent **statement** form (`for $a1, $b1, $a2, $b2 -> $x, $y { ...
push a start {} into an array ... }`, no `do`) is unaffected and — after
ADR-0023 — produces the correct `raku`-matching output. This isolates the bug
to the `do for` expression-form compiler path specifically, not to the
underlying spawn/capture machinery.

## Discovery context

Found while writing the ADR-0023 pin test
(`t/for-loop-param-start-sibling-isolation.t`): the ADR's required
multi-param acceptance-criteria variant, written using `do for` (mirroring
the original ticket's single-param repro style), failed even after the
ADR-0023 implementation. `rust-gdb` confirmed `spec.multi_param_names` is an
empty `Vec` at `vm/vm_for_loop_body.rs`'s `push_loop_local_scope` call site
for this exact repro, tracing back to the hardcoded empty `Vec::new()` in
`helpers_do_expr.rs`. Confirmed independent of ADR-0023 by reproducing on
`main` before that change.

## Fix sketch (not yet attempted)

Mirror `stmt.rs:2349-2352`'s `multi_param_names` collection (map `params`,
stripping a leading `\`) into `helpers_do_expr.rs`'s `ForLoop` emission, and
similarly wire `rw_param_names` / `multi_param_type_constraints` from
`params_def` the way the statement path does. Needs a look at why the
expression path built these separately from the statement path in the first
place (possibly an earlier partial-feature state) before assuming a
straight copy is correct — the statement path takes `params_def: &[ParamDef]`
where the expression path's `params_def` handling may differ.

## Verification (once fixed)

- The repro above should print `A1/B1,A1/B1,A1/B1,A1/B1,A1/B1 |
  A2/B2,A2/B2,A2/B2,A2/B2,A2/B2` under mutsu, matching `raku`.
