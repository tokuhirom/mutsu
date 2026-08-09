# Closure literal passed as a method-call NAMED argument is not marked escaping — captured scalar goes stale across threads

## Affected tests
- `t/http-session-inmemory.rakutest` subtest 13 ("Session expires appropriately"): expected `'Visit 1'`, got `'Visit 4'`
- `t/http-session-persistent.rakutest` subtest 13 (same name): expected `'Visit 1'`, got `'Visit 4'`

The tests build the session middleware with `…new(expiration => Duration.new(60*30), now => { $fake-now })` and then advance `$fake-now += Duration.new(...)` on the main thread. The `&!now` closure, invoked on the server's worker threads, keeps returning the ORIGINAL `$fake-now`, so sessions never expire. (Subtests 11/12 pass either way — only 13 actually detects expiry.)

## Repro (verified)
`tmp/repro-stale-w4.raku` — no Cro needed:

```raku
my $x = 1;
class Store {
    has &.now is required;
    method read() { &!now() }
}
my $store = Store.new(now => { $x });
my $cmd = Channel.new;
my $out = Channel.new;
my $w = start {
    for ^2 { $cmd.receive; $out.send($store.read()) }
}
$cmd.send(1); say "first (expect 1): ", $out.receive;
$x = 42;
$cmd.send(1); say "second (expect 42): ", $out.receive;
await $w;
```

- raku: `first 1` / `second 42`
- mutsu (release): `first 1` / `second 1` (stale)

Discriminating variants (all verified):
- `tmp/repro-stale-w9.raku`: same via an ordinary method `$store.set(now => { $x })` — FAILS too (not `.new`-specific).
- `tmp/repro-stale-w8.raku`: named-arg closure to a plain SUB (`stash(now => { $x })`) — PASSES (the sub-call path already handles it).
- `tmp/repro-stale-w7.raku`: `my &getter = { $x }; Store.new(now => &getter)` — PASSES (assignment marks it escaping, `$x` gets a cell).
- `tmp/repro-stale-w5.raku`: single-threaded W4 — PASSES (the by-name coherence lane hides the missing cell on one thread).
- Fresh `start { $store.read() }` after the mutation sees 42 (`tmp/repro-stale-w6.raku`) — each spawn re-snapshots, which is why short-lived-thread tests never caught this. Long-lived server threads (Cro) see the spawn-time snapshot forever.

## Root cause
Escape analysis marks closure-literal call arguments escaping so their captured-and-mutated lexicals get shared `ContainerRef` cells (`box_captured_lexicals`, `src/vm/vm_register_ops.rs:753`; policy comment in `src/compiler/helpers_call_args.rs:111-126`).

The FUNCTION-call compile path unwraps a fat-arrow named argument before testing for a closure literal — `src/compiler/expr_call.rs:1428-1436`:

```rust
let value_expr = match arg {
    Expr::Binary { op: TokenKind::FatArrow, right, .. } => right.as_ref(),
    other => other,
};
let escaping_args = is_start || Self::is_closure_literal_arg(value_expr);
```

The METHOD-call compile paths do NOT unwrap: `src/compiler/expr_method.rs:155-157` (and the second site at `expr_method.rs:560`) test `Self::is_closure_literal_arg(arg)` on the raw argument, so `now => { $x }` (an `Expr::Binary { op: FatArrow, .. }`, same AST for colonpair form `:now({ $x })`) is never a "closure literal", `escaping` stays false, no cell is created for `$x`, and the closure captures a by-value snapshot. Worker threads clone the env at spawn and never see later main-thread writes. (This is the residue of #5891, which fixed the sub-call path only.)

## Fix direction
In `src/compiler/expr_method.rs`, at both `is_closure_literal_arg` call sites (~line 156 and ~line 560), unwrap the fat-arrow named-argument value exactly as `expr_call.rs:1428-1436` does before testing, e.g.

```rust
let value_expr = match arg {
    Expr::Binary { op: TokenKind::FatArrow, right, .. } => right.as_ref(),
    other => other,
};
let arg_esc = esc && (Self::is_closure_literal_arg(value_expr) || matches!(...legacy names...));
```

`compile_method_arg_with_escape` already wraps the whole argument compile in `with_escape`, so no further plumbing is needed. Consider hoisting the unwrap into a shared helper next to `is_closure_literal_arg` (`src/compiler/helpers_call_args.rs:138`) so the two paths cannot drift again.

Risks: broader escaping means more boxing; the #2746 perf guard is preserved because only the literal closure value is marked, not the whole argument list. Watch the bench CI ctor/yaml rows. The `Pair.new` two-positional special case at `expr_method.rs:137-181` is unrelated (positional, not FatArrow) — don't touch it.

## Verification
- `tmp/repro-stale-w4.raku` and `tmp/repro-stale-w9.raku` print `second (expect 42): 42`.
- `t/http-session-inmemory.rakutest` subtest 13 passes (file 13/13 together with the sibling tickets).
- `t/http-session-persistent.rakutest` subtest 13 passes.
- W5/W6/W7/W8 controls unchanged. Add a `t/` pin, e.g. `t/closure-named-arg-method-escape.t`, from W4.
