# Remaining language-feature gaps that no roast file whitelists

Extracted from PLAN.md §4 (2026-08-02). These are real spec gaps, but none of them flips a roast
file to passing on its own — which is why they never got picked up. Grouped here so they stay
visible without occupying the plan outline.

**Items 1 and 3 verified resolved 2026-08-14** (see below); only item 2 remains open.

## 1. Multi-line feeds — resolved, and the original repro was invalid feed syntax to begin with

The repro this item shipped with,

```raku
my @r = (1, 2, 3)
    ==> map({ $_ * 2 })
    ==> sort();
```

is not actually valid feed usage in raku: `=` binds tighter than `==>`, so `my @r = (1, 2, 3)` is a
complete assignment on its own, and the `==> map(...) ==> sort()` chain operates on — and then
discards — the assignment expression's value. Verified against real `raku`: this exact snippet prints
`[1 2 3]` (i.e. the feed never touches `@r`) in **both** `raku` and mutsu, single-line or multi-line —
so there was never a discrepancy here, mutsu-vs-raku, for this shape.

The correct feed form puts the target at the END of the chain (`SOURCE ==> STEP ==> ... ==> my
@target;`), and that form works correctly across multiple lines in mutsu today:

```raku
(1, 2, 3)
    ==> map({ $_ * 2 })
    ==> sort()
    ==> my @r;
say @r;   # both raku and mutsu: [2 4 6]
```

Whether the `!ws_before.contains('\n')` guard this item originally named ever mattered for a *correct*
multi-line feed is unclear from this round — the working case above didn't need bisecting once a valid
repro was used. Closing as resolved; re-open with a genuine failing repro if one turns up.

`==>>` / `<<==` and `~<` / `~>` are still unimplemented/unspecified **in Rakudo itself**, so they still
cannot be started (no oracle) — unchanged from the original filing.

## 2. Typed-exception gaps needing compile-time scope analysis

- ~~strict-mode undeclared-variable detection~~ — **resolved 2026-08-17**, see below.
- cross-`EVAL` detection of class redeclaration — still open
- `X::Redeclaration::Outer` — still open

The remaining two bullets need compile-time scope analysis that mutsu does not currently perform;
each is non-trivial on its own and is left for a future session.

### strict-mode undeclared-variable *read* detection — resolved 2026-08-17

```raku
use strict; my $x = $y; say "no error";
```

`raku` dies at compile time with `X::Undeclared` ("Variable '$y' is not declared..."); mutsu used to
exit 0 and print `no error` — `$y` was silently read as Nil.

`use strict` already had a WRITE-side check (`SetGlobal` in `src/vm/vm_exec_dispatch.rs`) but no
symmetric READ-side check. Added one to the tail of the `OpCode::GetGlobal` handler — the one place a
scalar-variable read falls through every real store (env, unit/package/module lexicals, `our`-vars,
per-call state, escaping-`our` captures, ...) and would otherwise silently yield `Value::NIL` for a
name that resolves nowhere. When `self.strict_mode` is set and the name isn't one of several
pseudo-variable / dynamic-scope shapes `GetGlobal` also carries (dynamic vars `$*FOO`, compile-time
pseudo vars `$?FOO`, internal `__`-prefixed temporaries, `::`-qualified names, bare `$!`/`$/`, and
`$0`/`$1`/... positional captures — see `Interpreter::strict_read_exempt` in
`src/vm/vm_value_helpers.rs`), it now raises the same `X::Undeclared` the write side already throws
(`strict_undeclared_error`).

Array/hash sigil reads (`GetArrayVar`/`GetHashVar`) are separate opcodes and were not touched — the
ticket's repro and the write-side precedent are both scalar-only.

Verified: full local `t/` suite (3191 files, 29730 tests) clean; targeted roast sweep
(`S02-names-vars`, `S04-declarations`, `S06-signature`, `S12-*`) shows only pre-existing,
unrelated failures (none mention `Undeclared`); the dedicated `roast/S02-names/strict.t` (already
whitelisted) still passes in full. New pinned test: `t/strict-undeclared-variable-read.t`.

## 3. `exits-ok($code, $exit, $reason)` — already implemented, ticket was stale

Verified 2026-08-14: `Interpreter::test_fn_exits_ok`
(`src/runtime/test_functions/eval_exception.rs`) already exists, is registered in
`TEST_MODULE_EXPORTS` and `is_test_function_name`, and matches `raku`'s behavior exactly:

```raku
use Test;
plan 3;
exits-ok({ exit 4 }, 4, "exits with 4");   # ok
exits-ok({ exit 5 }, 4, "wrong code");     # not ok, both raku and mutsu
exits-ok({ 1 }, 4, "does not exit");       # not ok, both raku and mutsu
```

Not clear when this landed relative to the ticket's 2026-08-02 filing; no dedicated `news/` entry was
found for it, so it may have shipped as an incidental part of a different Test-completeness slice.
Nothing left to do here.

## 4. `:D` / `:U` DefiniteHow coercion

`6.c/APPENDICES/A04-experimental/01-misc.t` sits at 16/19 on this (`Target:D(Source:U)`). Tracked
with the file in [TODO_roast/BLOCKERS.md](../../TODO_roast/BLOCKERS.md); listed here only so the
feature name is searchable.
