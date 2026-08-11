# A for-loop parameter inside an escaping closure reads the same-named captured OUTER lexical instead of the iteration value

## Minimal deterministic repro (11 lines, no dependencies)

```raku
sub make() {
    my $i = -1;
    my @parts = 1,;
    for 1..3 { $i++ }
    -> {
        for @parts -> $i {
            say "i=", $i;
        }
    }
}
make()();
```

raku prints `i=1` (the iteration value); mutsu prints `i=2` (the value the
OUTER `my $i` had when the closure was created). The closure captures the
enclosing `$i` as a free... it is NOT even a free variable of the closure —
the inner `for @parts -> $i` declares its own parameter — yet the captured
env entry for "i" wins over the per-iteration binding when the closure body
executes. Repro file: `tmp/loop-param-captured-shadow.raku`.

Sensitivities (verified): the outer `$i` must be MUTATED after
initialization (`$i++` in a loop — matching the shape where the outer `$i`
is a counter); the closure must escape `make()` and be invoked later.

## Real-world failure: `t/http-router-named-urls.t` (Cro::HTTP), 2 subtests

`Cro::HTTP::Router::LinkGenerator.rakumod`'s `signature-to-sub` builds
`@path-parts` (static segments) / `@fn-parts` (variable-segment indices)
using a counter `my $i = -1; for $s.params[] { ...; $i++; ... }`, then
returns the closure

```raku
-> *@args, *%nameds {
    my @result = @path-parts;
    for @fn-parts -> $i {
        @result[$i] = @args.shift;
        ...
    }
    ...
}
```

Under mutsu, the closure's `for @fn-parts -> $i` sees `$i` frozen at the
BUILD counter's final value: for route `-> 'search', $category, :$query`
(3 params, counter ends at 2) every iteration runs with `$i == 2`, so
`abs-link('qs', 'tools', ...)` produces `["search", Any, "tools"]` →
`/search//tools?...` instead of `/search/tools?...` ("Escaped named
param"); for `-> 'product', $id, 'docs', $file` (counter ends at 3) both
iterations write index 3 — the second overwrites the first, dropping `42`
→ `/product//docs/foo%20bar.jpg` ("Escaped positional"). Instrumented
shadow-lib trace confirmed: `S2S fn-parts=[1]` at build, `GEN loop i=2` at
call; `fn-parts=[1, 3]` at build, `i=3, i=3` at call — exactly the outer
counter's final values.

## Root cause — VERIFIED 2026-08-11 (gdb, ADR-0025 diagnosis session)

The ticket's original "resolving by NAME through the merged captured env"
guess was close but the mechanism is sharper: **the closure body's `$i`
reads are `GetUpvalue` ops that bypass the frame env AND the loop binding
entirely.** Chain:

1. A for-loop parameter only gets a local slot if the name ALREADY has one
   (`compiler/stmt.rs`, the `param_local = self.local_map.get(...)` lookup
   — it never allocates). In the repro's closure, "i" has no prior slot, so
   the param is an env-only binding and the name is absent from the
   compiled body's `own` set.
2. `compute_free_vars` therefore classifies the body's `$i` reads as FREE
   variable reads, and the loop-binding writes happen inside the ForLoop
   opcode exec (no name-write op), so "i" also looks read-only.
3. `compute_upvalues` rewrites the pure reads to `GetUpvalue` — verified
   with `rust-gdb -batch -ex 'break src/vm/vm_exec_dispatch.rs:204'` (the
   GetUpvalue arm): it fires for each `i=` print in the repro. The read
   resolves against the closure's captured env/upvalue array (the outer
   `$i` counter cell), never seeing the ForLoop's per-iteration binding.

Fix direction (compiler-side): a for-loop parameter must be an OWN binding
of the compiled code that contains the loop — either allocate a local slot
for slotless loop params (making body reads GetLocal and restoring
`param_local` sync), or at minimum exclude loop-param names from
`free_var_syms`/upvalue eligibility for the enclosing body (the
`expr_declared_syms`/`my_declared_enum_sym` precedent in
`compute_free_vars`). The slot route is the sound one — exclusion alone
still leaves the body reading a name the merge may have installed.

## Relationship to other open findings

Same family symptom-wise as
`todo/deep/closure-read-only-capture-loses-to-caller-env-same-name.md`
(ADR-0025), but mechanically independent: that one is about the captured-env
merge / cell boxing; this one is a compiler scoping bug. ADR-0025 slices
widen cell prevalence, so this repro must be re-run when its slices land
(a captured cell installed under "i" makes the GetUpvalue read a live cell
— still wrong, and a ForLoop binding that wrote through such a cell would
corrupt the outer counter).

**Update 2026-08-11: the write-corruption direction fired in CI and is now
guarded for MULTI-params.** ADR-0025 slice 1 boxed the captured vow in
roast `integration/advent2013-day14.t`'s `config_combiner`, and
`for %kvs.kv -> $k, $v`'s `Stmt::Assign`-based binding wrote iteration
Strs through the vow's cell (`$v.keep` then hit a Str; file hung). Fixed
in `exec_for_loop`'s multi-param prep (`vm_for_loop_body.rs`): a scalar
multi-param name currently bound to a cell is severed for the loop's
duration (save/restore already preserves the cell). Pin: test 7 of
`t/closure-capture-instance-cell.t`. Still open here: the READ-side
GetUpvalue bypass (the 11-line single-param repro still prints `i=2`),
and single-param loops were not audited for an equivalent write-through
(their bind is native env insert, believed rebind-safe — verify when
fixing).

## Verification (once fixed)

- The 11-line repro prints `i=1`.
- `t/http-router-named-urls.t` "Escaped named param" / "Escaped
  positional" pass (the file's rc=124 timeout at the end is a SEPARATE,
  still-undiagnosed issue — see the note in BLOCKERS/cro handoff; do not
  expect notok=0 rc=0 from this fix alone).
- roast: no regression in S04-statements/for*.t, S06-*/closure*.t.
