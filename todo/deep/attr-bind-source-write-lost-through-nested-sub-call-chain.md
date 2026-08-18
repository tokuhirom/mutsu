# A `$!x := $var` bind's shared cell is lost when the write to `$var` happens back through a nested sub/closure call chain (e.g. `lives-ok { ... }`)

## Repro (minimal, no `Test` module needed to trigger the underlying VM bug — see below for why the roast/`t/` regression only shows up under `MUTSU_REAL_TEST=1`)

```raku
use Test;
plan 2;
my $var = 100;
my class Klass2 { has $.x; method bind { $!x := $var } }
my $obj = Klass2.new;
lives-ok { $obj.bind() }, 'binding lives';   # real Test.rakumod's lives-ok
is $obj.x, 100, 'reads bound value';
$var = 200;
is $obj.x, 200, 'tracks source changes';     # FAILS: got 100, not 200
```

Confirmed against `raku` running the same real `Test.rakumod`: `raku` gets 200. mutsu
gets 100 only when `$obj.bind()` is invoked from *inside* a real (multi-frame)
sub call chain — `lives-ok(Callable $code, ...) { try { $code(); } ... }` —
not when called directly (`$obj.bind();` with no wrapper). Calling `.bind()`
directly, or through a hand-written single-level wrapper sub, does **not**
reproduce it; the bug needs a specific dynamic-call-frame shape (see below).

This is what regresses `t/has-attr-binding.t` test 6 ("binding $!x tracks
source changes") under `MUTSU_REAL_TEST=1`, discovered during the ongoing `t/`
residue sweep for `todo/deep/vendor-real-test-module.md`. It reproduces
identically against the *unmodified* vendored `modules/Rakudo-Core/lib/Test.rakumod`
(not just a hand-reduced copy), deterministically (100% of runs, with or
without a debugger attached — see "false lead" below).

## Why the no-twigil sibling test (`has $x`, tests 1-3 of the same file) passes

`t/has-attr-binding.t`'s first block uses `has $x` (a **sigilless** attribute,
no `.`/`!` twigil) and `$x := $var` inside `method bind`, called *directly*
(`$obj.bind();`, no `lives-ok` wrapper) — and it passes. Two structurally
different things are true there simultaneously, and only one needs to be
undone to build a matching failing case:

- `$x := $var` (sigilless attribute) compiles to a completely different bind
  mechanism than `$!x := $var` (twigil'd attribute) — see below.
- `.bind()` is called directly, one call level deep, not through a multi-frame
  sub/closure chain.

## Root cause, traced with `rust-gdb` breakpoints and one deliberate, single-build,
env-var-gated `eprintln!` pass (removed before writing this ticket — see
`docs/CLAUDE.md`'s debugging guidelines for when that's the sanctioned
fallback over repeated printf-and-rebuild)

1. **False lead, ruled out first:** running the *minimal* hand-reduced repro
   (a hacked copy of `Test.rakumod` under `-I`) under `rust-gdb` appeared to
   "fix" the bug (100% pass under gdb, 100% fail without). This was **not** a
   real race — it was a testing mistake: the gdb invocation was missing
   `MUTSU_REAL_TEST=1`, so it was silently running the *native* Test provider
   (which never showed the bug) instead of the real module. Once corrected,
   the bug reproduces identically with or without gdb attached, and
   `MUTSU_GC=off` / `MUTSU_JIT=off` / disabling ASLR (`setarch -R`) all made
   no difference. **This is a deterministic logic bug, not a race** — do not
   waste time on a timing-based theory here again.

2. `$!x := $var` (attribute-twigil target) compiles to the *generic*
   `:=`-bind machinery shared with plain `my $a := $b` — the `if let
   Some(source_name) = bind_source { ... }` block in
   `src/vm/vm_var_assign_set_local.rs` (~line 1300 onward). `$x := $var`
   (sigilless attribute, no twigil) does **not** go through this block at
   all — confirmed empirically (an env-var-gated print placed at the top of
   that block never fired for the sigilless case). The sigilless case must
   route through a different mechanism (candidates in the same file:
   `materialize_bound_slot_to_cell` / the sigilless-specific attribute
   cell-write helpers in `vm_var_assign_computed_attr.rs`) that this
   investigation did not fully trace — worth doing before attempting a fix,
   since it evidently gets this case right and may be the more robust model
   to extend rather than patching the generic path.

3. Inside the generic bind block, `source_in_outer_frame` (line ~1361) is
   computed as:
   ```rust
   let source_in_outer_frame = !is_percall_pseudo_var
       && self.call_frames.iter().any(|f| f.saved_env.contains_key(&resolved_source));
   ```
   For the `lives-ok { $obj.bind() }` repro this is `true` (traced:
   `call_frames.len() == 3` at bind time — one frame each for `lives-ok`,
   an intervening frame — very likely `try {}`'s own block-scope frame —
   and the closure `{ $obj.bind() }`). Per-frame, `contains_key_own_tier`
   (does *this* frame's own saved_env directly hold `"var"`, vs.
   `contains_key`, which chain-walks) was `true / false / true` across the
   three frames (index 1 — the presumed `try {}` frame — was `false`).

4. The container-promotion branch that's supposed to fire on
   `source_in_outer_frame` (line ~1522) *does* run, and *does* insert the
   freshly-minted `ContainerRef` into `frame.saved_env` for the frames whose
   `contains_key_own_tier` was `true`. But its companion `saved_locals`
   update is very likely dead code for this call shape:
   ```rust
   for (i, local_name) in code.locals.iter().enumerate() {
       if local_name == &resolved_source && i < frame.saved_locals.len() {
           frame.saved_locals[i] = container.clone();
       }
   }
   ```
   `code` here is **the currently-executing frame's own `CompiledCode`** (the
   `bind` method's) — not each ancestor `frame`'s own layout. `resolved_source`
   is `"var"`, a free variable captured by name into `bind`'s body; it is not
   one of `bind`'s own declared locals, so `bind`'s own `code.locals` almost
   never contains an entry literally named `"var"`, and this loop is a no-op
   for exactly the cross-frame-free-variable case it exists to handle. The
   comment immediately above it acknowledges the general hazard ("`code.locals`
   is this frame's slot layout, not the parent's") but the actual guard
   (`contains_key_own_tier`) does not fix the indexing mismatch — it only
   gates *whether* to attempt the (still-wrong) update.

   **This cannot be fixed by finding the "right" `code` to index with inside
   this loop**, because `VmCallFrame` (`src/vm.rs:287`) does not store a
   reference to its own owning `CompiledCode` at all — only a positional
   `saved_locals: Vec<Value>` with no accompanying name table. Making this
   loop correct needs either (a) `VmCallFrame` carrying an `Arc<CompiledCode>`
   (or at least a locals-name table) per frame, or (b) restoring `self.locals`
   from `saved_env` by name on every frame pop instead of trusting
   `saved_locals` positionally for cross-frame-affected slots — a real
   correctness invariant change, not a one-line patch.

5. Consistent with this: by the time execution returns to the top-level
   script frame and `$var = 200;` executes, `self.locals[0]` (the script's own
   slot for `$var`) is a **plain `Value::Int`**, never converted to a
   `ContainerRef` — traced via the `[setlocal]` instrumentation. Compare to
   the sigilless (`has $x`) case, where the equivalent slot *does* show
   `ContainerRef(Mutex { data: Int(_) })` at the same point. So `$var = 200`
   just overwrites the plain slot; whatever `ContainerRef` momentarily existed
   in an ancestor frame's `saved_env` was never reconciled back into the
   top-level frame's actual `self.locals`/`self.env` pair, and is gone once
   all four intervening calls (`lives-ok` / the presumed `try{}` frame /
   the closure / `bind`) have each popped and restored from their own
   (mostly stale) snapshots.

6. **Not fully explained:** the *very first* investigation into this (before
   finding the real vendored module reproduces it directly) found that in a
   hand-reduced copy of `Test.rakumod`, the bug only appeared once `proclaim`
   (called from *inside* `lives-ok`, after the `try{ $code(); }` block already
   ran) performed **any** extra method call of its own (even an unrelated one
   like `'x'.chars` or `$desc.Str`) — removing every statement from `proclaim`
   except a bare `return $cond;` made the *same* `lives-ok { $obj.bind() }`
   repro pass. This was not re-verified against the real, unmodified module
   (which was later found to fail deterministically with `proclaim` fully
   intact, obviously, since it's unmodified) — but it suggests the loss might
   not be fully "baked in" the moment `bind()` returns; an additional method
   call after the `try{}` block, still inside `lives-ok`'s own frame, may be
   what actually clobbers the (partially-propagated) container, rather than
   the propagation simply never having reached far enough in the first place.
   Whoever picks this up should re-examine step 4/5 with that in mind before
   assuming the loss happens entirely inside `bind`'s own return.

## Why this matters beyond one `t/` file

This is very likely the same failure family as
`todo/deep/control-warn-resume-list-assign-first-target-stale-on-repeat-call.md`
(also: real `Test` module, repeat call through a nested sub, a captured
outer variable ends up stale on a later read) — that ticket's own repro
notes "reproduces... only once `use Test;` has loaded the real, large
vendored module; an empty synthetic module does not trigger it, ruling out
'any module load' as the cause", which is the same shape of "only shows up
once you have enough real, non-trivial call-chain depth" symptom seen here.
Worth investigating together rather than independently.

It also directly explains the `vm_var_assign_set_local.rs` comment (lines
~226-229) that already, separately, documents: "the reconcile on, cell-sharing
would propagate a captured-outer write that the reconcile happens to drop
through some carriers (e.g. `lives-ok { ... }`)" — i.e. this exact failure
mode was known to exist in the abstract before this ticket pinned a concrete
repro to it. The `MUTSU_NO_BLANKET_RECONCILE` / `blanket_reconcile_disabled()`
toggle that comment describes as the intended escape hatch **no longer exists
in the code** (the migration it describes appears to have completed
unconditionally onto the "boxing" side for the `needs_cell_named_sub` case
only, which does not cover this attribute-bind case) — so there is currently
no way to opt into the more-correct behavior at all.

## Affected files

- `src/vm/vm_var_assign_set_local.rs` — the generic `:=`-bind block
  (~line 1300-1600), specifically the ancestor-frame propagation loop
  (~1579-1595) and its `source_in_outer_frame` decision (~1361-1365).
- `src/vm.rs:287` — `VmCallFrame`, which has no per-frame `CompiledCode`
  reference, making the propagation loop's `saved_locals` update
  structurally unable to find the right slot index.
- `src/vm/vm_var_assign_computed_attr.rs` — the sigilless-attribute-specific
  cell mechanism that (empirically) does NOT have this bug; worth
  understanding fully as a model for the fix, or as a target to generalize
  the twigil'd-attribute case onto.

## Suggested next steps

1. Fully trace why the sigilless (`has $x`) case is immune — find the exact
   function(s) it goes through instead of the generic `bind_source` block,
   and check whether it is immune because it takes an entirely different,
   more-robust path, or only because no test happens to combine it with a
   multi-frame call chain yet.
2. Resolve the "not fully explained" step 6 above with the real (unmodified)
   `Test.rakumod` — construct a repro that isolates whether the loss happens
   entirely by the time `bind()` returns, or needs the additional `proclaim`
   method call afterward.
3. Only then decide the fix shape: extending `VmCallFrame` with a per-frame
   code/locals-name reference (real architectural addition, matches this
   repo's "refactor boldly" guidance), vs. a narrower restore-from-saved_env
   reconciliation specifically for attribute-bind targets.
4. Cross-check against
   `todo/deep/control-warn-resume-list-assign-first-target-stale-on-repeat-call.md`
   for a shared root cause before fixing either in isolation.
