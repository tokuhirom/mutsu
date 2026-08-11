# A pointy-block `is <custom-trait>` parameter fails "unknown trait" only when the trait-providing module is large/complex (Cro::HTTP::Router)

## Symptom

`roast`-adjacent Cro::HTTP suite: `http-router.rakutest` gets past the "Cro::Transform" bootstrap tests (64/83 pass) but every route that uses a
`Cro::HTTP::Router`-provided custom parameter trait on a **pointy-block** parameter (`get -> 'search', :$min-price is query = 0 { ... }`) fails.
Minimal repro (no roast/Cro checkout needed beyond the vendored `tmp/cro-work/C_RO_CRO_HTTP_*` dist used by the Cro campaign):

```raku
use Cro::HTTP::Router;
my $blk = -> :$min-price is query { say "ok $min-price" };
$blk(min-price => 5);
```

```
$ MUTSU_FUDGE=... target/debug/mutsu $INC -I lib repro.raku
Runtime error: Can't use unknown trait 'is' -> 'query' in a parameter declaration
```

Confirmed via `rust-gdb` that this is the **parse-time** check in
`src/parser/stmt/sub/param_validate.rs::validate_param_trait_pub` (not the runtime
`check_param_custom_traits` in `vm_register_sub_ops.rs`, which is what a **named sub**
signature parameter uses instead — see the fix landed alongside this ticket for the
named-sub/hoisting variant of this bug class). `validate_param_trait_pub` is the only
possible check for a pointy-block/`for`/`with`/`while` parameter per its own doc comment:
"a loop parameter's traits are lowered away at compile time ... there is no declaration
site that could later dispatch a user `trait_mod:<is>`."

That check is `is_builtin_param_trait(trait_name) || is_user_declared_sub("trait_mod:<is>")`.
`is_user_declared_sub` walks the parser's `SCOPES` thread-local for `user_subs` entries.
`apply_module_exports` (`src/parser/stmt/simple/module_exports.rs`) is supposed to insert
`"trait_mod:<is>"` into that set as soon as it sees an export whose name starts with
`"trait_mod:<"` — and `Cro::HTTP::Router.rakumod` does declare four such exports
(`query`/`header`/`cookie`/`auth`) inside `module Cro::HTTP::Router { ... }`, well before
any route uses them.

## What does NOT reproduce it

Every attempt to reproduce this with a **synthetic** module of the same shape (separate
`Roles.rakumod` declaring the marker roles, `module Foo { multi trait_mod:<is>(...) is
export { $p does Foo::Query } }`, 1-4 candidates, pointy block used both as a `my $x = ->
...` assignment and as a call argument to a wrapper sub) **succeeded** — the trait
registered and applied correctly every time. See the session transcript for ~15 synthetic
variants tried (`tmp/traitrepro*/`), none of which reproduced the failure.

A **direct unit test** calling `crate::parser::parse_program` on the exact 2-line repro
above, with `LIB_PATHS` pointed at the real vendored `Cro::HTTP::Router.rakumod` (plus
`Interpreter::resolve_bundled_lib_paths()` for the bundled batteries, plus a matching
`PROGRAM_PATH`), **also succeeds**: `parse_program` returns `Ok`, and
`is_user_declared_sub("trait_mod:<is>")` returns `true` immediately after parsing line 1.
Yet the CLI, using the same functions, deterministically fails on the same input. This
means there is some difference between "call `parse_program` once, directly" and
"whatever `main.rs`/the CLI's parse driver actually does" that has not been identified —
possibly multiple parse passes over the same script (prelude parsing happens first via
`parse_dispatch::parse_source` for several builtin-role/NativeCall preludes in
`runtime/run_prelude.rs`), possibly something else entirely.

## A real, but so-far-unconfirmed-as-root-cause, soundness issue found along the way

`src/parser/memo.rs`'s `ParseMemo<T>` (backing `PRIMARY_MEMO`, the expression memo, and the
statement memo — all three cleared together by `crate::parser::invalidate_all_memos()`)
keys its cache **by raw pointer identity**: `fn key(input: &str) -> (usize, usize) {
(input.as_ptr() as usize, input.len()) }`. `scan_module_source`
(`src/parser/stmt/simple/module_exports.rs`) reads a module file into a local `String`,
parses it with `parse_program_partial` (populating these memo tables with entries keyed by
pointers into *that* buffer), and then the `String` is dropped by the caller
(`find_and_scan_module`) without ever calling `invalidate_all_memos()`. If a later
allocation (parsing a *different* file, or a different nested module scan) happens to
receive the same freed address+length — plausible, since the allocator readily reuses
freed blocks of matching size, and module scanning frees and allocates many similarly-sized
`String` buffers in a tight loop — a **stale memo entry from the dropped buffer will be
returned for unrelated input**, silently producing a wrong parse result (either a
false failure or a false success) with no error indicating anything happened.

This is a real bug independent of the Cro repro: a temporary fix that adds
`crate::parser::invalidate_all_memos()` at the end of `scan_module_source` (before its
`source` reference's owning `String` is dropped by the caller) was tried and did **not**
fix the Cro repro above — so it is not (solely) responsible for *this* symptom, or the
collision happens somewhere the blanket invalidation doesn't reach (e.g. a nested
`register_module_exports` call *during* `parse_program_partial(source)`, which frees and
reuses buffers before the outer scan's own `invalidate_all_memos()` call is reached — see
the session transcript for the step-by-step allocation/free sequence). Regardless of
whether it explains this ticket, `ParseMemo`'s pointer-identity keying should probably be
made sound (e.g. tag entries with a monotonically increasing "parse generation" number, or
simply call `invalidate_all_memos()` immediately after every `parse_program_partial` call
that operates on a short-lived buffer, not just here) — worth a separate, standalone
investigation and likely its own ticket once someone can build a minimal, deterministic
repro of the collision itself (e.g. via two back-to-back `parse_program_partial` calls on
buffers deliberately allocated/freed to encourage address reuse, or an AddressSanitizer
build to catch a `String`-drop-then-reuse pattern more directly).

## Why this is filed as `deep`, not `tickets`

- Two independent explanations were investigated (module-scan parse truncation from an
  unrelated downstream syntax construct; memo-cache pointer collision) and neither was
  conclusively confirmed as *the* cause, despite an isolated unit test proving the
  individual mechanism (`apply_module_exports` → `register_user_sub("trait_mod:<is>")`)
  works correctly in isolation.
- The bug only manifests through the full CLI parse path on the real, large, vendored
  `Cro::HTTP::Router.rakumod` (1636 lines, 16 `use` statements, deeply nested class/role
  bodies) — a minimal synthetic reproduction was not found after ~15 attempts.
- Fixing it for real likely requires either (a) instrumenting the actual CLI parse path
  (not a unit test standing in for it) to find the exact point of divergence from the
  successful unit-test path, or (b) proving/disproving the memo-pointer-collision theory
  with a dedicated repro, which is itself a chunk of investigation.

## Non-determinism observed across rebuilds (strong evidence for the memo theory)

During the same session, a `make roast`-style suite run of `Cro::HTTP` (via
`tmp/cro-suite-run.sh http`, release binary) once showed `http-router.rakutest` at
**64/83 pass** — the trait-registration bug in this ticket did not trigger that run. A
short time later, after an unrelated small edit-and-revert round-trip (`git stash` /
`git stash pop` on `vm_register_sub_ops.rs`, no functional change) and a **fresh `cargo
build --release`**, the exact same test file, run against the exact same source tree and
vendored Cro checkout, went to **0/83 — the very first route registration in the file now
hits the "unknown trait" error and aborts before any subtest runs.** Repeated 5/5 with the
new release binary, and 3/3 with a freshly rebuilt debug binary: fully deterministic
*for that binary*, but the pass/fail outcome flipped between builds with no source change
that should affect this code path. This is exactly the signature of a raw-pointer-keyed
cache colliding differently depending on the allocator's address layout for a given
binary — reinforcing (though still not proving) the `ParseMemo` theory above over the
scan-truncation theory (which would not plausibly depend on binary layout at all, since
the parse of `Router.rakumod`'s syntax is unaffected by allocator addresses).

**Practical implication:** `http-router.rakutest`'s pass count is not a stable number to
cite in `TODO_roast/BLOCKERS.md` or a PR description — it may read anywhere from 0 to 64
of 83 depending on the exact binary build, until this ticket is resolved. Anyone re-running
the suite should not be surprised by a different count than a prior session recorded, and
should not treat a lower count as a regression from their own changes without first
confirming the *same* binary/build reproduces it more than once.

## Status update (2026-08-09): the ParseMemo soundness hole is fixed; symptom attribution still open

The pointer-identity keying described above is fixed: memo keys are now
`(generation, ptr, len)` with a fresh thread-local generation per
`parse_program` / `parse_program_partial` call (restored on exit), so a nested
scan's entries can never be returned to the enclosing parse via allocator
address reuse (`news/2026-08/parse-memo-generation-key.md`). The
`STMT_ANON_STATES_TLS` table, which shared the same raw-pointer keying, was
fixed the same way.

Whether that collision was *this ticket's* root cause remains unproven: every
build available this session (debug and release) was on the "lucky" side, so
there was no failing binary to test `MUTSU_PARSE_MEMO=0` against. After the
fix, four release rebuilds with deliberately varied binary layout (dead-code
padding in `main`) all held at the historical maximum 64/83 for
`http-router.rakutest`, and the minimal repro passes.

**Decision protocol going forward:** if the 64/83 ⇔ 0/83 flip (or the "unknown
trait 'is' -> 'query'" error) ever recurs on a post-fix binary, the memo theory
is **refuted** — re-run with `MUTSU_PARSE_MEMO=0` to double-check, then pursue
the "full CLI parse path divergence" investigation below (the side where ~15
synthetic repro attempts failed). Until then, treat the memo collision as the
most plausible explanation and this ticket as watch-only.

## 2026-08-11: the flip recurred — memo theory now REFUTED per the protocol above

While investigating an unrelated hang (since resolved as a real bug --
`news/2026-08/parameter-slurpy-positional-introspection-fix.md` -- and
confirmed independent of this ticket: the same binary that failed
`http-router.rakutest` with "unknown trait" passed
`http-router-named-urls.t` 39/39 cleanly), a plain `cargo
build` from this session's starting commit (no functional source changes,
just an unrelated one-line addition to `state_supplier.rs` later reverted)
flipped `http-router.rakutest` back to **0/83** — first-route "unknown trait
'is' -> 'query'" — and simultaneously made `http-router-named-urls.t` (a
different file, no custom traits of its own, but same `-I` module set) fail
identically with **"unknown trait 'is' -> 'cookie'"** before any subtest
runs. Followed the decision protocol exactly:

- Reproduced 5/5 on this one binary (fully deterministic *for this binary*,
  consistent with prior observations).
- **`MUTSU_PARSE_MEMO=0` did NOT fix it** — same "unknown trait" error, 3/3.
  Per the protocol above, **this refutes the ParseMemo collision theory** as
  the explanation for this occurrence (the generation-key fix from
  2026-08-09 is doing its job; whatever is happening now is a different
  mechanism, or the "genuine miss" / scan-truncation path the ticket already
  flags as the alternative).
- Rebuilding again (touching only `src/main.rs`, reverted after) did **not**
  un-flip it — stayed broken across 2 more rebuilds in this session. This
  session did not find a rebuild that returned to the "lucky" 64/83 state
  once it had flipped, unlike the 2026-08-09 note's four-rebuild streak that
  stayed "lucky." (Not enough rebuild attempts were made to know whether
  "stuck on the unlucky side" is now the more common state or this session
  was just unlucky — worth tracking.)
- Confirmed the flip is **not scoped to `is query`/`http-router.rakutest`
  specifically** — `http-router-named-urls.t`'s `is cookie`/`is header` (same
  `Cro::HTTP::Router` module, different call site) fails the same way on the
  same binary, and (per the "What has been ruled out" note in the
  named-urls-hang ticket) even a *content-free* rebuild with zero source
  changes at all reproduced the same "unknown trait" failure — so this is
  not caused by any specific edit, just binary-layout non-determinism that
  is apparently still present post-memo-fix.

**Practical impact restated:** `http-router.rakutest`'s pass count (and by
extension any Cro::HTTP suite file sharing this large `-I` module set) is
*still* not a stable number to cite from a single build. Per the protocol,
the next investigator should pick up the "genuine miss" path (suggested
step 3 below: does `parse_program_partial`'s scan of `Router.rakumod`
actually reach and return from the whole `module Cro::HTTP::Router { ... }`
block on an "unlucky" binary, or silently truncate?) rather than revisiting
the memo theory, which is now refuted for this occurrence.

## Suggested next steps

1. Instrument `main.rs`'s actual parse call (not a substitute unit test) with a
   `MUTSU_DEBUG_*`-gated `eprintln!` right where `validate_param_trait_pub` fails, dumping
   `SCOPES` state and `PARSE_MEMO` stats (`ParseMemo::stats()` — hits/misses/stores) to see
   whether the failing lookup is a memo hit (confirming the collision theory) or a genuine
   miss with `trait_mod:<is>` truly absent from every scope (confirming the scan-truncation
   theory).
2. If it is a memo hit: find exactly which nested scan's freed buffer the stale entry came
   from (a `MUTSU_DEBUG_MEMO_STORE` print of `(ptr, len, first-40-chars-of-input)` on every
   `store()` call, correlated with `find_and_scan_module`'s file being scanned, should
   locate the culprit directly).
3. If it is a genuine miss: check whether `parse_program_partial`'s parse of
   `Cro::HTTP::Router.rakumod` actually reaches and returns from the whole
   `module Cro::HTTP::Router { ... }` block, or silently truncates at some unrelated
   internal parse difficulty (candidate found during this investigation:
   `--dump-ast`-ing `Router.rakumod` **standalone** fails at line 228,
   `when X::Cro::BodyParserSelector::NoneApplicable { }`, with "needs parens to avoid
   gobbling block" — worth checking whether the *scan* context hits the same wall even
   though the *real* end-to-end run of `http-router.rakutest` does not abort outright).

## Affected files (read-only references, for orientation)

- `src/parser/stmt/sub/param_validate.rs` — `validate_param_trait_pub` (the failing check)
- `src/parser/stmt/simple/module_exports.rs` — `apply_module_exports`, `scan_module_source`,
  `find_and_scan_module`
- `src/parser/memo.rs` — `ParseMemo` (the pointer-keyed memo cache)
- `src/vm/vm_register_sub_ops.rs` — `check_param_custom_traits` (the *runtime* sibling
  check, already fixed for the named-sub/hoisting case in the PR filed alongside this
  ticket)
- `tmp/cro-work/C_RO_CRO_HTTP_*/lib/Cro/HTTP/Router.rakumod` (vendored, gitignored) — the
  real module that triggers this
