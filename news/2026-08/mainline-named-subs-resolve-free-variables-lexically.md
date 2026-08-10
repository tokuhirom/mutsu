# Mainline named subs resolve free variables through unit-lexical cells, not the ambient env

A named `sub` declared at mainline whose body reads a free variable used to
resolve that name **dynamically** — against whatever the calling frame's env
currently held under that bare name — instead of **lexically** against the
binding visible at the sub's own declaration site. Any later block (`{ }`,
`if`, `for`, `start { }`, ...) that declared a same-named `my` shadowed the
sub's true lexical binding for every call made from inside that block:

```raku
my $client = "outer";
sub helper($u) { $client }
{
    my $client = "inner";
    say helper(0);   # raku: outer    mutsu (was): inner
}
```

This is fixed per [ADR-0024](../../docs/adr/0024-mainline-lexicals-for-named-subs.md),
which extends the `unit_lexicals` mechanism already used for `unit module`
compunits: mainline's own captured `my` scalars are eagerly boxed into shared
cells under a reserved `MAINLINE_UNIT_KEY` at named-sub registration time,
resolved through those cells whenever the last (non-block) call frame is a
marked mainline sub.

## What landed

- `MAINLINE_UNIT_KEY` + `mainline_lexical_subs: HashSet<String>` on
  `Interpreter`, cloned into thread clones alongside `unit_lexicals`.
- Eager capture in `exec_register_sub_op`: for each free variable a mainline
  sub's compiled body (and every `signature_alternates` body, for a `multi`)
  reads OR writes, a plain scalar `my` local with its own mainline slot is
  boxed into a `ContainerRef` cell (or an existing cell is reused), inserted
  into `unit_lexicals[MAINLINE_UNIT_KEY]`. A shared `type_constrained_unboxable`
  helper (extracted from `box_captured_lexicals`) skips a type/`where`-
  constrained scalar, matching the existing closure-capture discipline.
- `mainline_lexical_frame_active()`: a cheap (map-presence-gated) predicate
  checked first in `unit_lexical_slot`, true only when the LAST routine frame
  is a non-block, `GLOBAL`-package, marked mainline sub — deliberately not an
  innermost-named-frame walk, so a closure created inside a shadowing block
  and invoked by the sub still reads its own captured (shadowed) binding.
- Writeback suppression at both free-var-write replay sites
  (`vm_call_named_inner.rs`, `vm_call_light_typed.rs`): a marked sub's write
  to its own captured lexical must not be replayed into the caller's slot,
  which — for a call made inside a shadowing block — would clobber the
  shadow instead of (correctly) updating only the real lexical's cell.
- Closure-capture cell injection in `capture_closure_env` (both the
  reflective whole-env path and the slim `free_var_syms` path): a closure
  created while a marked mainline sub's frame is active overrides each
  captured free variable that has a cell, instead of inheriting whatever the
  (possibly shadowed) creating frame's env holds.
- `MUTSU_VM_STATS` counters `mainline_lexical_boxes` / `mainline_lexical_hits`;
  confirmed `boxes == 0` across all 23 `benchmarks/*.raku` (empty-map fast
  path, no measurable cost for programs that never trigger this capture).

## Gaps the ADR did not anticipate, found and fixed during implementation

- **Duplicate-named slots under shadow slots.** `code.locals.iter().rposition`
  (the ADR's literal instruction) picks the wrong slot whenever ANY other
  scope in the same compiled unit — before or after the sub, e.g. exactly the
  headline shadowing-block example — redeclares the same name: shadow slots
  give the shadow its own distinct slot with the identical name, so a plain
  positional search can grab it instead of mainline's own. Fixed by
  disambiguating on liveness instead: at the moment a `RegisterSub` runs, a
  shadowing block declared elsewhere has either not executed yet (still its
  pool-allocated `Nil`) or is unrelated; only the slot that is genuinely
  initialized right now is captured, and an ambiguous (zero or multiple live)
  case is skipped (legacy dynamic fallback, no partial state).
- **Write-only free variables.** A setter (`sub set-v($x) { $v = $x }`) never
  appears in `free_var_syms` (reads only) — only in the separate
  `free_var_writes` set. The capture loop now unions both.
- **Light/fast call paths skip `routine_stack`.** `mainline_lexical_frame_active`
  keys off the last `routine_stack` frame, but `call_compiled_function_fast`,
  `call_compiled_function_light[_spec]`, and
  `call_compiled_function_positional_light` all deliberately skip pushing a
  frame (that's the overhead they exist to avoid), so a captured sub
  dispatched through one of them ran with no frame info and its cells never
  resolved. Fixed by excluding `mainline_lexical_subs` members from every
  light/fast eligibility check at the five call sites in
  `vm_call_func_ops.rs`, forcing them onto the frame-pushing path.
- **Two env-direct read/write shortcuts bypass the store.** `GetGlobal`'s "J4
  fast scalar read" hot path and `SetGlobal`'s generic "write through
  ContainerRef" shortcut both consult `env` directly by name before falling
  into `get_env_with_main_alias`/`unit_scope_lexical_write` — and env, being
  flat/name-keyed, can (and does) hold a shadowing block's OWN boxed cell
  under the identical key by the time the sub runs. Both are now gated on
  `!mainline_lexical_frame_active()` (read) / an upfront
  `unit_scope_lexical_write` check (write) so a marked sub's own captured
  names never reach the generic env-cell shortcut.

## Two more gaps, found by CI (real regressions in already-whitelisted roast files)

The initial PR's CI run regressed two whitelisted files deterministically.
Both traced back to the same underlying pattern: a **frame-independent**
"assign this name by value" utility that predates ADR-0024 and had no notion
of "an existing cell here must be preserved" — unlike the frame-gated
read/write paths above, these do not run inside the capturing sub's own
frame, so `mainline_lexical_frame_active()` is unhelpful for them. Both are
fixed with the same new tool: `mainline_lexical_cell(name)`, a
frame-independent lookup straight into `unit_lexicals[MAINLINE_UNIT_KEY]`,
tried before the utility's own blind overwrite.

- **`roast/S06-routine-modifiers/lvalue-subroutines.t` (a Proxy built from an
  `is rw` mainline sub).** `sub lastvar is rw { $var2 }; lastvar() = 3` does
  not assign inside `lastvar`'s own frame: `assign_rw_target_expr`
  (`runtime/builtins_lvalue.rs`) introspects the callee's AST for the target
  *name* and assigns it directly in the CALLING frame via a raw
  `self.env.insert`. From inside a Proxy STORE closure built by a mainline
  `checklastval` sub, that blind insert replaced the cell reference itself,
  so a later FETCH (reading through the SAME cell `lastvar`'s own body reads)
  kept returning the pre-assignment value forever.
- **`roast/S32-io/IO-Socket-Async.t` ("Coped with grapheme split across
  packets").** A mainline sub capturing `$port`, reread after `$port = await
  $tap.socket-port` (a REAL async I/O wait — unlike a same-thread
  `Promise.new`/`.keep`, which resolves inline and never touches this path),
  kept observing the pre-reassignment port. The worker thread completing that
  `await` can have been spawned/cloned *before* the capturing sub registered
  (its own `env` snapshot predates the cell), so its own write to `$port`
  landed in `shared_vars` as a plain value; `sync_shared_vars_to_env()`
  (`runtime/runtime_shared_vars.rs`, pre-existing ADR-0010 cross-thread
  plumbing, invoked from `await`) then blindly `env.insert`ed that plain
  value on the awaiting thread, replacing the cell every other reader —
  including the capturing sub — still held.

## Verification

`t/named-sub-lexical-scope.t` pins the ADR's full divergence matrix (rows
1, 2a, 2b, 3, 4, 5, 6, 7, 8, "adv"), each raku-verified independently.
`tmp/nsub-lex-matrix.raku` and `tmp/nsub-lex-edge.raku` (multi-signature
union, nested shadows, recursion, a type-constrained lexical, a block-
declared sub correctly left on the legacy path) match `raku` byte-for-byte.
`make test` passes; roast is CI's job (blast radius is every mainline named
sub with free variables, so no local subset).
