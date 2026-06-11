# `is rw` parameter — true shared-cell aliasing (implementation plan)

> Prep for the next session. Migrate `is rw` sub/method parameters from the
> current **value-writeback** model to a **live shared `ContainerRef` cell**, so
> a binding to a rw param (`$a := $arg`) and writes through any of
> caller-var / param / re-bind alias all observe one physical container.
>
> Builds directly on the element-cell infra hardened in #2902/#2908/#2911/#2914/
> #2922/#2925 (`ContainerRef` cell, `assign_element_slot`/`hash_insert_through`
> write-through chokepoints, read-side decont). Read alongside
> `docs/container-identity.md` and the memory `phase2-element-containers-progress`.
>
> ⚠️ **High-risk: touches the hot call path.** The PLAN flags this explicitly
> ("hot な call path 全体に触れる高リスク改修"). Gate the change so the common
> (non-rw) path is byte-identical and zero-cost. Lean on CI's release roast.

## Target tests (definition of done)

`roast/S03-binding/scalars.t` — currently **2 fails: 24, 27** (file is 31/33).
Both are in the `sub ($arg is rw) { $a := $arg }` block (lines 108-121):

```raku
my $a;
my $b = sub ($arg is rw) { $a := $arg };   # bind outer $a to the rw param $arg
my $val = 42;

$b($val);
is $a, 42, "bound rw sub param was bound correctly (1)";   # 23 PASS
$val++;
is $a, 43, "bound rw sub param was bound correctly (2)";   # 24 FAIL  <-- $a must track $val
lives-ok { $a = 23 }, "remains rw (1)";                    # 25 PASS
is $a, 23,            "remains rw (2)";                     # 26 PASS
is $val, 23,          "remains rw (3)";                     # 27 FAIL  <-- $a write -> $val
```

**Must NOT regress** the readonly block right above it (lines 90-105): a plain
`sub ($arg) { $a := $arg }` must SNAPSHOT — after `$val++`, `$a` stays 42, and a
later `$val++` reports `$val == 43` independently. Promotion is gated on `is rw`.

Minimal repros (write to `tmp/` with the Write tool):

```raku
# A. rw param live alias (THE fix) — currently 42/42, raku 43/23
my $a;
my $b = sub ($arg is rw) { $a := $arg };
my $val = 42;
$b($val);
say $a;            # 42
$val++;
say $a;            # raku 43   (mutsu today: 42)
$a = 23;
say $val;          # raku 23   (mutsu today: 43)

# B. readonly param must stay a snapshot (regression guard) — must print 42
my $a2;
my $b2 = sub ($arg) { $a2 := $arg };
my $v2 = 42;
$b2($v2);
$v2++;
say $a2;           # 42 (NO aliasing for readonly params)

# C. direct rw alias without re-bind (sanity) — raku 99
sub bump($x is rw) { $x++ }
my $n = 5;
bump($n);
say $n;            # 6   (this already works today — confirm it still does)
```

Reference: `raku tmp/<file>.raku` for expected output before each comparison.

## Root cause

Current `is rw` handling is **value-writeback**, not a live alias:

- `bind_function_args_values` (`src/runtime/types/binding.rs:51`) returns
  `rw_bindings: Vec<(param_name, source_name)>` — name pairs, not shared storage.
- At sub return, `apply_rw_bindings_to_env` (`src/runtime/types/mod.rs:179`)
  COPIES the param's final value back into the caller var:
  `target_env.insert(source_name, self.env.get(param_name).clone())`.
- The closure-dispatch writeback in `src/vm/vm_closure_dispatch.rs` (~351
  `rw_bindings`, ~670 `apply_rw_bindings_to_env`, ~738 `needs_caller_writeback`,
  ~750 `rw_sources`) merges param→caller at frame teardown.

So `$arg`, `$a`, `$val` are SEPARATE `Value` copies linked only by name. mutsu
already makes `$a =:= $val` report **True** (name-link), but `$val++` does not
propagate to `$a` — confirming identity-without-shared-storage. The re-bind
`$a := $arg` aliases `$a` to the param local, which dies at sub return; the
writeback then snapshots, so `$a` keeps its 42.

## Approach — pass the caller variable as a shared cell

Reuse the landed `ContainerRef` cell (the same mechanism element binds use):

1. **At the call site / arg binding**, for a parameter declared `is rw` whose
   argument is an lvalue *variable* (`$val`), **promote the caller's variable to
   a `ContainerRef` cell** (if it is not already one) and write that cell back
   into the caller env slot, then bind the **param local to the same cell**.
   - This is the scalar-container analogue of `array_slot_ref(idx, terminal)` /
     `hash_slot_ref(key, terminal)` — here the "slot" is an env variable.
   - Gating: ONLY when the param def `is_rw` is true AND the arg is a plain
     variable reference (a `WrapVarRef` carrying the source name). A non-variable
     rw arg (literal/temporary) has nothing to alias — keep today's behavior.
2. **Inside the body**, `$a := $arg` already routes through the scalar-bind path
   (`MarkBind` + `VarDecl` `__scalar_bind`); binding to a cell-valued `$arg`
   makes `$a` share the cell. Reads decont at the value chokepoint
   (`GetLocal`/`GetGlobal` via `into_deref`/`descalarize`); writes go through
   `assign_element_slot`-style write-through. **Verify** the scalar `=` write to
   a cell-valued var writes THROUGH (it must already, for element binds).
3. **At sub return**, the cell IS the live alias, so the value-writeback becomes
   a no-op for cell-backed rw params. Skip `apply_rw_bindings_to_env` for any
   rw binding whose param local already holds a `ContainerRef` (the cell did the
   work). Keep the old writeback for the non-cell fallback (non-variable args).

The decont chokepoints already exist (element-cell work), so value ops, `=:=`
(cell Arc identity via `containers_same_slot`), and rendering all work for free.

## Key sites to investigate (exact, from this session's grep)

- `src/runtime/types/binding.rs:51` `bind_function_args_values` — where the
  (param, source) pair is determined; the place to detect `is_rw && arg-is-var`
  and promote+share the cell instead of recording a writeback pair.
- `src/runtime/types/mod.rs:179` `apply_rw_bindings_to_env` — make it skip
  cell-backed params (no-op when the param local is a `ContainerRef`).
- `src/vm/vm_closure_dispatch.rs:351/670/738/750` — the closure (anon `sub`)
  dispatch path that the failing test actually exercises (`$b = sub (...) {...}`
  is a closure, not a named sub). **This is the path test 24/27 hit.**
- `src/vm/vm_call_dispatch.rs:1574` — named-sub path (`bind_function_args_values`
  caller); apply the same promotion for parity.
- `src/vm/vm_method_dispatch.rs:432` and `src/vm/vm_call_method_compiled.rs:1337/
  1858` — method rw params; mirror once the sub path works.
- Arg passing: how a rw arg carries its source variable (`WrapVarRef` — grep
  `WrapVarRef` in `src/compiler/` + `src/vm/`). The source name is already known
  (it drives today's writeback), so promotion has the var name in hand.

## Pitfalls / guardrails

- **Readonly params must stay snapshots** (repro B / scalars.t 96-98). Promote
  ONLY when `is_rw`. A plain `$arg` must keep copying the value.
- **Non-variable rw args** (`bump(5)` is illegal; but `bump($a[0])`,
  `bump(f())`) — only a plain scalar *variable* source can be cell-promoted here;
  element/temp sources keep the existing path (and element sources are already a
  separate cell via the element-cell work — verify they compose, don't
  double-promote).
- **Hot-path perf**: the promotion must be behind the `is_rw` flag so the common
  call (no rw params) does zero extra work. Time `int.t`-class microbench /
  a call-heavy roast file before/after; the element-cell promotion is
  escape-aware and added no cliff — keep that property.
- **Don't regress the writeback fallback**: leave `apply_rw_bindings_to_env`
  intact for the non-cell case; only short-circuit when the param holds a cell.
- **`temp`/`let` restore interaction**: a cell-backed rw var must restore
  correctly (the element-cell work already handles cell save/restore; re-confirm
  `S04-blocks-and-statements/let.t` passes — it was the canary that broke a prior
  naive promotion).
- **`=:=` already reports True today** via name-link, so do not "fix" `=:=` —
  the gap is live mutation, not identity reporting. After the cell change, `=:=`
  should resolve via cell Arc identity (`containers_same_slot`) instead.

## Verification plan (every iteration)

1. Repros A–C vs `raku` (B must stay 42; C must stay 6).
2. `roast/S03-binding/scalars.t` — target 24, 27 pass, **no regression** on
   1-23/25-26/28-33 (esp. the readonly block 18-23 and list-binding 28-33).
3. `roast/S06-signature/*` rw/`is raw` tests, `roast/S03-binding/*`,
   `roast/S12-methods/*` accessors (method rw params).
4. `make test` completes; `S04-blocks-and-statements/let.t` PASS (temp/let canary).
5. Call-heavy perf canary unchanged (no cliff — gate on `is_rw`).
6. Push → **CI release roast** (cell changes leak only in release — the
   #2898/#2908/#2922 precedent).

## Why this is the right next slice

- Single, well-scoped pair of failing subtests (scalars.t 24/27) with a clear
  root cause.
- Directly reuses the now-mature element-cell infra (write-through chokepoints,
  read decont, `=:=` cell identity, leak hardening) — *apply, not re-derive*.
- Advances the container-identity north-star (rw params = caller-container
  aliasing) which the PLAN lists as a top cross-cutting blocker, and converts the
  `apply_rw_bindings_to_env` writeback (a non-cell workaround) toward the real
  cell — a genuine mechanism simplification, not just a test fix.
