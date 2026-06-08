# Collapsing the VM ↔ Interpreter duplication

## Why this matters (the real cost is maintenance, not perf)

mutsu has **two engines**: the bytecode **VM** (`src/vm/`, with pure native ops in
`src/builtins/`) and the legacy tree-walking **Interpreter** (`src/runtime/`). For
many Raku operations — builtin functions, methods, operators, coercions — the
*same logic is implemented in both places*. The VM is meant to be authoritative;
the Interpreter is being eliminated.

The problem this document targets is **not** runtime cost (the interpreter
"bridge" rarely fires — fallback rates on normal code are ~0%). It is the
**maintenance hazard of duplicate implementations**:

- A fix applied to one copy is silently forgotten in the other → the two drift.
- Someone (human or AI) reads the codebase, finds the *wrong* copy, and "fixes"
  a place that isn't on the hot path — or can't tell which copy is authoritative.
- Two sources of truth for one behavior is a standing invitation to bugs.

So the goal is to **collapse each duplicated operation to a single
implementation** — keep the native one, delete the interpreter one — even when
the duplicate "isn't hurting performance." Fewer implementations of the same
thing is the win.

## How the interpreter reaches native code (the mechanism that makes deletion safe)

The interpreter's giant `Interpreter::call_function` match (`src/runtime/builtins.rs`)
has per-name arms (`"abs" => self.builtin_abs(...)`, …). Its catch-all is:

```rust
_ => self.call_function_fallback(name, &args)   // src/runtime/builtins.rs
```

and `call_function_fallback` (`src/runtime/builtins_operators.rs`) tries the
**native** table:

```rust
if let Some(native_result) = crate::builtins::native_function(Symbol::intern(name), args) {
    return native_result;
}
```

So when the interpreter itself evaluates Raku code (EVAL, regex embedded `{ }`
blocks, gather/react bodies, anywhere the interpreter is the carrier) and the
per-name arm is **removed**, the call falls through to the native implementation.
That is what lets us delete an interpreter arm + its `builtin_*` body without
breaking the interpreter-evaluated paths — **provided** native actually covers
the name through that fallback.

## Safe-deletion procedure (per duplicated builtin)

1. Confirm the name has a real `=> Some(...)` arm in `src/builtins/functions.rs`
   `native_function` (grep for `"<name>" =>`, not just the string).
2. Confirm the interpreter `builtin_<name>` has **no caller other than** the
   `call_function` match arm (`grep -rn "builtin_<name>\b" src/runtime/`).
3. **Verify behavioral equivalence through the fallback path**, not just the VM
   path: `mutsu -e 'say EVAL(q{<name>(...edge cases...)})'` must match `raku`.
   The VM path and the interpreter-fallback path are **different entry points**
   (`VM::try_native_function` vs `Interpreter::call_function_fallback` →
   `native_function`) and do **not** have identical coverage — see the gotcha.
4. Delete the arm and the `builtin_<name>` body (a `pub(super)` fn with no caller
   trips `clippy -D warnings`, so it must go, not just be orphaned).
5. `make test` + `make roast` — the interpreter copies are exercised by EVAL /
   regex / carrier paths across the suite, so the full roast run is the net.

### Gotcha found in practice (`chrs` / `ords`) — and how it was resolved

`chrs` originally had a native arm only in `native_function_variadic` (the 4+ arg
path), so `chrs(72,105)` (2 args) routed to `native_function_2arg`, found nothing,
and `EVAL(q{chrs(72,105)})` failed with *"Unknown function: chrs"*. The first batch
**kept** the interpreter copy. The right fix (Category A second batch) was **not**
to keep the duplicate but to **make native reachable at the call's arity**: route
`chrs` through `native_function_variadic` for all arities. `ords`, despite being
grouped with `chrs`, was actually already reachable — it has a `native_function_1arg`
arm and Raku's `ords` is 1-arg only — so it just needed deleting.

Lesson: step 3 (EVAL-path verification) is mandatory — a native arm existing is
necessary but not sufficient; check the **arity** the fallback will dispatch to.
When a native arm exists at the wrong arity, prefer adding/rerouting the native arm
(one impl) over keeping the interpreter duplicate.

## Progress

- **Done (Slice 6.3 dedup, first batch)** — deleted the interpreter copies of 9
  pure value builtins, all verified equivalent through the EVAL fallback path and
  `make roast`: `abs`, `lc`, `uc`, `tc`, `trim`, `flip`, `chr`, `ord`, `chars`.
- **Done (Category A complete, second batch)** — the remaining pure value builtin
  duplicates were deleted, finishing Category A. `sign` and `ords` were pure
  deletions (native `native_function_1arg` already covered them; `ords` is 1-arg
  only in Raku, so the single-arg fallback fully reaches it). The three that the
  first batch had **kept** as "fallback-unreachable" were made reachable instead
  of left duplicated:
  - `chrs` — routed through `native_function_variadic` for *every* arity (a guard
    at the top of `native_function`, like `sum`/`zip`), and its variadic arm now
    flattens via `crate::runtime::utils::value_to_list` so `chrs(72..74)` /
    `chrs((72,73,74))` keep working. Interpreter `builtin_chrs` deleted.
  - `unival` / `univals` — added `native_function_1arg` arms that delegate to the
    `.unival` / `.univals` method impl (`methods_0arg/dispatch_core_unicode.rs`),
    the single source of truth. Interpreter `builtin_unival` / `builtin_univals`
    (and the `unival_for_char` helper) deleted.
  All verified equal through both the VM and EVAL paths against `raku`, plus the
  whitelisted `S32-num/sign.t`, `S32-str/ords.t`, `S15-unicode-information/unival.t`,
  `S29-conversions/ord_and_chr.t`. **No pure-value builtin duplicate remains** —
  the next dedup work is Category B (genuine forks) / Category C (methods/arith).

## Remaining duplication (prioritized)

Audit (2026-06) found ~23 builtins duplicated between `src/builtins/functions.rs`
and `src/runtime/builtins*.rs`, plus method and operator duplication.

- **Category A — pure value builtins, delete-and-fallthrough** (low risk; the
  above batch + any stragglers). Each needs the EVAL-path check.
- **Category B — genuine forks — DONE**
  (#2727/#2728/#2730/#2731/#2733/#2734/#2735/#2739). Two distinct fork shapes,
  collapsed by two methods:
  - *Block forks* (`sort`/`min`/`max`/`minmax`/`first`): the native copy bailed on
    a comparator/mapper/matcher block. Fixed by extracting the orchestration
    (`sort_items_generic`, `extrema_from_values_generic`,
    `minmax_from_values_generic`, `find_first_match_generic`) into one
    engine-agnostic implementation and swapping only the block invocation through a
    closure/trait (`SortCaller`, `FirstMatcher`, `vm_call_on_value` /
    `call_sub_value`). The VM runs them natively (`interpreter_fallbacks=0`); the
    interpreter copy is now a thin adapter.
  - *Lazy forks* (`elems`/`flat`/`join`/`reverse`): the fork was lazy-forcing /
    polymorphism re-implemented in both layers, and the two copies had **drifted**.
    Collapsed to one correct implementation (delegate `elems`→`.elems` method,
    `flat`/`join`→shared `flat_val`/`join_flat`, `reverse`→native), reconciling each
    drift against `raku` — which surfaced and fixed several latent bugs
    (`elems("hello")` 5→1, `flat` nested over-flatten, `join(sep, Range)` ignoring
    the separator, `reverse()` `Nil`→`()`).
  - `index`/`rindex` had no interpreter copy (already native-only).
- **Category C — methods** duplicated across `src/builtins/methods_*` (native
  fast path) and `src/runtime/methods.rs` / `methods_mut.rs` (slow path), and
  operator/arith/coercion logic across `src/builtins/arith.rs` /
  `src/vm/vm_arith_ops.rs` vs `src/runtime/`. Larger, do after the function
  forks.

The end state is the legacy `Interpreter` method/function execution paths
deleted entirely (PLAN.md final goal); the `env_dirty` dual-store flag then falls
out because the only remaining by-name arbitrary writer (the interpreter bridge)
is gone. Until then, prefer **deleting a confirmed-redundant interpreter copy**
over adding a third place to keep in sync.

## Category C progress — arith / operator / coercion (2026-06-07)

Phased, low-risk-first (user-approved); detection by **manual audit** of
`should_bypass_native_fastpath` (`methods_native_bypass.rs:105`) + native
coverage, each change gated on EVAL equivalence + `make roast`.

### Phase 1a — reduction `%`/`mod` → native `arith_mod`
`runtime/ops.rs::apply_reduction_op` reimplemented `%`/`mod` locally (Int + f64
only, soft divide-by-zero `Failure`), duplicating `builtins::arith_mod`. Replaced
the two local arms with a delegation. **2 duplicate arms removed**; also a
correctness fix — `[%] 2**70, 3` was lossy via `to_int` (now exact), and
`[%] 5, 0` now throws like `raku` / the `%` operator instead of a soft Failure.

### Phase 1b — unify Instance→numeric bridge coercion
Of 4 numeric-coercion helpers, two were duplicates of the *Instance→numeric
bridge*: VM `coerce_numeric_bridge_value` (full: Failure throw, Match, has_user_method,
~25 arith/comparison call sites) and interpreter `coerce_infix_operand_numeric`
(simplified copy, 2 callers). Made `coerce_infix_operand_numeric` the single
authoritative impl (expanded; dispatch via `call_method_with_values`); the VM
helper delegates to it. **1 duplicate removed.** `utils::coerce_numeric(l,r)` and
`coerce_to_numeric(v)` are already single-impl (shared by VM + native) — not
duplicates, left as-is.

### Phase 2 — dead method copies + operator-body dedup
**Audit finding**: the "dead simple-value method copy" harvest is **largely
already done** — simple 0-arg value methods (`flip`/`fc`/`succ`/`pred`/`abs`/
`sign`/`sqrt`/`floor`/`ceiling`/`round`/…) have **no** interpreter dispatch copy.
What remains in `dispatch_method_by_name_{1,2,3}` is **live** (Instance-only forks
that coerce + re-dispatch e.g. `dispatch_trig_instance_method`; Buf/Blob
`X::Buf::AsStr` special-cases; Failure-explosion exclusion lists; Promise/Supply/
Match methods; Category-B comparator/lazy forks). So Phase 2's real remaining
duplication is **operator-body reimplementation** in `apply_reduction_op` (which
is itself authoritative — the VM delegates to it via `vm_dispatch_helpers.rs:115/453`
etc.).
- [x] **`~` (concat)**: `apply_reduction_op`'s `~` arm reimplemented Buf~Buf +
      a `format!` fallback lacking the VM's Buf~non-Buf decode and non-ASCII NFC
      normalization. Made `VM::concat_values` a state-free `pub(crate)` assoc fn
      and delegated both the VM `~` op and the reduction `~` arm to it. **1
      duplicate removed**; fixes a latent bug (`[~]` now NFC-normalizes like `~`).
- [x] **`minmax`**: `apply_reduction_op`'s `minmax` arm had its own `range_bounds`
      closure that (unlike the VM's `minmax_bounds_of_value`) did **not** recurse
      into elements, so `[minmax] (1,5),(2,8)` etc. computed wrong bounds. Made
      `vm_misc_ops::minmax_bounds_of_value` a `pub(crate)` fn (module exposed
      `pub(crate)`) and delegated the arm to it. **1 duplicate removed** + bug fix
      (`[minmax]` over nested arrays/ranges now matches raku, e.g. `0..8`, `1..9`).
- [ ] Remaining `apply_reduction_op` bodies: logic short-circuit `&&`/`||`/`//`
      are the reduction-only impl (the VM compiles infix short-circuit to jumps, so
      not VM-op duplicates); comparisons already use shared `runtime::compare_values`.
      Likely little left here.
- [ ] Genuine-fork methods → fold into native (Category B style; depends on the
      `%`-chain block-dispatch blocker noted under Category B in PLAN.md).

### Phase 3 groundwork — root cause of the `%`-chain block-dispatch blocker

The Category B "`%h.sort({block})` → expected 2 got 0" regression has the same
root cause as a live bug in `%h.first({block})`: **mutsu uses the `Value` variant
to distinguish a call-site named arg (`Value::Pair`, excluded from positional
arity everywhere in dispatch) from a positional pair value (`Value::ValuePair`).
Iterating a Hash yields `Value::Pair` elements**, so passing one straight to a
matcher/comparator block binds it as a *named* argument, leaving the block with
zero positionals (`first`'s `$p` / sort's `$^a,$^b`). Array literals
(`(a=>3),(b=>1)`) yield `ValuePair`, which is why `@a.first({...})` worked but
`%h.first({...})` did not.

- Added `runtime::utils::pair_as_positional(&Value)` (maps `Pair`→`ValuePair`)
  as the single helper for "bind this element positionally to a block".
- Fixed `find_first_match_over_items` (drives `.first`) to use it. `%h.first({...})`
  and `%h.first(&sub)` now bind the pair positionally. Test: `t/hash-first-positional.t`.
- This is the helper the eventual native-sort migration needs to absorb what the
  legacy `dispatch_sort` was implicitly handling, unblocking deletion of
  `dispatch_sort` (Category B). Separately pre-existing & out of scope: pointy
  `-> $p {...}` / `*.value` matchers on `.first` go through the `smart_match` path
  (not the `Value::Sub` call branch) and leave `$_`/`$p` unbound — fails even for
  literal arrays; a distinct fix.

### Phase 3 — complete (#2743, #2744, #2745, #2747)

The three remaining Phase 3 items are done.

- **① sort wrapper dedup — `dispatch_sort` deleted (#2744).** The orchestration
  body (`sort_items_generic` / `sort_indices_generic` + the `SortCaller` trait)
  was already shared, but the *wrapper* around it (arg parse `:k`/`:by`/arity +
  target-shape → element-list dispatch) was duplicated in `Interpreter::
  dispatch_sort` and `VM::try_native_sort`, and had drifted (`dispatch_sort` took
  stray adverbs `:kv`/`:p`/`:v` as a callable; `try_native_sort` bailed to the
  interpreter for them, which then ran `dispatch_sort` anyway). Collapsed into one
  engine-agnostic `sort_value_generic(&mut dyn SortCaller, target, args)` (added
  `SortCaller::callable_arity`). `dispatch_sort` deleted; its callers build an
  `InterpCaller` and call the shared fn. `try_native_sort` keeps only the
  target-shape gate (Shaped/Instance/Supply still fall back) and delegates.
  Behavior-preserving.

- **③ `.first` pointy-block param binding (#2743).** The "out of scope" pointy
  matcher bug above turned out to be a one-line-locus fix, *not* a `smart_match`
  issue: a pointy lambda `-> $p {...}` compiles to `params: ["p"]` with an **empty
  `param_defs`**, so binding falls to the legacy path in
  `bind_function_args_values`, whose positional filter treated the
  `ValuePair(Str,_)` (from `pair_as_positional`) as a *named* arg, leaving `$p`
  unbound. Fix: in the legacy path, when the param list has a plain positional
  name, treat `ValuePair(Str,_)` as positional. One shared helper → both the VM
  (`vm_native_first`) and interpreter `.first`/`.grep`/`.sort` carrier paths fixed
  at once. (`$:name` placeholders, WhateverCode, `$^a` are unaffected.)

### Category C, item ② — placement audit (comb / substr / split)

The "fold into native" item was reframed by an explicit *placement* audit under
the "1 operation = 1 implementation" principle, extended to **"is that single
implementation in the right layer?"**

- **split — already correct (the model).** The pure mechanics (`SplitOpts`,
  `apply_split_opts`, `parse_split_limit`, `split_by_string_static`,
  `split_by_strings_static`, `SplitMatch`) live in `builtins/split.rs` and the
  interpreter *imports and reuses* them, adding only the regex-split case. The
  regex matcher (`regex_find_*`) is genuinely interpreter-coupled (`parse_regex`,
  `current_package`, grammar-subrule resolution, embedded `{ }` block eval), so
  regex split correctly lives in `runtime/`. No change.

- **comb — pure split was stranded in the interpreter; relocated (#2745).** The
  `Int`-chunk and `Str`-fixed matchers are pure but lived in
  `runtime/.../dispatch_comb_with_args`, so `.comb(2)` / `.comb("x")` took a
  VM→interpreter fallback. Moved into `builtins/comb.rs` (`comb_pure`) as the
  single impl, called by both the native fast path (`native_comb_method` in
  `native_method_1arg`/`2arg`) and the interpreter. The `Regex` matcher and
  Code-arg rejection stay in `runtime/`. **Not** a duplicate native copy — the one
  impl moved down to the correct layer.

- **substr — no relocatable pure logic, but four native copies → one (#2747).**
  Position resolution is genuinely interpreter-coupled (a `WhateverCode` start or
  length calls `eval_call_on_value`; out-of-range yields a `Failure`), so unlike
  comb there is no pure algorithm to relocate from the interpreter. The real
  duplication was the **four** native copies of the simple non-neg slice
  (`native_method_1arg`/`2arg` + the 2-/3-arg `substr` function); collapsed into
  one `builtins/substr.rs::native_substr_slice`. The interpreter's `dispatch_substr`
  remains the single owner of the Whatever/Range/negative/out-of-range cases.

### Category C — done

All Category C duplication is removed: Phase 1a/1b (arith/coercion), Phase 2
(reduction operator bodies), Phase 3 (sort wrapper, `.first` binding) and the
item ② placement audit (comb relocated, substr 4→1, split confirmed correct). The
only remaining function/method-level duplicate flagged in PLAN.md is the regex
validator/matcher double-implementation (ANALYSIS §3.1), tracked separately.
