# ADR-11276: Built-in methods are handler rows in the one method table

- **Status**: Accepted (user decision 2026-10-03). Slices 1 and 2 done, slice 3 under way; see
  §9. Amended 2026-10-06: slice 3 is re-cut into macro-slices (§10); the "one PR per family"
  plan of §6 item 3 is withdrawn. Supersedes
  [ADR-0019](0019-compiled-declarations-and-unified-method-dispatch.md) design decision 1 of its E2
  design ("rows are recognition metadata, not function pointers; invocation stays in the arity
  cascades").
- **Date**: 2026-10-03
- **Deciders**: tokuhirom, Claude
- **Issue**: [#11276](https://github.com/tokuhirom/mutsu/issues/11276)
- **Related**: [ADR-0019](0019-compiled-declarations-and-unified-method-dispatch.md) (E1 `TypeId`,
  E3 resolved-call cache, E4 the one resolver; E2 is the box this replaces),
  [ADR-0117](0117-str-methods-and-nqp-ops-share-one-routine.md) and
  [ADR-0118](0118-int-operators-share-one-routine.md) (one implementation per primitive),
  #7540 (the E2 design issue), #11271 (the drift this ADR removes the cause of)

## 1. Context

### 1.1 How a built-in method is dispatched today

A call to a built-in method passes through these layers, each written as a `match` on the method
name's `&str`:

| layer | where | size (2026-10-03) |
|---|---|---|
| admission gate | `vm/vm_native_dispatch.rs` `try_native_method_raw` | 991 lines, ~16 checks before the arity switch |
| the gate's twin for the interpreter path | `runtime/methods_native_bypass.rs` `should_bypass_native_fastpath` | 928 lines |
| pure arity cascades | `builtins/methods_0arg/`, `builtins/methods_narg/` | ~480 quoted-name arms |
| receiver-mutating helpers | `vm/vm_call_method_mut_ops.rs` and siblings | 3703 lines in the main file |
| slow path (`&mut Interpreter`) | `runtime/methods*.rs` | 88 files, ~56k lines, ~715 quoted-name arms |

Separately, the metadata about those methods (which type declares the method, which arities it
takes, whether a type object may call it) lives in `builtins/native_method_row_table.rs`. ADR-0019
E2 introduced it as *recognition metadata*: the resolver, `.^can` and `.^methods` read it, and
nothing invokes through it.

### 1.2 Why that is wrong

**Two sources of truth that nothing ties together.** A method exists when a `match` arm exists,
and introspection says it exists when a row exists. The rows were generated once (2026-08-10) and
baked from Rakudo once (#9869). Every arm added since then without a row made `.^can` lie. #11271
measured the result: `Rat.^can('numerator')`, `Num.^can('isNaN')`, `Int.^can('FatRat')` and
`Instant.^can('Bridge')` were all `False`, and 412 rows were missing on the types it could probe.
`native_call_unmodeled`, the counter E2b was meant to drive to zero, stood at 19842 hits over `t/`
on 2026-10-03. The #11271 oracle test now stops the drift for the probed types, but it can only
detect drift; it cannot make it impossible.

**The resolver cannot see native candidates.** E4's `resolve_sequence` builds the MRO-ordered
candidate sequence from user methods plus recognition rows. On a row miss it falls back to the
arity cascade (the structural fallback that replaced E2b's literal-zero gate), so the native base is
not in the sequence at all. That is why `nextsame` reaching a native base still needs synthesized
fallbacks (`native_*_next_candidate` in `builtins_dispatch_next.rs`).

**A string `match` costs per arm.** rustc lowers a `match` on `&str` to a sequence of length checks
and `memcmp`s in source order, with no hashing or jump table. The method name is already an
interned `Symbol`, but it is turned back into a `&str` to be matched. A late arm pays for every
arm before it, and for the gate's checks and the sub-dispatchers tried first (Match, hash-like,
`Label`, `Failure`, numeric subclasses in `native_method_0arg`).

**It violates the rule that a primitive has one implementation.** The gate and its bypass twin are
the same decision written twice. The pure/slow split also puts methods of one type in two places
for a reason unrelated to the language: whether the body needs `&mut Interpreter`.

### 1.3 How Rakudo models it

In Rakudo a built-in method is an ordinary `Method` in its class's `method_table`, written in the
CORE setting. `.^can`, `.^methods`, `.^lookup`, `nextsame`, `wrap` and `augment` work on built-ins
for that reason alone, with no special cases. MoarVM's method cache maps `(type, name)` to the
code object, and a call site's inline cache holds the result.

## 2. Decision

**A built-in method is one registered row, and the row is the only way to reach its
implementation.**

```rust
pub(crate) struct MethodRow {
    pub owner: TypeId,       // the type Rakudo declares it on (checked by #11271's oracle)
    pub name: Symbol,
    pub shape: CallShape,    // arities / named args admitted; signature for bind errors
    pub flags: RowFlags,     // TYPE_OBJECT_OK, MUTATES_RECEIVER, DECLARED, ...
    pub handler: Handler,
}

pub(crate) enum Handler {
    /// Needs no interpreter: callable from `value::gist`, constant folding and the JIT.
    Pure(fn(&Value, &[Value]) -> Result<Value, RuntimeError>),
    /// Calls closures, reads dynamic variables, does I/O.
    Interp(fn(&mut Interpreter, &Value, &[Value]) -> Result<Value, RuntimeError>),
    /// Writes through the receiver's container (`push`, `splice`, `substr-rw`).
    Mut(fn(&mut Interpreter, ReceiverPlace, &[Value]) -> Result<Value, RuntimeError>),
}
```

1. **Registration is the definition.** A method is written as a Rust function together with its
   row, in the family module that implements it (`Str`, numeric, `List`/`Array`, `Hash`/`Map`, ...).
   No other table names the method. A method without a row cannot be written, so introspection and
   dispatch cannot disagree.
2. **The pure/interpreter split becomes a property of the row**, not two dispatch structures. It
   stays because `value::gist` and constant folding call pure methods without an interpreter. One
   type's methods live in one place whatever their handler kind.
3. **One table for built-in and user methods.** The registry's per-type method entry carries
   built-in rows next to user `MethodDef`s. `resolve_sequence` walks the MRO once and yields
   `Native` candidates that can be *invoked*. `.^can`, `.^methods`, `.^lookup`, `nextsame` and
   `callsame` read the same sequence; the synthesized `native_*_next_candidate` fallbacks go.
4. **Receiver-state checks run once, before dispatch.** Lazy-Seq deferral, `Proxy`, mixins and
   `Failure` (which must explode, not dispatch) are a single guard step. Method-specific exceptions
   become flags on the method's own row. `try_native_method_raw`'s per-name checks and
   `should_bypass_native_fastpath` are deleted.
5. **The call site caches the handler.** The inline cache maps `(receiver TypeId, method
   generation)` to the resolved row. A monomorphic hit is one guard comparison plus one indirect
   call, for every method alike. The JIT can embed a `Pure` handler pointer as a constant.
6. **The table is static.** Building it must not run at `Interpreter` construction or on the
   startup path. Rows are `const`/`static` data. The lookup structure for cache misses is either
   generated and committed (the `unicode_name_data` precedent, checked by a test that regenerates
   it) or built lazily on the first miss, but only within a startup budget measured in slice 1.
   It must add no runtime dependency without a `KEEP` justification in `Cargo.toml`.

## 3. Consequences

- **Correctness.** `.^can` and `.^methods` cannot drift from dispatch. #11271's oracle test then
  checks only one thing: that each row's owner matches Rakudo.
- **Performance.** Every method costs the same; methods late in today's cascades get faster. The
  call is an indirect call where today's arm body is a direct call. Today's arms are already
  outside the VM loop, in a separate large function, so that call was not inlined before either.
  The perf gate in §5 decides.
- **Maintainability.** Adding a method is one function plus one row in one file. About 2,000
  lines of gate code and its twin go, along with the recognition-only row table, the
  `native_call_unmodeled` counter and E2's inverse-probe tests.
- **Cost.** The migration is large: about 1,200 quoted-name arms and the slow-path files behind
  them. It is done family by family (§6), and every slice deletes the arms it moves, so no method
  ever has two implementations.
- **Risk.** Today's arms rely on checks that ran earlier in the cascade: Match laziness, itemized
  hashes, numeric subclasses. Moving an arm out of the cascade loses those checks unless the
  family's guard reproduces them. Each slice must run the family's roast directories, not only
  `t/`.

## 4. What must keep working

- Every dispatch answer that roast and `t/` pin today, including type-object calls (`Str.gist`),
  user `is Array` subclasses delegating to storage, `Map` versus `Hash` owners, allomorphs, and
  `Rat` versus `FatRat`. These are ADR-0019 E2's pinned cases.
- Hand-written pre-dispatch fast lanes (`vm_native_map`, `vm_native_sort`, `vm_native_first`, ...)
  stay where they are. They become `Interp` handlers reached through the row instead of being
  matched by name.
- `wrap` and `augment` of a built-in method. They become possible for the first time, because the
  row is a real candidate. They have to be pinned when the resolver cutover lands.

## 5. Perf gate

Before slice 1 changes any dispatch, measure the current cost with callgrind (the `perf-tuning`
skill): a hot loop on an early arm (`.elems`), on a late arm (`.isNaN`, `.numerator`), and on a
slow-path method (`.map` with a block). Each migrated family must show those numbers at parity or
better, and bench CI (fib, bench-tak, one dispatch-heavy bench) must be at parity, before its
slice merges. ADR-0019 G3's "cache-hit dispatch remains generation-checked O(1)" still holds.

## 6. Slices

1. **Mechanism.** `MethodRow`, `Handler` and the static table; dispatch through the table with
   fallback to the cascades on a miss (the same structural fallback E4b uses, so nothing has to
   switch over at once); the inline cache holds the handler. Also: the baseline measurement (§5),
   and a ratchet counting the quoted-name arms left in the cascades and the slow path
   (`scripts/*-baseline.txt`, shrink only).
2. **Migrate one family end to end**, for example `Num`/`Rat`/`Int` numeric accessors. Move the
   arms, delete them from the cascades, and bring the family's receiver guards to the guard step.
   This slice proves the pattern and the perf gate.
3. **Remaining families.** Withdrawn as written ("one PR each"): it produced more than twenty
   PRs for 11% of the rows. §10 re-cuts the remaining work into macro-slices 3A-3G and states the
   rules that decide where a PR boundary may fall.
4. **Resolver cutover.** `Native` candidates in `resolve_sequence` become invocable; delete the
   `native_*_next_candidate` fallbacks; pin `nextsame`, `wrap` and `augment` on built-ins.
5. **Deletion.** The cascades, `try_native_method_raw`'s name checks,
   `should_bypass_native_fastpath`, `native_method_row_table.rs`, `native_call_unmodeled` and the
   E2 inverse probes.

## 7. Alternatives rejected

- **Keep recognition rows and enforce them with checks only** (the #11271 oracle and a ratchet on
  quoted names). This stops new drift on the probed types but keeps two sources of truth, the
  admission gate and its twin, and per-arm dispatch cost. It is what #11271 does as a stopgap,
  not an end state.
- **Write built-in methods in Raku, as Rakudo's CORE setting does.** This gives the most faithful
  MOP, but compiling a setting at startup, or loading a precompiled one, costs startup time, and
  Raku-level bodies are slower than Rust ones. The row table gives the same MOP shape without
  that cost.
- **A `HashMap<Symbol, fn>` built at `Interpreter` construction.** Simple, but it adds startup work
  to every process; §2.6 rules it out unless it is measured to be within budget.
- **Convert all ~1,200 arms in one PR.** This repeats the 2026-08-04 handler-ID attempt (b252837e7,
  reverted the same day as f1485d136), whose rows became load-bearing before they were complete.
  The fallback in slice 1 is what makes a family-by-family migration safe.

## 8. Open questions

- `ReceiverPlace`'s exact shape: a slot, an env name, or an attribute cell. ADR-0097's binding
  descriptor is the likely answer.
- Whether a row's `shape` carries a full signature (for Rakudo-style bind errors on built-ins) or
  only an arity mask, with the handler raising the error. Slice 3A settled named arguments
  (§9.15: a row declares the names it binds); positional binding stays an arity plus the
  handler's own declines.
- Where folded owners (`Buf`/`Blob`/`utf8` to `Blob`, `Sub`/`Method`/`Block` to `Code`) belong
  once owners are real `TypeId`s, versus Rakudo's MRO, where `Buf.^mro` does not contain `Blob`.

## 9. Implementation status

- 2026-10-03: accepted. Slice 1 (the mechanism) started.
- 2026-10-03, slice 1a: `src/builtins/method_table/` holds `MethodRow` / `Handler::Pure` and
  the `(DispatchShape, method) -> row` lookup, resolved along each shape's MRO from the built-in
  type catalog on first use. A miss falls back to the cascades. `DispatchShape` covers `List`,
  `Array`, `Hash`, `Str`, `Num` and `Rat`. The table replaced `builtins::fast_0arg`. Its rows
  are `List.elems/end/Bool`, `Map.elems/Bool`, `Str.chars/Bool`, `Num.isNaN` and
  `Rat.numerator/denominator`; the cascade arms that answer the same methods for other receivers
  call the same handlers. The lookup sits at the top of `try_native_method`. Debug builds
  re-answer every hit through the full pure path.
  - Baseline (§5), callgrind on the profiling build, 200,000 calls per benchmark, against the
    same `main`: `@a.elems` -15.3%, `%h.elems` -15.3%, `$s.chars` -11.6%, `$n.isNaN` -41.4%,
    `$r.numerator` -36.3%. A benchmark whose calls miss the table (`@a.map(*+1).elems`) moved
    +0.19%: about 50 Ir per iteration for two misses (a bit test on the symbol id), plus a
    `memcmp` shift with identical call counts.
  - No arm-count ratchet in CI. A first draft added one (`check-method-arms`, a global count of
    the cascades' quoted-name arms every PR had to keep equal to a committed baseline). It was
    dropped before merging: on 2026-10-03 the #11271 Rakudo oracle test, which had the same shape
    (one shared table that every parallel PR touching a native method had to edit), kept `main`
    red and made every concurrent agent's CI fail (#11405, #11407). A migration progress count
    belongs in a report, not in a gate that couples unrelated PRs.
  - Finding for the next slices: a `.elems` call on a variable spends ~6,300 Ir, of which only
    ~1,600 are in `try_native_method`. The rest is the `CallMethodMut` path's per-call probes
    before it (`native_lever_a_user_override_sym` ~500, `cool_type_object_string_method` ~450,
    `maybe_autothread_method_args`, `try_fast_accessor_read`, `reify_or_consume_seq_target`,
    `try_baggy_storage_delegate_mut`, `try_env_pure_mut_dispatch`). The call-site inline cache
    (§2.5) and the single guard step (§2.4) have to sit in front of those to pay off, so they
    are deferred to slice 1b instead of being bolted onto `try_native_method`.
- 2026-10-03, slice 1b: `vm/vm_method_site_lane.rs` answers a `CallMethodMut` from its row
  before the opcode's probe chain runs, in `exec_call_method_mut_site`. This is the single
  guard step (§2.4) for the shapes the table covers. The guard requires a site with no
  arguments, no modifier, no quoted name, no argument sources and no `@!`/`%!` receiver. The
  method name must be one the full path does not inspect before its native probe. No
  accessor-ref marker, no pending writeback and no user `find_method` may exist. The receiver
  must have a `DispatchShape` with a row, and no augment of its type may define the method.
  The call-site cache (§2.5) is `CompiledCode::method_sites`: one memo per method-name
  constant, holding `(shape, row)` for one registry write generation, so a hit skips the
  table lookup and the augment probe. It is never filled during a `native_base_bypass`. In
  debug builds the full path still runs and must agree with the lane. The whole `t/` suite
  (6173 files) passes with that check on.
  - Callgrind on the profiling build, 200,000 calls per benchmark, second run, against the
    same `main`: `@a.elems` 1,264M to 369M (-70.7%), `@a.end` -70.8%, `%h.elems` -70.8%,
    `$s.chars` -43.8%, `$n.isNaN` -47.8%, `$r.numerator` -73.0%. The miss case
    (`@a.map(*+1).elems`) moved +0.1%, and the empty loop did not move. A table-answered call
    on a variable now costs ~930 Ir per iteration, down from ~5,400.
  - Next: slice 2 migrates the first family. Rows that take arguments need the lane to grow
    the argument checks the full path makes (Junction autothreading, `use fatal` Failures).
    The plain `CallMethod` opcode (an inline receiver) does not take the lane yet.
- 2026-10-03, slice 2 (the `Rational` family): `numerator`, `denominator`, `nude`, `norm` and
  `isNaN` are rows owned by `Rat` and by `FatRat` (Rakudo composes the `Rational` role into
  each), all pointing at one handler per method in `method_table/rational.rs` that also covers
  big components. `Int` and `Complex` have `isNaN` rows. `DispatchShape` gains `Int` (inline,
  boxed and big), `FatRat` and `Complex`, and a big rational takes the shape its FatRat flag
  names. `native_method_0arg` now asks the table before its prologue, so every caller of the
  cascades reaches a row, and the five arms are deleted from `methods_0arg/coercion.rs`. What
  is left of the `isNaN` arm answers only receivers with no shape (`Bool`, `Instant`,
  `Duration`, ...). The debug cross-check runs the cascade without the table
  (`native_method_0arg_cascade`) and accepts a decline, since a migrated method has no arm.
  - Behaviour change, towards Rakudo: `Int` no longer answers `numerator`, `denominator`,
    `nude` or `norm` (Rakudo's `Int` does not do `Rational`), so the recognition rows
    `Int.numerator`/`Int.denominator` are gone too. `.norm` keeps the receiver's type for big
    components as well: the cascade arm turned a big `FatRat` into a `Rat`, and a big `Rat`
    whose reduced parts fit a word into a `FatRat`.
  - The lane tests the method name against the table's bit set before probing the receiver,
    because an `Int` now has a shape. Without that test, every `Int` call with no row
    (`$i.abs`) took the memo's lock and missed, which cost +2.0% on that benchmark.
  - Callgrind on the profiling build, 200,000 calls per benchmark, second run, against the
    same `main`: `$f.numerator` (FatRat) 1,785M to 313M (-82.5%), `$i.isNaN` -75.4%,
    `$c.isNaN` (Complex) -83.5%, `$r.nude` -73.3%. Calls the table does not answer:
    `$i.abs` +0.2%, `$s.uc` -1.5%, `@a.map(*+1).elems` +0.2%, the empty loop unchanged.
    `$r.numerator`, already a lane hit, moved +2.3% (~35 Ir per call): the handler now
    matches three rational views instead of one, and the shape probe has more arms.
- 2026-10-03, slice 3, first family (the text methods): `codes`, `ord`, `uc`, `lc`, `fc`,
  `tc`, `tclc`, `wordcase`, `flip`, `trim`, `trim-leading`, `trim-trailing`, `chomp` and
  `chop` are rows owned by `Str` and again by `Cool`, as in Rakudo, where `Cool`'s candidate
  stringifies the invocant and calls `Str`'s. Both owners' rows point at one handler per
  method in `method_table/str.rs`, which reads the receiver through
  `grapheme_index::with_str`, so every shape whose MRO has `Cool` (all of them but `Str`)
  finds the `Cool` row. The cascade arms stay for receivers with no shape (`Bool`, `Cool`
  subclass instances, type objects) and call the same handlers.
  - The `Cool` rows are what keeps the shapes other than `Str` from paying for the new names.
    With only `Str` rows, `42.flip` passed the name bit test, missed for the `Int` shape and
    ran the lane's resolve on every call: +7.2% on that benchmark.
  - Callgrind on the profiling build, 200,000 calls per benchmark, second run, against the
    same `main`: `$s.codes` 1,376M to 332M (-75.9%), `$s.ord` -75.0%, `$s.chomp` -74.5%,
    `$s.trim` -72.2%, `$s.flip` -68.1%, `$s.uc` -25.3% (the case map itself dominates),
    `$i.flip` (Int, through `Cool`) -60.8%, `@a.flip` -63.4%. Calls the table does not
    answer: `$i.abs` +0.2%, `$s.comb` 0.0%, the empty loop unchanged.
- 2026-10-04, slice 3, the numeric family: `abs`, `sign`, `floor`, `ceiling`, `round` and
  `truncate` with no arguments are rows owned by `Int`, `Num`, `Rat`, `FatRat` and `Complex`
  (Rakudo has a copy in each type's method table, some composed from `Real` and `Rational`).
  `Complex` has no `sign` row, because Rakudo's `Complex.sign` comes from `Cool`. Every owner's
  row points at one handler per method in `method_table/real.rs`. The cascade arms call the
  same `*_of` functions and keep only the receivers with no shape (`Bool`, enums,
  `Duration`/`Instant`).
  - The rational rounding goes through `int_div`, the one floored-division routine
    (ADR-0118, enforced by `check-prims`): `ceiling` is `-((-n) div d)` and `round` is
    `(2n + d) div 2d`.
  - Three wrong answers are fixed on the way:
    - a `Num` past a machine word floors, ceilings and truncates to a big `Int` instead of
      saturating at `i64`;
    - a word-sized `Rat` rounds exactly instead of through an `f64`;
    - `arith_negate` keeps a rational with an `i64::MIN` numerator `Rational` instead of
      degrading it to a `Num`, which also fixes its `.abs`.
  - Callgrind on the profiling build, 200,000 calls per benchmark, second run, against the
    same `main`: `$c.abs` (Complex) 1,854M to 325M (-82.5%), `$r.floor` -80.1%, `$n.round`
    -73.2%, `$i.sign` -72.8%, `$i.abs` -72.4%, `$n.floor` -72.4%, `$r.round` -48.5% (the
    exact rounding costs three integer operations through the shared arithmetic
    routines). A call the table does not answer (`$i.chars`) and the empty loop are
    unchanged.
- 2026-10-04, slice 3, the first rows that take arguments (Str's search family): `contains`,
  `starts-with`, `ends-with`, `index` and `rindex` with one needle, and `substr` with a start
  and an optional length, are rows owned by `Str` and by `Cool`, both pointing at one handler
  per method in `method_table/str_search.rs`. The mechanism grows four things:
  - Rows are keyed by `(shape, method, arity)`, so `substr` has a one- and a two-argument
    row. The table's name test is a per-name bit set of arities, so a call with an argument
    count no row of that name takes (`$s.index($n, $from)`) is refused before the receiver
    is probed.
  - A row is handed only plain scalar arguments (`Str`, `Int`, `Num`, `Rat`, `FatRat`;
    `plain_args`). Named arguments reach the native layer as `Pair`s and are
    indistinguishable from positional ones there. A `Junction` must autothread, a
    `Failure` may explode under `use fatal`, and a lazy `Seq` must be reified; each of
    those, and a `Regex` or list needle, takes the cascades.
  - `Handler::Narrow` is a pure handler that may decline: `None` means the arguments are
    outside the row's signature, the way a multi candidate fails to bind. `substr` binds
    only a non-negative `Int` start inside the string; a negative or `WhateverCode`
    position, a `Range` and an out-of-range start (which answers a `Failure`) decline.
    This answers §8's second open question for now: the shape is an arity, and finer
    binding lives in the handler.
  - The call-site lane admits sites with up to two arguments. It requires the site's
    argument-source descriptor to name only positional arguments (no named argument, no
    `|` spread), and every argument to be plain. Its memo payload carries the arity, and
    it remembers misses as well as rows. Without that, a name with a row for another shape
    or arity repeated the lookup on every call.
  - Callgrind on the profiling build, 200,000 calls per benchmark, second run, against the
    same `main`: `$s.contains($n)` 2,168M to 623M (-71.3%), `$s.starts-with($n)` -71.9%,
    `$s.rindex($n)` -71.7%, `$s.index($n)` -70.5%, `$s.substr(6)` -65.6%,
    `$s.substr(6, 5)` -64.4%. `$i.chars` (an `Int`, no row) -3.7% from the remembered
    miss. `$s.index($n, 3)` (no row for two arguments) +0.2%, down from +3.2% before
    the miss memo and the arity bits. The empty loop is unchanged.
- 2026-10-04, slice 3, the numeric coercions: `Int`, `Num` and `Bool` with no arguments are
  rows owned by `Int`, `Num`, `Rat` and `FatRat`, and `Complex` has a `Bool` row.
  `Complex.Int` and `Complex.Num` read `$*TOLERANCE`, which needs the interpreter, so they
  stay off the table (#11795 records that `.Int` ignores it today).
  `method_table/coerce.rs` holds the one implementation. The cascade's `.Int`/`.Num` arms
  and `Str.Int`'s numify-then-truncate path call it, which replaces the per-type copies
  (`numeric_to_int` and the arm bodies). A `Num` past a machine word now truncates to a
  big `Int` instead of saturating at `i64` (`1e30.Int`, `"1e30".Int`).
  - The table gains a per-name shape mask next to the arity bits. A receiver whose shape
    has no row of that name (`"42".Int`, once `Int` has rows on the numeric types) is
    refused by a bit test, with no memo lock and no hash lookup.
  - `try_native_method_raw` calls the 0-arg cascade without the table:
    `try_native_method` asked the table first, and the raw path's lever-A gate refuses
    every shaped receiver with an augment, so the second probe could only repeat the
    first.
  - Callgrind on the profiling build, 200,000 calls per benchmark, second run, against
    the same `main`: `$r.Num` 1,837M to 338M (-81.6%), `$r.Int` -77.7%, `$i.Int` -71.0%,
    `$i.Num` -70.8%, `$n.Int` -69.6%. `$i.Bool` and `$n.Bool` are -38%: `Bool` is a name
    the call-site lane leaves to the full path (`scalar_early_lane_skips`), so only the
    table answers it. `"42".Int` (a `Str`, no row) is +1.3%, which is the lane's and the
    native entry's shape probe and bit tests (about 50 instructions each); it was +6.8%
    before the miss memo, the shape mask and the raw-path change. The empty loop is
    unchanged.
- 2026-10-04, slice 3, stringification: `List.join` with and without a separator, and `Str`
  with no arguments on `Str`, `Int`, `Num`, `Rat`, `FatRat` and `Complex`, are rows. Both
  are `Handler::Narrow`.
  - `join` declines a list that holds an element needing the interpreter (an instance or
    mixin whose `Str` may be user code, a Junction, a deferred Seq), a `Proxy` (FETCHed at
    render time, ADR-0040 §9.2) or an undefined element (Rakudo warns for each one,
    #11838).
  - `Str` declines a zero-denominator rational, whose `X::Numeric::DivideByZero` carries
    the interpreter's context.
  - The 0-argument and 1-argument `join` cascade arms had diverging copies of the element
    walk: only the 1-argument one resolved `is default` holes and refused mixin and lazy
    elements. Both now call `method_table::list::join_source_items` and `join_items`.
  - A Pair's `.join($sep)` is the Pair's own `.Str` (`a\t1`), as in Rakudo. The 1-argument
    arm had joined key and value with the separator.
  - Callgrind on the profiling build, 200,000 calls per benchmark, second run, against the
    same `main`:
    - `$s.Str` 1,311M to 359M (-72.6%);
    - `$i.Str` -63.2%, `$n.Str` -55.7%, `$r.Str` -53.8%;
    - `$l.join("-")` (a List) -52.8%, `@a.join` -51.7%, `@a.join(",")` -49.4%.
    - `%h.Str` (a `Hash`; no row) is +0.8%, the same residual cost of the two bit tests
      noted for `"42".Int`. The empty loop is unchanged.
- 2026-10-04, slice 3, the count family: `keys`, `Numeric` and `Int` on `List` and on `Map`,
  and `chars` on `Cool`, are rows. Arrays and hashes reach them through `List` and `Map`.
  - `List.keys` is the same lazy counting Seq the cascade built (`ListGen::positional`).
  - `Map.keys` yields an object hash's real key objects and a plain hash's decoded `Str`
    keys.
  - The cascade's `.keys` arm calls both handlers.
  - `chars` moves into the text rows `Str` and `Cool` share, so `$i.chars` and
    `@a.chars` (the stringified list) reach the one handler.
  - Callgrind on the profiling build, 200,000 calls per benchmark, second run, against the
    same `main`: `@a.Int` 1,969M to 396M (-79.9%), `%h.Numeric` -79.3%, `$i.chars` -64.9%,
    `@a.keys` -62.1%, `%h.keys` -60.0%. `@a.sum` (no row) -0.2%; the empty loop is
    unchanged.
- 2026-10-04, slice 3, Str's iteration family: `comb`, `words`, `lines` and `ords` with no
  arguments are rows owned by `Str` and `Cool` (`method_table/str_iter.rs`).
  - `comb`, `words` and `lines` answer the same lazy `Seq` over the receiver's string form
    as before (`value::str_iter_seq`); `ords` answers a `Seq` of the NFC codepoints.
  - The cascade arms call the same handlers. `lines` keeps its guard for `Supply` and
    `IO` instances, which have no shape.
  - Callgrind on the profiling build, 200,000 calls per benchmark, second run, against the
    same `main`: `$s.comb` 1,971M to 931M (-52.8%), `$s.words` -52.8%, `$s.lines` -52.2%,
    `$s.ords` -48.1%, `$i.comb` (an `Int`, through `Cool`) -42.7%. `$s.flip` (an existing
    row, as a control) and the empty loop are unchanged.

### 9.1 Complex component family (2026-10-04)

`Complex.re`, `im`, `reals` and `conj` are zero-argument rows in
`method_table/complex.rs`. The cascade's Complex cases call the same handlers,
and the non-Complex `conj` cases share that handler's identity branch. The
remaining `re` and `im` cascade cases on non-Complex numeric receivers disagree
with Rakudo and are tracked separately as #11949; this slice does not change
their behavior.

Callgrind on profiling builds from the same `main`, 50,000 calls per benchmark,
second run on each binary: `$z.re` went from 632,196,562 to 270,594,032
instructions (-57.2%), and `$z.reals` from 733,841,605 to 341,787,961
(-53.4%). The benchmarks have no module load and use the same loop and receiver
on both sides. The `Complex` roast file and the focused method-row test pass.

### 9.2 Successor and predecessor family (2026-10-05)

`Str`, `Int`, `Num`, `Rat`, `FatRat` and `Complex` now declare `succ` and
`pred` rows in `method_table/succ_pred.rs`. Each row calls the shared
`value_succ` / `value_pred` implementation (ADR-0118), and the cascade uses
those same handlers for receiver shapes that have no row. This keeps Bool's
step and values requiring user dispatch on the existing path.

Callgrind on profiling builds from the same `main`, with a 50,000-iteration
loop alternating `Int.succ`, `Int.pred` and `Str.succ`, fell from 1,035,535,695
to 331,319,634 instructions (-68.0%). The input has no module load; each side
was run twice and the second run recorded. The focused method-row test, the
increment/decrement roast files and the numeric operator parity tests pass.

### 9.3 List and Array reverse rows (2026-10-05)

`List.reverse` and `Array.reverse` are zero-argument rows with one shared
handler in `method_table/list.rs`, matching Rakudo's separate declarations.
The native cascade calls that same handler for receivers without a List or
Array dispatch shape. The focused test pins both owners and the reversed Seq
result for each collection shape.

Callgrind on profiling builds from the same `main`, with a 50,000-iteration
loop calling both a List and an Array receiver each iteration, fell from
1,835,310,992 to 617,706,226 instructions (-66.3%). Both binaries were run
twice and the second run is recorded; the benchmark has no module load.

### 9.4 Map view methods (2026-10-05)

`Map.values`, `kv`, `pairs` and `antipairs` are zero-argument rows owned by
`Map`. `Hash` resolves the same rows through its MRO. Each cascade's `Hash`
case calls the shared handler, which preserves dereferenced values and the
original key objects of object hashes.

Callgrind on profiling binaries, with a 20,000-iteration loop over a two-entry
Map that calls each view and reads its `.elems`, fell from 1,631,207,873 to
1,497,226,995 instructions (-8.21%). Both binaries were run twice and the
second run is recorded. The baseline was built from `0f0ffdf`; the only
intervening `main` commit (`e4ef467`) changed documentation, so its executable
sources were identical.

### 9.5 Counted `Any.head` and `Any.tail` (2026-10-05)

The one-argument forms are rows owned by `Any` for every receiver shape the
table covers. `method_table/list.rs` holds their handlers, and the native
cascade calls those same handlers for shapes outside the table. `Handler::Narrow`
keeps unsupported argument forms on the cascade path. `tail` returns an empty
Seq for a non-positive `Int` count before unsigned slicing; the old native
gate bypassed `tail`, so this guard is part of safely admitting its plain
receiver shapes. Array results still carry writable element cells.

Callgrind on profiling builds against `f643c43`, with 20,000 iterations
calling `head(3)` and `tail(3)` on both a List and an Array each iteration,
fell from 1,635,670,247 to 637,274,373 instructions (-61.04%). Each binary was
run twice and the second run is recorded; the benchmark has no module load.

### 9.6 Collection transformations (2026-10-05)

`Any.flat`, `List.flat` and `Array.flat`, `Any.sort` and `List.sort`, and
`Any.unique` and `Any.repeated` are rows in `method_table/list_transform.rs`.
Each handler is also the implementation used by the native cascade for
receivers the table cannot cover. `sort` keeps its interpreter path for a
comparator argument or an Array whose elements need a user-dispatched
stringifier; the pure row handles zero-argument reified Arrays, plain Hashes
and simple scalars. Seq, Range, Set/Bag/Mix, Uni and itemized Array sorting
keep their collection-specific path. `unique` decomposes a plain Hash into its
Pair elements and declines collections whose identity keys require user
`WHICH` dispatch. The test pins nested List flattening, itemized Array
children, collection-specific sort fallbacks, Hash pairs, unique and repeated
results, user `WHICH`, nonzero-arity fallback and repeated call sites. The
focused method-row test and four relevant `S32-list` roast files pass.

### 9.7 Per-slice measurement policy (2026-10-05)

By user decision, do not run a local callgrind A/B or require a per-family
performance number for the remaining migration slices. The first slice and
multiple later family slices already measured the row lane's repeated dispatch
gain. This supersedes §5's per-family measurement requirement for the rest of
ADR-11276's migration; focused behavior checks and the ordinary pre-publication
gate still apply. Standard CI benchmark reporting is unchanged.

### 9.8 List aggregate family (2026-10-05)

Any now owns rows for minmax and sum; List owns permutations and combinations,
which Array inherits through its MRO. The rows and native cascade call the same
implementations. Any's aggregate rows cover all nine plain receiver shapes;
Hash is viewed as its List of Pairs, so `Hash.minmax` compares those Pairs and
`Hash.sum` reports the missing `Numeric(Pair)` candidate. Sum keeps its Junction
handling, Raku numeric string grammar, numeric promotion and range behavior.
The combinator methods keep their lazy Seq results and ordering.
Squish remains on the interpreter path: its default comparison is ===/WHICH and
can call a user method, which a Pure row cannot. The focused aggregate
method-row test checks the Rakudo owners, scalar/Hash behavior and Seq/Range
cascade paths; all files under roast/S32-list pass.

### 9.9 Collection invert family (2026-10-05)

`List.invert` and `Array.invert` are rows owned by their respective positional
types, and `Map.invert` is inherited by `Hash`. All three rows call the
existing Pair-expansion implementation, so list values in Pair payloads still
fan out and object hashes retain their typed keys. The cascade uses the same
handler for unshaped Pair collections; Set, Bag and Mix keep their specialized
invert behavior, and invalid positional elements retain the existing type
check path. The focused method-row test covers all three owners, Hash
inheritance, typed keys and repeated Pair expansion.

### 9.10 List positional views (2026-10-05)

`List.values`, `kv`, `pairs` and `antipairs` are now rows owned by `List`, and
`Array` reaches the same handlers through its MRO. The handlers use the existing
lazy `ListGen::Positional` modes, so values remain decontainerized and positional
keys remain live when the result is consumed. The cascade's Array/List cases call
the same handlers; shaped arrays, Sets/Bags/Mixes and lazy or user-defined
receivers retain their specialized paths.

The focused method-row test covers List and Array results and repeated call sites.
The relevant `roast/S32-list` view tests pass. `pairup` and `cache` were
investigated but not moved: the current native owner catalog does not mark those
methods as declared rows at their apparent owners, so adding them would violate
the owner-consistency invariant. They remain on the existing cascade until their
catalog status is resolved.

### 9.11 Any scalar collection family (2026-10-05)

`Any.elems`, `end`, `keys`, `values`, `kv`, `pairs`, `antipairs` and `reverse`
are now rows in `method_table/any_collection.rs`. The handlers implement Any's
one-element scalar semantics and are reached for the plain scalar dispatch
shapes; List and Map rows remain more specific in the MRO. `Any.end` and
`Any.reverse` also cover Hash where no more-specific row exists. The native
cascade calls the same handlers for Bool and other scalar representations that
do not have a dispatch shape, while Range, lazy Seq, Set/Bag/Mix, Pair and
user-defined receivers retain their existing specialized paths.

The focused row test covers scalar and text receivers, concrete List/Map row
precedence, and Range reverse fallback. The collection and numeric TAP suites
(209 files, 3056 tests) and all 50 `roast/S32-list` files (1865 tests) pass.

### 9.12 Collection search worries (#11760, 2026-10-05)

`List` and `Array` calls to `contains`, `index` and `rindex` keep using the
shared `Cool` search handlers and now return Rakudo's resumable "did you mean"
worry with the existing stringified-search result. `Map.contains` and
`Map.index` are rows owned by `Map`; `Hash` reaches them through its MRO, and
the warning names the concrete receiver type. The native one-argument cascade
uses the same handlers, and `settle_native_warning` resumes them at the call
site. The focused test pins both the warning text and the preserved result.
### 9.13 Plain positional representation conversions (2026-10-05)

`List.list`, `List.List` and `List.Array` are rows in
`method_table/positional.rs`. `Array` reaches the same handlers through the
`List` MRO. The rows admit only non-shaped, non-lazy positional values: plain
List values share their backing storage, and `.Array` always creates a fresh
real Array with itemized elements. Shaped and lazy arrays remain on the existing
cascade because their conversions need dimensional defaults or lazy context.

The focused method-row test covers both positional representations, fresh
Array storage, itemization, and repeated conversions.

### 9.14 Extrema and eager collection family (2026-10-05)

`Any.min`, `max`, `minpairs` and `maxpairs` are now pure `Handler::Narrow`
rows for plain positional and scalar receivers. Hashes use the same rows with
their typed-key/value ordering, while ranges, lazy values, user-comparison
values and unsupported receivers decline to the interpreter cascade. `List`
also owns `eager`, `item`, `sink` and `is-lazy` rows; already-eager Lists and
Arrays return themselves, itemization stays on the shared representation, and
lazy or shaped values retain the existing interpreter path.

The shared handlers preserve first-winner and all-ties behavior for extrema
pairs. The focused collection aggregate test covers scalar, List, Array and
Hash results, eager identity, empty and fallback behavior. The method-table
unit suite verifies owner declarations and that every row answers its resolved
plain shape.

### 9.15 Slice 3A: the guard step (2026-10-06)

Branch `refactor/11276-3a-guard-step`. What landed, and where it differs from §10.5.

- **Groups.** `method_table/` has one directory per slice group (`scalars`, `collections`,
  `instances`, `io_concurrency`, `mutating`, `ctors_mop`), each with its own `FAMILIES` list that
  `table.rs` concatenates, so a slice adds its rows in its own directory and never edits a shared
  list. `tests/` has one module per group beside the generic invariants. The row types
  (`row.rs`) and the guard step with its entries (`dispatch.rs`) are files of their own.
- **Row flags and named arguments.** `MethodRow` gained `flags` (`TYPE_OBJECT_OK`, `ANY_ARGS`)
  and `named`, the names the row binds. This answers §8's question for named arguments: the
  shape stays an arity, and the guard step splits string-keyed `Pair`s (the named flavour,
  ADR-0021) from the positionals, finds the row by its positional arity, refuses a name no row
  binds (the cascades then apply the implicit `*%_`), and gives the handler a `Named` view.
  Row-declared names replace `accepted_nameds` as rows migrate; slice 5 deletes that table.
- **Argument admission.** A row is handed plain scalars by default, as before. `ANY_ARGS` opts
  in to any plain argument: the allowlist `Value::is_plain_argument` (a tag probe) refuses a
  `Junction` (which must autothread), a `Seq`, `LazyList`, `Slip` or thunk (which must be
  reified), a `Proxy`, container or variable reference, a `Mixin` or `Instance` (their `Str` may
  be user code), a shaped or lazy array, an itemized hash and a lazy `Match`. Those calls take
  the cascades exactly as before.
- **Handler kinds.** `Handler::Named` reads named arguments; `Handler::Interp` needs the
  interpreter. `Interpreter::try_native_method`, the VM's call-site lane and
  `call_method_with_values` reach interpreter rows; the pure entries (the cascades' own
  prologue) decline them. The caller's veto (an `augment` of the receiver's type) runs before an
  interpreter handler, which has effects, so the debug cross-check never re-runs it: it is
  answered once, by the lane or by `try_dispatch_in`.
- **Shapes.** `DispatchShape` moved to `value/dispatch_shape.rs` and gained `Bool`, `Range`,
  `Pair`, `Capture`, `Version`, `Uni`, `Set`, `SetHash`, `Bag`, `BagHash`, `Mix`, `MixHash`,
  `Date` and `DateTime`. Value kinds decode by tag probe; `Date` and `DateTime` by the class
  name of a built-in `Instance`, so a user subclass has no shape. A shape added after the
  first nine is **closed**: only rows its own type owns reach it, because an ancestor's row
  (`Any.elems`) was written for the shapes that existed and would answer a `Range` wrongly. The
  slice that owns a shape audits the ancestor rows and opens it (`DispatchShape::inherits`).
  `Receiver` (a shape plus a type-object bit) is the lookup key, the call-site memo byte and the
  guard step's input; the per-name shape masks are 64 bits wide.
- **Type objects.** The type object of a built-in type (`Package("Int")`) is a receiver, and
  answers only a row flagged `TYPE_OBJECT_OK`: the numeric `Bool` rows, so `Int.Bool` is
  `False` through the table.
- **Proof rows** (each a real migration; the cascade arm calls the handler): `flat(:hammer)`
  on `Any`/`List`/`Array` (`Named`), `List.combinations($of)` with an `Int` or a `Range`
  (`ANY_ARGS`), `Any.collate`, which reads `$*COLLATION` (`Interp`), `Bool.key/value/Bool`,
  `Uni.Bool`, `Version.parts/plus/whatever`, `Pair.key/value/antipair`,
  `Range.excludes-min/excludes-max`, `Bool` on `Range`, `Capture` and the six quant-hash owners,
  `Date.year/month/day` and `DateTime.year/month/day/hour/minute`. The `isNaN` arm no longer
  takes "has a shape" to mean "is numeric".
- **`scripts/method-rows-report.py`** prints the arms left per cascade layer and file, the
  registered rows per group and handler kind, and the recognition rows with no registered row.
  The row dump is the ignored unit test `method_table::tests::dump_rows`. On this tree: 1,260
  quoted-name arms (747 distinct names), 219 registered rows, 1,451 of 1,649 recognition rows
  left. It is a report, not a gate.

Where it differs from §10.5:

- **Shapes are added where a row needs them.** `Seq` (a method decides whether it consumes),
  `Match` (whether it forces), `Failure` (which must explode for every method it does not
  declare), `Nil` and `Junction` (autothreading is the guard) have a method-dependent guard, so
  the slice whose first rows decide it adds the shape (3C for `Seq`, 3D for `Match`, `Failure`
  and `Nil`). `Instant`, `Duration`, `IO::Path`, `IO::Handle`, `Blob`, `Buf` and `Code` are one
  variant and one class-table entry each, added with their first row in 3B, 3D and 3E; a shape
  with no row would be dead data.
- **The cascade functions were not split per owner group.** `dispatch_core` is already a
  sequence of receiver-kind blocks that fall through in order, so a pure move would have to
  keep that order across files, and nothing in 3A needs it. Two slices that delete arms in the
  same block rebase over a hunk; if that proves costly, the split is a pure-move PR of its own.
- **The report is a Python script, and no diff-based "no new arm" check was added.** That check
  would be a new gate on every PR; AGENTS.md's slow-path rule stays a review rule.
- **The lane declines a site that passes a named argument or a `|` spread.** Such a call
  reaches its row through the native entry instead. An interpreter row reached through the lane
  gets copies of the receiver and the arguments, because it may call back into the VM.
- **Perf** is not measured (§9.7). The hot-path change is `Receiver::of` replacing
  `dispatch_shape()` in the lane after the name bit test, which adds one tag probe for a
  `Package` receiver; Bench's deterministic series on `main` is the watch.

Findings filed: [#11989](https://github.com/tokuhirom/mutsu/issues/11989) (`Duration.new(0).Bool`
is `True`), [#11990](https://github.com/tokuhirom/mutsu/issues/11990) (`Version.parts` answers
`Whatever` for `*`) and [#11992](https://github.com/tokuhirom/mutsu/issues/11992) (`Int.Num` on a
type object).

### 9.16 Slice 3C: collections and quant hashes (2026-10-06)

Branch `refactor/11276-3c-collections`. Owners: `Any`, `List`, `Array`, `Hash`, `Map`, `Range`,
`Seq`, `Pair`, `Capture`, `Set`, `SetHash`, `Bag`, `BagHash`, `Mix`, `MixHash`, and the three
small owners the report files under *collections* (`Junction`, `Nil`, `Iterable`). This is the
first commit's inventory, taken with
`scripts/method-rows-report.py --inventory collections,"quant hashes"`; later commits tick
families off and the closing paragraph records what was deferred.

**Inventory (unregistered recognition rows, 2026-10-06).** 544 recognition rows over 116
method names, of which only **354 can be registered**: a row's owner must be the type Rakudo
declares the method on (`rows_are_declared_by_rakudo` enforces it against
`rakudo_method_tables.txt`). The other 190 (`Array.keys`, `Hash.Str`, `Any.say`, ...) are
*inherited-only*: Rakudo declares the method on an ancestor, so the call is served by the
ancestor's row once the receiver's shape inherits it, and the recognition row simply
disappears with the table in slice 5. `--inventory` prints both counts.

| owner | declared | Pure | Interp | Mut | inherited-only |
|---|---:|---:|---:|---:|---:|
| Any | 21 | 7 | 14 | 0 | 22 |
| List | 25 | 18 | 0 | 7 | 24 |
| Array | 19 | 11 | 0 | 8 | 56 |
| Hash | 13 | 9 | 2 | 2 | 30 |
| Map | 19 | 19 | 0 | 0 | 0 |
| Range | 32 | 32 | 0 | 0 | 8 |
| Seq | 26 | 26 | 0 | 0 | 11 |
| Pair | 17 | 17 | 0 | 0 | 21 |
| Capture | 18 | 18 | 0 | 0 | 0 |
| Junction, Nil, Iterable | 4 | 4 | 0 | 0 | 3 |
| Set, SetHash | 48 | 48 | 0 | 0 | 5 |
| Bag, BagHash | 57 | 54 | 1 | 2 | 5 |
| Mix, MixHash | 55 | 54 | 1 | 0 | 5 |
| **total** | **354** | **317** | **18** | **19** | **190** |

The recognition table flags 34 rows `MUTATES_RECEIVER`; 19 of them are declared rows. The
rows that really write the receiver (`push`, `pop`, `shift`, `unshift`, `append`, `prepend`,
`splice` and `rotate` on `List` and `Array`, `push` and `append` on `Hash`, `BagHash.add` and
`remove`) move in 3F with `Handler::Mut`. The others (`map`, `grep`, `reduce`, `produce`,
`rotor`, `categorize` and `classify` on `List` and `Array`) call a closure and do not write the
receiver: the flag is the 2026-08-10 planning estimate, and each is inherited from `Any`'s
interpreter row anyway. That leaves **335 declared rows for 3C** (317 Pure, 18 Interp), cut by
method name, not by owner (ADR §10.3 rule 3): one handler answers a name for every owner and
shape that has it, and the cascade arms for that name go, or shrink to the other groups'
receivers for a name several groups share (`gist`, `Str`, `Numeric`, ...). `Any.say`, `put`,
`print`, `note`, `HOW`, `WHAT`, `WHY`, `defined`, `not`, `so` and `self` are declared on `Mu`
and belong to 3D.

**What landed (209 declared rows at the start, 92 left; 676 -> 821 rows registered).** Four
families, one commit each with its focused test (`t/oo/method/date-method-rows.t`,
`datetime-method-rows.t`, `instant-duration-method-rows.t`, `t/regex/match/match-method-rows.t`),
each compared with Rakudo and checked against the roast directories of its owners and the unit
suite (`cargo test --lib method_table native_method_row`):

- [x] *Calendar rows of `Date` and `DateTime`* (`instances/dateish.rs`, 44 rows): `day-of-month`,
  `day-of-week`, `day-of-year`, `daycount`, `days-in-month`, `days-in-year`, `is-leap-year`, `week`,
  `week-number`, `week-year`, `weekday-of-month`, `formatter`, `yyyy-mm-dd` / `mm-dd-yyyy` /
  `dd-mm-yyyy` / `mm-dd` / `yyyy-mm` with and without a separator. One handler per name, both
  owners' rows pointing at it (Rakudo composes `Dateish` into both).
- [x] *`Date`'s and `DateTime`'s own rows* (`instances/date.rs`, `datetime.rs`, 34 rows): `succ`,
  `pred`, `first-date-in-month`, `last-date-in-month`, `second`, `timezone`, `offset*`,
  `whole-second`, `hh-mm-ss`, `posix`, `utc`, `julian-date`, `modified-julian-date`, `day-fraction`,
  the coercions (`Date`, `DateTime`, `Instant`, `Int`, `Numeric`, `Real`), `WHICH`, `raku`, `Str`,
  `gist`. A value with a `:formatter` is rendered by running that Callable, which only the
  interpreter can do: the `Str`/`gist` rows decline it and the interpreter's path answers.
- [x] *`Instant` and `Duration`* (`instances/instant.rs`, 50 rows): two new instance-class shapes.
  Both do `Real` and hold their seconds in one number, so a handler asks the question of that number
  and wraps the answer only where the method keeps the type (`abs`, `succ`, `pred`): `Bool`,
  `Bridge`, `Int`, `Num`, `Rat` and `FatRat` (with and without an epsilon), `Complex`, `Numeric`,
  `Real`, `conj`, `Str`, `gist`, `raku`, `abs`, `narrow`, `isNaN`, `tai`, `succ`, `pred`, `to-nanos`,
  `rand`, and `Instant`'s `to-posix`, `Date`, `DateTime`, `Instant`. Every cascade arm that matched
  `class_name == "Instant" | "Duration"` is gone: only the built-in classes ever took them, and the
  shapes only match those. (`Duration.Bool` is #11989, fixed meanwhile by #12014; its test is pinned
  here too.)
- [x] *`Match`* (`instances/regex_match.rs`, 17 rows): `from`, `to`, `pos`, `Str`, `Bool`, `orig`,
  `target`, `made`, `ast`, `clone`, `prematch`, `postmatch`, `actions`, `caps`, `chunks`, `gist`,
  `raku`. The shape covers a plain `Match`, lazy or eager; a grammar cursor (its class is the
  grammar's own and may override any of these) and a subclass have none, and the cascade's `Match`
  blocks keep answering them by calling the same handlers.

Date, DateTime, Instant and Duration are **open** shapes (`DispatchShape::inherits`): the audit ran
every registered `Any`, `Cool` and `Mu` row against each of them, 119 name/arity pairs, mutsu before
and after the change and Rakudo (a probe script generated from the row dump, kept in `tmp/` only).
Answers moved to Rakudo's: 27 for `Instant`, 22 for `Duration` (`Instant.sqrt` was "No such
method", `Instant.Complex` was `0+0i`), 1 each for `Date` and `DateTime`; none regressed (a
regression is an answer that matched Rakudo before and does not now). Six `Instant`/`Duration`
answers changed without reaching Rakudo's (the last digit of a float, the spelling of an error, a
`Seq` of roots), and 27 answers of methods Rakudo does not give a `Date` at all (`sin`, `cos`, ...;
mutsu's generic numeric fallback answers them, as before) changed value because `Date.Numeric`
is now the day count. `Match` stays **closed**: its list-like and
string-delegating methods are the cascade's (a `Match` is a `Capture` and, through the Cool
delegation of its `Str`, a `Cool`).

What the work taught, which the remaining families follow:

- **Opening a shape means every ancestor row answers it.** `every_row_is_reached_and_answers` fails
  on the first row that declines, so `scalar_like` (the test `Any`'s one-element rows assert) now
  includes the four instance classes, and `Any.min/max/minpairs/maxpairs/sort` accept them.
  `numify`, the one place a `Cool` receiver becomes a number, reads the seconds of an `Instant` or
  `Duration`; so do the native integer coercions (`int8` .. `uint64`, `byte`), which read their
  receiver directly.
- **The debug cross-check wants the cascade to decline, or to agree.** A cascade arm that answers a
  shaped receiver with a placeholder (`Complex`, `Rat`, `FatRat` of an `Instance` is `0`) fails the
  cross-check against a correct row. Where the row is the only implementation the cascade declines
  for the receiver (a three-line arm each), and slice 5 deletes the declines with the cascades.
- **`Numeric` of a `Real` object is the object.** `Date.Numeric` is the day count and
  `DateTime.Numeric` an `Instant`, as in Rakudo, and `+$duration` stays a `Duration`. Three places read
  `.Numeric` as a number and now take the `Bridge` of an object they get back: `==` (the bridge
  runs once more, so two `DateTime`s at different offsets compare by instant, roast
  `S32-temporal/DateTime.t`), the argument coercion of a builtin function (`abs($duration)`), and
  `sprintf`'s float directives. Roast found the first two; the third was a regression in
  `Duration.fmt('%.2f')` that the probe script caught.
- **A shape for a lazy value needs a guard that never forces it.** `Value::view()` on a lazy
  `Match` materializes it, so no `Match` handler reads its receiver through it unless it needs the
  attribute map, and `value_type_name` (which the augment gate asks on every call) names a lazy
  match's class from its capture node. A recognition row for a name the table lacked (`Date.mm-dd`,
  `Instant.to-nanos`, ...) is added with its row, as in 3B.

Behaviour changes toward Rakudo, each pinned in a focused test: `Date.Int/Numeric/Real` are the day
count (they were the POSIX timestamp), `DateTime.Numeric/Real` the `Instant`; `DateTime.Int`,
`Date.weekday`, `DateTime.weekday` and `Date.Instant` are no longer answered (Rakudo has none of
them); `Date.mm-dd` and `yyyy-mm` exist; `DateTime.offset-in-minutes` is a `Rat`;
`julian-date` and `modified-julian-date` count from the UTC instant; `Instant.Complex` and
`Duration.Complex` carry the seconds; `Instant.narrow` narrows the seconds; and `Instant.sqrt`,
`exp`, `log`, `floor`, `ceiling`, `round`, `truncate`, `sign`, `cis`, `conj`, `int8` .. `uint64` and
`to-nanos` answer.

**Deferred, with the reason (92 declared rows).**

- **`Date`'s and `DateTime`'s interpreter rows** (`earlier`, `later`, `truncated-to`, `in-timezone`,
  `local`, `clone`, `IO`, the formatter rendering of `Str`/`gist`; 10 rows). They read named
  adverbs (`:2hours`, `:truncate-to`) that also arrive as positional `Pair`s, reblesse the result into
  a subclass and keep the formatter. `Handler::Named` needs the unit names declared; one commit of its
  own with the `Interp` rows. `posix(1)` and the separator argument of the orderings (a non-`Str`)
  stay on the slow path for the same reason.
- **`Instant`'s and `Duration`'s `base` and `polymod`** (4 rows): `base` carries the option machinery of
  `native_base_with_options`, and `polymod` is an interpreter method.
- **`Match`'s remaining rows** (`Int`, `Numeric`, `WHICH`, `not`, `replace-with`, ...; 7 rows) and the
  48 inherited-only names: they delegate to the matched string through `Cool`/`Str`, so they follow
  the opening of the `Match` shape, which needs `Capture`'s rows to answer a `Match` first.
- **The objects group** (`Mu`, `Code`, `Signature`, `Exception`, `Failure`, `Nil`, `Backtrace`,
  `Backtrace::Frame`, `X::AdHoc`, `CX::Warn`, `X::TypeCheck::Assignment`, `Supply`; 75 rows). `Failure`
  must explode for every method it does not declare, `Nil` answers most methods with itself, and
  `Code` carries its signature in a `Sub` value that has no shape: each is a guard of its own, and the
  exception classes are mostly user subclasses (no shape). `Backtrace` and `Backtrace::Frame` are
  the cheapest (class-name shapes, 19 pure rows over `backtrace_methods.rs`); they are claimable as
  `refactor/11276-3d-backtrace`.
- **`RakuAST::*`** (47 rows): the oracle snapshot lists none of these owners, so they wait for the
  snapshot extension (§10.6).

## 10. Slice plan for the remaining migration (amendment 2026-10-06)

This section replaces §6 item 3. It changes how the work is cut, not what is built: §2 and §4
stand, and §9.7 (no per-family performance measurement) stands.

### 10.1 Why one PR per family stopped being workable

- **Pace.** From 2026-10-03 to 2026-10-05 more than twenty PRs landed (slice 1a through §9.14).
  They moved 186 `(owner, name)` pairs, 169 of them rows of the recognition table, which has 1,648
  unique rows: about 8 rows per PR and 11% of the table. 1,479 rows remain, which is about 180
  more PRs at that pace.
- **CI cost follows the PR count, not the diff.** A PR that touches code builds the release binary
  and runs the whole TAP and roast suites: five runner-occupying jobs, about 22 runner-minutes, 14-19
  minutes of wall clock ([ADR-11581](11581-ci-runner-budget.md)). That cost is the same for a
  10-row PR and a 500-row one; only a docs-only PR skips it. 180 PRs are about 4,000 runner-minutes,
  drawn from the 20 concurrent runners that about 100 merges a day already share.
- **Every PR repeats fixed costs**: a claim round-trip, a gate run, an ADR §9 paragraph, a `news/`
  file, and rebases against shared files. Of the 20 commits that touched `method_table/` in those
  three days, 15 edited this ADR's §9, 10 edited `method_table/mod.rs` (the `FAMILIES` list),
  6 edited `method_table/tests.rs` and 7 edited `methods_0arg/collection.rs`, whose arms
  interleave several owners.
- **The rows are not 1,479 independent problems.** By the recognition table's own flags, 1,257 of
  the remaining rows (85%) are served by the pure arity cascades, 179 by the interpreter slow path
  and 43 mutate their receiver. Most of the work is one mechanism applied to many receiver kinds,
  and a mechanism is validated once.

### 10.2 What remains

Counted on 2026-10-06 from `native_method_row_table.rs` minus the pairs that already have a
method-table row. A row is *Pure* when its arity mask has `A0`-`A2` and neither `MUTATES_RECEIVER`
nor `SPECIAL`, *Mut* when it has `MUTATES_RECEIVER`, and *Interp* otherwise (the slow path or a
named interceptor). Those flags were baked on 2026-08-10, so the kind column is a planning
estimate: a slice's inventory commit re-classifies its own arms.

| owner group | owners | rows | Pure | Interp | Mut |
|---|---|---:|---:|---:|---:|
| numbers | Int, Num, Rat, Complex, Bool | 281 | 277 | 4 | 0 |
| text | Str, Cool, Uni, Blob, Version | 193 | 169 | 15 | 9 |
| collections | Any, List, Array, Hash, Map, Range, Seq, Pair, Capture, Junction, Nil, Iterable | 376 | 312 | 32 | 32 |
| quant hashes | Set, SetHash, Bag, BagHash, Mix, MixHash | 181 | 177 | 2 | 2 |
| time | Date, DateTime, Instant, Duration | 123 | 109 | 14 | 0 |
| match | Match | 72 | 72 | 0 | 0 |
| I/O | IO::Path, IO::Handle, IO::Path::Parts | 102 | 15 | 87 | 0 |
| objects | Mu, Code, Backtrace, Exception, Failure, Signature, ... | 104 | 79 | 25 | 0 |
| RakuAST | `RakuAST::*` | 47 | 47 | 0 | 0 |
| **total** | | **1,479** | **1,257** | **179** | **43** |

The cascades hold about 1,260 quoted-name arms over 748 distinct names (`builtins/methods_0arg/`
and `methods_narg/`: ~480, `runtime/methods*.rs`: ~740, the VM's mutation helpers: ~40). 412 of
those names are in no recognition row at all: the `new` dispatch per built-in type, the
metaobject protocol, the subscript protocol, concurrency and distribution classes, internal
`__mutsu_*` names. Those are real built-in methods whose owners the recognition table never
modelled, so they are counted in names, not rows.

### 10.3 The rules that place a PR boundary

1. **A PR is justified by a mechanism or by a deletion unit, never by a family.** A mechanism is
   something CI must validate that the previous PR did not exercise: a handler kind, a receiver
   guard, a resolver change. A deletion unit is a group of owners whose cascade arms all go.
   Moving more rows through a mechanism that already merged adds no new risk class, so those
   rows ride in the slice of their owner group.
2. **Budget: nine PRs for the rest of the campaign** (§10.4). A PR for this campaign that moves
   fewer than 100 rows and introduces no mechanism is not accepted, except a fix-forward of a
   regression. A slice may split only at a commit boundary where the first half is green on its
   own *and* the second half has a different mechanism or a different owner group.
3. **The unit of validation is the receiver kind.** The risk §3 names (a moved arm loses the checks
   that ran earlier in the cascade) is a property of what the receiver is, not of the method. So
   the guard for every built-in receiver kind is built once, in 3A, and the owner-group slices
   contain only handlers and rows.
4. **Build once, publish once.** A slice is one branch of many commits, one commit per family,
   each with its focused test and never squashed (AGENTS.md). Pushing a branch with no PR starts
   no CI (`ci.yml` runs on pull requests, the hourly schedule and dispatch), so the branch is
   pushed after every commit, since the container is disposable. The PR is opened when the
   pre-publication gate passes (`scripts/dev gate`, `--full` where the box allows) and the debug
   cross-check has run over the whole TAP suite. `debug-tap` is post-merge only (ADR-11581), and
   that cross-check is this ADR's soundness net, so a slice dispatches `ci.yml` on its branch
   before merging, or runs `prove t/` on the debug binary itself. One validated push replaces
   many speculative ones.
5. **Disjoint write sets.** Slices that can run in parallel must not edit the same line. Each
   owns its group directory under `method_table/`, its test file and the cascade arms of its
   owners. The shared files named in §10.1 (`method_table/mod.rs`, `method_table/tests.rs`, the
   interleaved cascade functions) are restructured once, in 3A, so that no later slice touches
   them (§10.5). The §9 subsection a slice appends is the one remaining collision, and it is a
   rebase of one paragraph.
6. **A slice ends at zero.** Its exit is a report, not a feeling: no quoted-name arm of its
   owners is left in any cascade, except receivers that have no shape and are named in the
   slice's §9 subsection with the reason (user instances of an `is Array` subclass, for
   example). Those leftovers are what slice 5 deletes. A name that several groups share
   (`elems`, `gist`) keeps its arm, reduced to the other groups' receivers, until the last of
   them moves. A family that cannot be made green inside its slice is deferred whole with its
   reason, never half-migrated, and is picked up by the next slice that owns the same owner or
   by slice 4.
7. **Behaviour changes toward Rakudo are allowed and pinned.** Earlier slices fixed wrong answers
   on the way (the numeric and stringification slices in §9), and a macro-slice will find many
   more. Each one is pinned in the slice's test file and listed in the PR body. A change to a
   currently whitelisted roast result is a regression, not a behaviour change.

### 10.4 The slices

| slice | scope | new mechanism | rows | needs |
|---|---|---|---:|---|
| **3A** | every receiver kind and every handler kind reaches the table (§10.5) | shapes for all built-in value kinds, `Handler::Interp`, named-argument slot, guard flags, type-object flag, group scaffold | proof rows only | - |
| **3B** | numbers and text: Int, Num, Rat, Complex, Bool, Str, Cool, Uni, Blob, Version | - | 465 | 3A |
| **3C** | collections and quant hashes: Any, List, Array, Hash, Map, Range, Seq, Pair, Capture, Set/Bag/Mix and their mutable forms | - | 523 | 3A |
| **3D** | instance classes: Date, DateTime, Instant, Duration, Match, Mu, Code, Backtrace, Exception, Failure, Signature, `RakuAST::*` | - | 346 | 3A |
| **3E** | I/O and concurrency: IO::Path, IO::Handle, IO::Spec, sockets, Proc::Async, Promise, Channel, Supply, schedulers, Lock | oracle snapshot gains owners it lacks | 102 + names outside the table | 3A |
| **3F** | receiver-mutating methods: `push`, `pop`, `shift`, `unshift`, `append`, `prepend`, `splice`, hash and quant-hash mutators, `subst-mutate`, `substr-rw` | `Handler::Mut`, `ReceiverPlace` | 43 + the four mutation paths | 3A |
| **3G** | constructors and the metaobject protocol: `new` per built-in type, `Metamodel::*`, subscript protocol, internal names | owner rows for classes the recognition table lacks | names outside the table | 3A |
| **4** | resolver cutover (§6 item 4) | native candidates invocable in `resolve_sequence` | - | 3B-3G |
| **5** | deletion (§6 item 5) | - | - | 4 |

The 465 and 523 exclude the Mut rows of their owners, which move in 3F; 465 + 523 + 346 + 102 + 43
is the 1,479 of §10.2. The partition is by owner group, not by cascade file, because the arms of
one cascade file belong to many owners (`runtime/methods_dispatch_match*.rs` mixes `Str`, `Mu`
and `Cool` methods). Each slice's first commit is an inventory: the report's list of arms per
owner, which becomes the slice's checklist in §9.

**Order.** 3A first and alone. 3B-3G need only 3A and touch disjoint owners, so they can run in
any order and in parallel, capped by the agent limit (three building agents on the 12-core box,
one in a remote container). 4 follows once the owners it needs have moved, and 5 follows 4. The
critical path is five PR cycles (3A, two waves of the six, 4, 5), not one hundred and eighty.

**Cost.** Nine PRs are about 200 runner-minutes of PR CI. Allowing two re-pushes each for real
failures, under 600, against about 4,000. The Bench deterministic series on `main` is the watch
for a dispatch regression, as in §9.7.

### 10.5 3A in detail

The list is the plan; §9.15 records what was built and where it differs.

3A is the one slice that changes how a call reaches a row, so it is the one that may regress
dispatch for every method. Its deliverables:

1. **Shapes.** `DispatchShape` grows to every built-in value kind that has a remaining Pure or
   Interp row (Bool, Range, Seq, Pair, Capture, Set/Bag/Mix and their mutable forms, Buf/Blob,
   Uni, Version, Date, DateTime, Instant, Duration, Match, Code, ...). One decode, in one place,
   refuses what `try_native_method_raw` refuses today: user subclasses and instances with
   overrides, type objects unless the row is `TYPE_OBJECT_OK`, `Proxy`, mixins, containers, lazy
   and shaped values, itemized hashes. This is §2.4.
2. **`Handler::Interp`**, reachable from the call-site lane and from `call_method_with_values`.
   The pre-dispatch fast lanes of §4 stay where they are and become rows of this kind in the
   slice that owns their receiver.
3. **Arguments.** Named arguments get their own slot instead of arriving as `Pair`s, which
   removes the `plain_args` refusal. Junction autothreading, `use fatal` Failures and lazy-Seq
   reification become flags the guard step applies once. The first commit amends §8's second
   question with the decision.
4. **Type objects.** The row carries `TYPE_OBJECT_OK`, so `Str.gist` and `Int.Str` on a type
   object resolve through the table.
5. **Shared files are edited once.** One module directory per slice group under `method_table/`,
   with `FAMILIES` concatenating the groups (empty ones included), so no later slice edits it.
   `tests.rs` becomes one test module per group, with a sample value for every shape written
   in 3A. Arms still inline in `dispatch_core`, `native_method_0arg_cascade` and the
   1/2-argument cascades move, with no behaviour change, into one sub-dispatcher file per owner
   group, so a group slice deletes whole files instead of editing lines its neighbours also
   edit. `native_method_row_table.rs` is not a conflict point (a migrated pair already has its
   recognition row) and is left alone until slice 5.
6. **`scripts/method-rows-report.py`**: arms left per cascade layer, file and owner group, rows
   per owner and kind. It is a report, not a gate (§9: a shared counter coupled every parallel
   PR and was dropped). If it can be made diff-based, comparing a PR's added lines against its
   merge base so that it couples nothing, a check that a PR adds no new quoted-name cascade arm
   enforces AGENTS.md's slow-path rule; otherwise that rule stays a review rule.
7. **Proof rows**: at least one row per new shape and per handler kind, each covered by the debug
   cross-check and the method-table unit tests.

### 10.6 What stays open, and where it is decided

- `ReceiverPlace`'s shape (§8.1): the first commit of 3F, after surveying the four paths the
  2026-10-04 investigation found (named array, scalar-held array, VM direct mutation, `is Array`
  storage). ADR-0097's binding descriptor is the first candidate.
- How a row declares named arguments (§8.2): settled in slice 3A (§9.15).
- Owners the oracle snapshot (`rakudo_method_tables.txt`, generated by
  `scripts/gen-rakudo-method-tables.raku`) lacks: 3E and 3G extend the snapshot before they add
  rows for those classes.
- Folded owners (§8.3): decided by the first slice that meets one (3B for `Buf`/`Blob`/`utf8`).
- Resolver cutover details (`wrap`, `augment`, `nextsame` on built-ins): slice 4's first commit.

### 10.7 Rejected ways of cutting

- **One PR per family** (§6 item 3 as first written): §10.1.
- **One PR for everything**: about 1,260 arms in one diff repeats the 2026-08-04 handler-ID attempt
  (§7), cannot be bisected by CI, and a single regression holds all of it.
- **By cascade file**: arms of one file belong to many owners, so a file slice needs every owner's
  shape at once, and the rows of one owner are spread over several slices.
- **By handler kind across all owners** (all Pure, then all Interp, then all Mut): the Pure slice
  would be 1,257 rows, which is the previous point, and nothing could run in parallel. Handler
  kinds are a mechanism, so they are slices only where the mechanism is the risk (3A, 3F).

### 10.8 Interim rule

Until 3A merges, no PR migrates a family for this campaign; the permitted work is 3A itself and
fixes to rows already merged. A session that wants to continue the migration takes the next
unchecked slice in the issue body ([#11276](https://github.com/tokuhirom/mutsu/issues/11276)),
claims it with that slice's id in the branch name (`refactor/11276-3b-numbers-text`), and
records progress as one §9 subsection per slice. Live status (which slices are open, merged or
deferred) is kept in the issue body, not here, so this section does not drift.
