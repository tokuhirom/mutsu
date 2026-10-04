# ADR-11276: Built-in methods are handler rows in the one method table

- **Status**: Accepted (user decision 2026-10-03). Slices 1 and 2 done, slice 3 under way; see
  §9. Supersedes
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
3. **Remaining families**, one PR each. Each one lowers the ratchet.
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
  only an arity mask, with the handler raising the error.
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
