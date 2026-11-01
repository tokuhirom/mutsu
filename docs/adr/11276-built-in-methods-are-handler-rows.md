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
  descriptor is the likely answer. **Decided in slice 3F (§9.23):** an env name plus the receiver
  as the call read it (`Var`), or a detached container (`Detached`); ADR-0097's descriptor replaces
  the name lookups under the place, not the handlers.
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

**Families (one commit each, with its focused test).** The by-name table is the checklist:

1. *Identity and rendering*: `gist` (17 owners), `raku` (16), `Str` (15), `WHICH` (14), `clone`
   (8), `perl`, `defined`, `Bool`, `not`, `so`, `self`, `item`, `sink`, `serial`, `Stringy`,
   `WHERE`.
2. *Coercions*: `list` (14), `Capture` (14), `hash` (12), `List` (12), `Array` (11), `Slip`,
   `Supply`, `Pair`, `Numeric`, `Int`.
3. *Size, keys and views*: `elems` (12), `end`, `keys`, `values`, `kv`, `pairs` (11 each),
   `antipairs` (10), `invert`, `kxxv`, `total`, `of`, `default`, `name`, `dynamic`, `is-lazy`.
4. *Subscripts and membership*: `AT-KEY`, `EXISTS-KEY` (10 each), `AT-POS`, `EXISTS-POS` (6),
   `ACCEPTS` (8), `contains`, `index`.
5. *Sampling*: `pick`, `roll` (14 each), `grab`, `grabpairs`, `pickpairs`.
6. *Positional slicing and reduction*: `head`, `tail`, `batch`, `join`, `fmt`, `flat`, `reverse`,
   `sort`, `unique`, `repeated`, `squish`, `cache`, `pairup`, `tree`, `chrs`, `min`, `max`,
   `minmax`, `sum`, `permutations`, `combinations`.
7. *Laziness markers*: `hyper`, `race`, `lazy`, `eager`, `iterator`.
8. *Range specifics*: `bounds`, `in-range`, `infinite`, `int-bounds`, `is-int`, `rand`.
9. *Interpreter rows* (`Handler::Interp`, the closure-calling methods `Any` declares):
   `map`, `grep`, `first`, `reduce`, `produce`, `classify`, `categorize`, `rotor`, `skip`,
   `match`, `iterator`, `eager`, `splice`, `squish`, `categorize-list`, `classify-list`.

**What landed (354 declared rows at the start, 180 left).** Eight family commits, each with its focused
test, each checked against Rakudo and against the roast directories of its owners:

- [x] Range's own methods (`bounds`, `is-int`, `infinite`, `int-bounds`, `rand`, `in-range`): arms
  deleted (no catch-all answers a Range). `is-int` now shares `range_is_int` with `int-bounds` and
  `minmax`, which moves it toward Rakudo (`1..*`, `1..Inf`, `*..5` are not Int ranges; a big-Int
  end and a Bool end are).
- [x] The quant hashes' views and sizes (`collections/quanthash.rs`): `keys`, `values`, `kv`,
  `pairs`, `antipairs`, `total`, `elems`, `default`, `of`, `hash`, `list`, `kxxv`, `invert`,
  `Baggy.Numeric`.
- [x] `AT-KEY`, `EXISTS-KEY` and `ACCEPTS` of the associatives (`collections/subscript.rs`);
  `Capture.AT-KEY` and `EXISTS-KEY` now work (they answered "does not support associative
  indexing").
- [x] Capture (`collections/capture.rs`): the views, `list`, `hash`, `elems`, `Numeric`, `AT-POS`
  (new), `EXISTS-POS` (new), and the `.Capture` coercion of every collection owner.
- [x] Pair's views (`keys`, `values`, `kv`, `pairs`, `antipairs`, `invert`) and `Pair.Pair`;
  `antipairs` answered through the generic positional path (`((:a(5)) => 0,)`) and is the one
  swapped pair now.
- [x] The laziness markers (`collections/lazy.rs`): `hyper`, `race`, `lazy` on List, Map and Range,
  `item` on Map and Range, `Range.is-lazy`.
- [x] Range's element methods (`elems`, `min`, `max`, `minmax`, `Numeric`, `list`, `sum`, `reverse`,
  `contains`, `index`) and the positional subscript (`AT-POS` on Range, `EXISTS-POS` on List and
  Range, `collections/positional.rs`). `List.AT-POS` and `Array.AT-POS` are not rows:
  `builtin_at_pos` answers them with the subscript opcode itself (`@a.AT-POS(-1)` is an
  `X::OutOfRange` failure, a typed array past its end is its type), before the native cascade a
  row would restate; they are an interpreter row's to be.
- [x] The small coercions: `Slip` (List, Array), `List` (Array, Map), `list` (Map), `hash` (Map),
  `default` (Array, Hash).

What the work taught, which the remaining families follow:

- **A name whose cascade arm ends in a catch-all keeps one delegating branch.** `keys`, `values`,
  `kv`, `pairs`, `antipairs`, `hash`, `elems` and `min`/`max` answer for every receiver in the
  end (`_ => value_to_list`, `_ => target.clone()`), so an arm with a covered shape's branch
  deleted answers that shape wrongly and the debug cross-check
  (`debug_assert_matches_full_path`) fails on it, as it should. Those arms call the row's
  handler for the covered shapes (the handler is the one implementation); arms with no
  catch-all (`total`, `default`, `of`, `kxxv`, Range's `bounds` and friends) are deleted
  outright. The delegating branches are what slice 5 removes with the cascades.
- **A one-argument row cannot replace its arm.** A row admits only plain arguments, and a
  cascade arm answers for an object key (an Instance whose `Str` is user code), so
  `AT-KEY`/`EXISTS-KEY`/`ACCEPTS`/`AT-POS` keep a delegating arm.
- **A row that the table answers before a gate in `try_native_method_raw` must apply that gate.**
  `Capture` on a list holding a `Pair` with a non-`Str` key is the interpreter's
  (`try_interpreter_capture`): the row declines through `capture_needs_str_key`, the one helper
  both use. Roast's `S02-types/capture.t` caught it.
- **A name `try_native_method_raw` hands to the interpreter first is not a pure row's.** The
  table answers before that function, so a pure row for `AT-POS` on a List or Array would
  shadow `builtin_at_pos` and answer `Nil` for `-1` where Rakudo fails
  (`t/vm/nqp-list-index-parity.t` pins both). Before registering a row, read
  `try_native_method_raw` and `call_method_with_values` for the name.
- **A one-argument row keeps its arms for the recognition tests**: `native_method_row`'s
  `*_rows_are_backed_by_the_cascade` tests ask the 1- and 2-argument cascades whether they
  recognize each recognition row, and only the 0-argument entry consults the table, so
  `Range.in-range` kept delegating arms.
- **A row must be reachable by some shape** (`every_row_is_reached_and_answers`): `Map.AT-KEY`
  has no row (no shape would reach it behind `Hash.AT-KEY`), and no `Seq` row can be registered
  before the `Seq` shape exists.
- **Rows that restate an ancestor's implementation can contradict an earlier slice's pin**:
  `List.sum` is declared by Rakudo, but `aggregate_rows_resolve_to_the_rakudo_owners` pins
  `Any.sum` for List, so the row was not added.

**Deferred, with the reason (the 180 declared rows that are left).**

- **`Seq`** (26 rows): the `Seq` shape is the one ADR §9.15 leaves to 3C, and it is the riskiest:
  a method on a `Seq` decides whether it consumes the `Seq`, and the call goes through
  `reify_or_consume_seq_target` first. It wants its own change (the shape, a consumption flag on
  the row, and the 26 rows), not a tail on this one.
- **The rendering and identity names** (`gist`, `raku`, `Str`, `WHICH`, `fmt`, `clone`: about 65
  rows). They are the names every group shares: one `match` over every receiver kind implements
  each (`dispatch_core_repr`, the `Str` arm of `dispatch_core_coerce`, the `WHICH` arm), behind a
  prologue with a per-name condition for instances, mixins and type objects. A row per owner
  over that one function would only register metadata, and splitting the function per shape
  edits the lines 3B and 3D must edit as well, so it is done once, after those two slices, as a
  split of each function into `gist_of`/`raku_of`/`str_of`/`which_of`/`fmt_of`.
- **Sampling** (`pick`, `roll`, `pickpairs`, `grab`, `grabpairs`: 34 rows). A random answer
  cannot be re-run by the debug cross-check, so it needs a `RowFlags::RANDOM` the cross-check
  honours; the one-argument forms take `*`, a count or a closure and reach the interpreter's
  `methods_pick_roll.rs`; and the quant hashes' `grab` mutates the receiver. Arity 0 alone would
  be a half-migrated name, which §10.3 rule 6 forbids.
- **Mutating methods** (19 declared rows flagged `MUTATES_RECEIVER`, 20 real mutators): 3F, with
  `Handler::Mut`.
- **Interpreter rows on `Any`** (`map`, `grep`, `first`, `reduce`, `produce`, `classify`,
  `categorize`, `rotor`, `skip`, `squish`, `eager`, `iterator`, `match`, `splice`,
  `categorize-list`, `classify-list`: 18 rows). Each calls a closure through the interpreter's
  own routes (`vm_native_map`, `methods_collection.rs`); a row has to join those routes, which is
  slice 4's resolver work.
- **The rest, by name**: `head`/`tail` (the arity-0 forms read the raw store and have their own arm),
  `flat` on Map and Range, `join` and `batch` on `Any`, `chrs` and `Supply` on List, `dynamic` and
  `name` on Array and Hash (the interpreter's container methods), `Any.list`/`Any.hash`/`Any.serial`
  and `Bool` on Junction, and `gist`/`raku` on `Nil` and `Junction` (no shape).
- **Opening the closed shapes.** `Range`, `Pair`, `Capture` and the six quant hashes stay closed:
  an ancestor row (`Any.head`, `Cool.uc`, ...) does not reach them. Opening one means reading every
  ancestor row for that shape's receivers; a `Range` inherits `Cool`'s string rows, which answer
  from the stringified receiver, and `.chars` of a `Range` is not obviously `"1..3".chars`. Each
  shape is opened by the change that audits it.

The report (`scripts/method-rows-report.py --inventory collections,"quant hashes"`) lists the 180;
it is the checklist for whoever takes the deferred families.

### 9.17 Slice 3B: numbers and text (2026-10-06)

Branch `refactor/11276-3b-numbers-text`. Owners: `Int`, `Num`, `Rat`, `FatRat`, `Complex`, `Bool`
(the report's *numbers* group) and `Str`, `Cool`, `Uni`, `Blob`, `Buf`, `Version` (*text*). This is
the first commit's inventory, taken with `scripts/method-rows-report.py --inventory numbers,text`;
later commits tick families off and the closing paragraph records what was deferred.

**Inventory (unregistered recognition rows, 2026-10-06).** 467 recognition rows over 163 method
names, of which only **354 can be registered**: as in 3C, a row's owner must be the type Rakudo
declares the method on (`rows_are_declared_by_rakudo`), and the other 113 (`Bool.isNaN`,
`Int.uc`, `Blob.push`, ...) are *inherited-only*, served by the ancestor's row once the receiver's
shape inherits it. The issue's "465 rows" is this 467 less two; the registrable count is the
354. `FatRat` has no declared row of its own (Rakudo composes its methods from `Rational`, which
the rows already register on `Rat`), and `Blob`/`Buf` have none because the oracle snapshot
(`rakudo_method_tables.txt`) does not list those two owners yet: their rows come with the
snapshot extension (the last family below).

| owner | declared | Pure | Interp | Mut | inherited-only |
|---|---:|---:|---:|---:|---:|
| Int | 68 | 67 | 1 | 0 | 17 |
| Num | 49 | 48 | 1 | 0 | 2 |
| Rat | 49 | 48 | 1 | 0 | 2 |
| Complex | 43 | 38 | 5 | 0 | 6 |
| Bool | 10 | 10 | 0 | 0 | 32 |
| Str | 38 | 31 | 6 | 1 | 16 |
| Cool | 75 | 69 | 6 | 0 | 6 |
| Uni | 15 | 15 | 0 | 0 | 0 |
| Version | 7 | 7 | 0 | 0 | 0 |
| FatRat, Blob, Buf | 0 | 0 | 0 | 0 | 32 |
| **total** | **354** | **333** | **20** | **1** | **113** |

`Str.subst-mutate` and `Str.substr-rw` write the receiver and move in 3F.

**Families (one commit each, with its focused test).** The by-name table
(`--inventory`'s second half) is the checklist; the counts are declared rows:

1. *Transcendental math* (159): `sin`, `cos`, `tan`, `sec`, `cosec`, `cotan` and their hyperbolic,
   inverse and inverse-hyperbolic forms, `atan2`, `exp`, `log`, `log2`, `log10`, `sqrt`, `expmod`,
   `cis`, `unpolar`, `polar`, `roots`, on `Int`, `Num`, `Rat`, `Complex` and the `Cool` that
   numifies a `Str`.
2. *Integer and real numerics* (60): `is-prime`, `narrow`, `conj`, `rand`, `base`,
   `base-repeating`, `polymod`, `Bridge`, `lsb`, `msb`, `chr`, `byte`, `int`..`uint64`,
   `abs`/`sign`/`floor`/`ceiling`/`round`/`truncate` on `Cool`, `succ`/`pred`/`pick`/`roll` on
   `Bool`.
3. *Coercions* (45): `Numeric`, `Real`, `Int`, `Num`, `Rat`, `FatRat`, `Complex`, `UInt`,
   `Capture`, `Version`, `Stringy`, `Str` on the scalars.
4. *Rendering and identity* (22): `gist`, `raku`, `WHICH`. These are the names §9.16 left for "the
   slice that owns the arm first": one `match` over every receiver kind
   (`dispatch_core_repr`, the `Str` arm of `dispatch_core_coerce`, the `WHICH` arm).
5. *Unicode* (35): `NFC`, `NFD`, `NFKC`, `NFKD`, `encode`, `uniname`, `uniprop`, `unival`,
   `unimatch`, `uniprops`, `univals`, `uninames`, `uniparse`, `parse-names`.
6. *`Uni`* (5): `elems`, `list`, `codes`, `AT-POS`, `EXISTS-POS`, and the audit that opens the
   `Uni` shape to its ancestors' rows.
7. *Text with arguments* (15): `indent`, `samecase`, `samemark`, `split`, `substr-eq`,
   `parse-base`, `naive-word-wrapper`, `fmt`, `ACCEPTS`, the `Str.Date` and `Str.DateTime`
   coercions.
8. *Interpreter rows* (13): `match`, `subst`, `trans`, `sprintf`, `printf`, `indices`, `IO`,
   `subst-mutate`, `substr-rw` (the last two are 3F's).
9. *`Blob` and `Buf`*: the two shapes, the snapshot extension for both owners, and their
   rows.

The audit of the ancestor rows for `Bool`, `Uni`, `Version` and `Blob`/`Buf` (a closed shape is
opened by the slice that owns it, §9.15) is part of each shape's family.

**What landed (354 declared rows at the start, 102 left).** 284 rows are registered (392 -> 676:
252 of the declared recognition rows, the rest being argument forms of a name and rows the
recognition table lacked, such as `Cool.acosec`). Five commits after the inventory, each
with its focused test (`t/oo/method/*-method-rows.t`), each checked against Rakudo, the roast
directories of its owners and the unit suite (`cargo test --lib method_table native_method_row`):

- [x] *Transcendental math* (`scalars/math.rs`, 159 rows): one handler per method for `Int`,
  `Num`, `Rat`, `Complex` and `Cool`; `numify` (`scalars/numify.rs`) is the `Cool` coercion all
  the `Cool` numeric rows share. `complex_math.rs` moved next to the rows.
- [x] *Integer and real numerics* (`scalars/real_misc.rs`, 43 rows): `is-prime`, `narrow`,
  `conj`, `Bridge`, `lsb`, `msb`, `chr`, `rand` and the native integer coercions.
  `RowFlags::RANDOM` (the debug cross-checks skip a row whose answer is random) is the one new
  mechanism; 3C's sampling family needs it too.
- [x] *`Cool`'s `abs`/`sign`/`floor`/`ceiling`/`truncate`/`round`* and `round($scale)`, and
  `Bool.succ`/`pred` (`scalars/cool_real.rs`, 9 rows). The `Cool` numeric wrapper of the
  0-argument cascade, the 1-argument one and `cool_aggregate.rs` are gone.
- [x] *Unicode* (`scalars/unicode.rs`, 32 rows): the `uni*` methods, `parse-names` and the
  normalization forms; the rows the recognition table flags `TYPE_OBJECT_OK` answer a built-in
  type object as the cascade did.
- [x] *`Uni`* (`scalars/uni.rs`, 10 rows): `elems`, `codes`, `Int`, `Numeric`, `Str`, `list`,
  `gist`, `raku`, `AT-POS`, `EXISTS-POS`.

The `Bool` shape is **open** (`DispatchShape::inherits`): its MRO is `Bool`, `Int`, `Cool`, `Any`,
`Mu`, so every registered `Int`, `Cool` and `Any` row now answers a `Bool`. The audit ran each of
those rows against `True` and `False` through a probe script (`tmp/` only: 290 method calls,
mutsu against Rakudo, before and after) and fixed what broke: the numeric handlers
(`real::abs_of`, `coerce::int_of`, `Rounding::of`, `cool_real::round_to`, ...) read a `Bool` as the
`Int` enum it is (`True.abs` is 1, `True.floor` is `True`), and `Any.min`/`max` take it as a
scalar. 83 answers moved to Rakudo's and none regressed.

What the work taught, which the remaining families follow:

- **A row reaches more receivers than its owner names.** A `Cool` row is reached by `Str`,
  `List`, `Array`, `Hash` and `FatRat` (its shape has no row of its own), so a handler for
  `Cool.sin` has to answer all of them; `every_row_is_reached_and_answers` fails on the first
  shape it cannot. `numify` is the one place that knows how a `Cool` becomes a number.
- **A `Cool` row's shapeless receivers keep a delegating arm.** `Match`, `Range`, `Seq` and
  instances of `Cool` subclasses have no shape yet, so the table never sees them; the arms for the
  Unicode names, `sqrt` (`Seq`), `Bridge`/`rand`/`narrow`/`abs` (`Instant`, `Duration`) and
  `round($scale)` (allomorphs) stay, calling the rows' handlers. Slice 3D, which adds the shapes,
  deletes them. Deleting such an arm outright regresses `$match.uniname` and `(1..3).uniname`.
- **The debug cross-check wants the cascade to agree, not only to exist.** A cascade arm that
  answers a shaped receiver differently from its row (`Uni.elems` fell to the scalar catch-all and
  said 1) is a failure; either the cascade declines or its arm calls the row's handler.
- **Rows with arguments no longer need cascade arms for the recognition tests.**
  `native_method_arities`, the probe behind the `*_rows_are_backed_by_the_cascade` tests, now
  asks the table for arities 1 and 2 (3C kept delegating arms for them: its lesson 4).
- **A row exists only where Rakudo declares the method.** `expmod` is `Int`'s alone (three
  recognition rows for `Num`, `Rat` and `Complex` were wrong and are gone), and `narrow`, `Bridge`,
  `lsb` and `msb` are not `Cool`'s, so a `Str` has no such method (`"5".lsb` is "No such method").
- **The type-object concreteness list stays a list of names.** `check_numeric_type_object_method`
  answers `Int.roots`, `Int.expmod`, `Int.is-prime` and `Int.chr` for the recognition tests'
  type-object probe; a `TYPE_OBJECT_OK` row cannot replace it, because type objects of `Cool`,
  `Any` and user classes are not shapes.
- **Roast outranks the oracle where they disagree.** The first CI run failed
  `S15-string-types/NF-types.t` and `NFK-types.t`: `NFC.chars` is the codepoint count there, and the
  `Uni` block's `chars` arm (which current Rakudo does not have) was deleted with the others. It is
  back, calling `uni::elems`; the probe script and the local roast runs covered `S15-unicode-information`
  but not `S15-string-types`, so run every roast directory of an owner, not the nearest one.
- **`Uni` stays closed.** Rakudo's `Uni` is `Any` and `Mu`, not `Cool`, and the `Any` rows assert
  `scalar_like`; opening it means teaching those handlers a codepoint array, which is the
  collections' business (3C's deferred "opening the closed shapes"), so it reaches only the rows
  it owns.

**Deferred, with the reason (the 102 declared rows that are left).**

- **Coercions** (45 rows: `Numeric`, `Real`, `Int`, `Num`, `UInt`, `Rat`, `FatRat`, `Complex`,
  `Str`, `Stringy`, `Version`, `Capture`). Each cascade arm mixes the shaped branches with a
  type-object warning, `Instant`/`Duration`, `Range`, `Buf` and `StrDistance` instances, and the
  `Complex` branches that read `$*TOLERANCE` (`Interpreter::dispatch_complex_to_real`). The `Complex`
  receiver needs `Handler::Interp` rows (`Complex.Int`, `Num`, `Rat`, `FatRat`, `Real`, and
  `Cool.UInt`), and without them `every_row_is_reached_and_answers` cannot pass for the `Cool` rows.
  Registering the rows over the existing arms would add metadata and delete nothing; it is one
  commit of its own with the interpreter rows.
- **The rendering and identity names** (`gist`, `raku`, `WHICH`: 22 rows): §9.16's deferral stands;
  the shared `match` is split once, after 3D.
- **Text methods with arguments** (`indent`, `samecase`, `samemark`, `split`, `substr-eq`,
  `parse-base`, `naive-word-wrapper`, `fmt`, `ACCEPTS` on `Version`, `Str.Date`, `Str.DateTime`:
  about 15 rows). Their arms already call one shared function and gate `IO::Path`, `Supply` and
  `IO::Spec` receivers by hand; a row would delete none of them.
- **Interpreter rows** (`match`, `subst`, `trans`, `sprintf`, `printf`, `indices`, `IO`, `encode`:
  about 13 rows) and `base`, `base-repeating`, `polymod` (7): `base` carries the `:`-option
  machinery of `native_base_with_options`, `polymod` is an interpreter method. `subst-mutate` and
  `substr-rw` are 3F's.
- **`Bool`'s own rows** (`Int`, `Numeric`, `Real`, `Str`, `gist`, `raku`, `ACCEPTS`, `pick`, `roll`):
  the first six ride with the coercions and rendering families; `pick`/`roll` are Rakudo's
  `Bool:U` methods and ride with the sampling family (3C's remainder), which can use `RANDOM` now.
- **`Blob` and `Buf`**: the two shapes (instances of the built-in classes, like `Date`), the
  oracle snapshot extension (`rakudo_method_tables.txt` has neither owner) and the `read-*`,
  `subbuf`, `bytes`, `decode` rows, with the `Buf`/`Blob`/`utf8` folding decision (§8.3).
  Its own change: a new shape and snapshot owners are a different risk from the pure handlers here.
- **`Version`'s rows** (7): `Str`, `gist`, `raku`, `WHICH` are the rendering names, `ACCEPTS`
  and `Version` coerce through the interpreter's smartmatch; the shape stays closed.

Findings filed: [#12088](https://github.com/tokuhirom/mutsu/issues/12088) (`Cool` numeric methods
on a `Range` or `Seq`), [#12089](https://github.com/tokuhirom/mutsu/issues/12089) (`Bool`'s
Enumeration methods), [#12090](https://github.com/tokuhirom/mutsu/issues/12090) (`Int.exp($base)`
and `0.asech`), [#12091](https://github.com/tokuhirom/mutsu/issues/12091) (the native integer
coercions of a `List`).

### 9.18 Slice 3D: instance classes (2026-10-06)

Branch `refactor/11276-3d-instance-classes`. Owners: `Date`, `DateTime`, `Instant`, `Duration` (the
report's *time* group), `Match`, and the *objects* group (`Mu`, `Code`, `Backtrace`,
`Backtrace::Frame`, `Exception`, `Failure`, `Signature`, `X::AdHoc`, `X::TypeCheck::Assignment`,
`CX::Warn`, ...). This is the first commit's inventory, taken with
`scripts/method-rows-report.py --inventory time,match,objects`; later commits tick families off and
the closing paragraph records what was deferred.

**Inventory (unregistered recognition rows, 2026-10-06; 676 rows registered).** 209 declared rows
over 161 method names, plus 82 *inherited-only* rows (a pair Rakudo does not declare on the owner,
so it is served by the ancestor's row once the receiver's shape inherits it). The plan's "346"
counted the inherited-only rows and the 47 `RakuAST::*` rows, which the oracle snapshot
(`rakudo_method_tables.txt`) does not list as owners, so they wait for the snapshot extension
(§10.6).

| owner | declared | Pure | Interp | inherited-only |
|---|---:|---:|---:|---:|
| Date | 29 | 25 | 4 | 1 |
| DateTime | 41 | 33 | 8 | 0 |
| Instant | 24 | 23 | 1 | 0 |
| Duration | 20 | 19 | 1 | 0 |
| Match | 24 | 24 | 0 | 48 |
| Mu | 21 | 10 | 11 | 0 |
| Code | 11 | 3 | 8 | 3 |
| Backtrace | 11 | 11 | 0 | 2 |
| Backtrace::Frame | 8 | 8 | 0 | 0 |
| Exception | 6 | 6 | 0 | 2 |
| Failure | 3 | 3 | 0 | 5 |
| Signature | 6 | 2 | 4 | 2 |
| X::AdHoc, X::TypeCheck::Assignment, CX::Warn, Supply | 5 | 5 | 0 | 17 |
| **total** | **209** | **148** | **37** | **82** |

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
- [x] *`Instant` and `Duration`* (`instances/instant.rs`, 51 rows): two new instance-class shapes.
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
  `DateTime.Numeric` an `Instant`, as in Rakudo, and `+$duration` stays a `Duration`. Four places read
  `.Numeric` or `.Real` as a number and now take the seconds or the `Bridge` of an object they get
  back: `==` (the bridge runs once more, so two `DateTime`s at different offsets compare by
  instant, roast `S32-temporal/DateTime.t`), the argument coercion of a builtin function
  (`abs($duration)`), `sprintf`'s float directives, and the slow path of `polymod` on an `Instant`
  or `Duration`. Roast found the first two, the probe script the third, and the whole debug TAP run
  (`t/vm/codegen/adr0051-catalog-ancestry-consumers.t`) the fourth.
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

### 9.19 Slice 3E: I/O and concurrency (2026-10-06)

Branch `refactor/11276-3e-io-concurrency`. Owners: `IO::Path` and `IO::Spec::*` (this slice), with
`IO::Handle`, `IO::CatHandle`, `IO::Pipe`, `IO::Special`, the sockets, `Proc::Async`, `Promise`, `Channel`,
`Supply`, the schedulers and `Lock` deferred (below). The inventory was taken with
`scripts/method-rows-report.py --inventory IO::Path,IO::Handle,...`: 81 declared recognition rows over
77 method names for the owners the oracle snapshot lists (44 on `IO::Path`, 37 on `IO::Handle`), and
none for `IO::Spec::*`, `Promise`, `Channel`, `Lock` and the rest, which the snapshot did not list at
all. The plan's "102 rows" counted `IO::Path` and `IO::Handle` together with the inherited-only
names; most of what the cascades do for these classes is not in the builtins cascades but in
`runtime/native_io/` and in a 560-line `IO::Spec` block of `call_method_with_values`, which the
report's arm counts do not see.

**What landed (821 -> 943 rows registered).** Two owner groups, each deleted whole:

- [x] *`IO::Path`* (75 rows in `io_concurrency/io_path_*.rs`): the lexical methods (19 rows:
  `Str`, `gist`, `IO`, `SPEC`, `basename`, `dirname`, `volume`, `cleanup`, `parts`, `parent`,
  `sibling`, `add`, `extension`, `is-absolute`, `is-relative`, `succ`, `pred`), the cwd methods (6:
  `absolute`, `relative`, `CWD`, `raku`), the 19 `stat` readers and file tests, the content rows (9:
  `slurp`, `lines`, `words`, `comb`, `open`), the filesystem rows (17: `spurt`, `mkdir`, `rmdir`,
  `unlink`, `chmod`, `chown`, `copy`, `rename`, `move`, `symlink`, `link`) and `child`, `resolve`,
  `dir`, `watch`, `Numeric` (5). The shape `IoPath` covers `IO::Path` and its four SPEC variants
  and is **closed**: a handler reads the receiver's class and `SPEC` attribute and hands the class
  back, so `IO::Path::Win32.new('x').parent` is an `IO::Path::Win32`. The `try_io_path_*` name gates
  and the six blocks that called them from `vm_call_method_compiled_{mut,interpret}.rs` are gone;
  `native_io_path` is `invoke_owner` plus `Cool`'s `Real`/`Int`/`Rat`/`Num`/`FatRat`, which are
  `Cool`'s rows and wait for the opening of the shape (3B remainder).
- [x] *`IO::Spec::Unix`, `Win32`, `Cygwin`, `QNX`* (47 rows in `io_spec.rs`, the oracle snapshot now
  lists the four owners): one handler per method reads the receiver's class (`SpecKind`), and every
  class Rakudo says declares the method has a row for it. The bodies were the arms of the block in
  `call_method_with_values`, moved verbatim into `runtime/native_io/io_spec_{paths,split}.rs`; the
  block is replaced by an `invoke_owner` call for the receivers the table declined.

**Mechanisms.**

- **`RowFlags::SLURPY`.** A `*@parts` method has a minimum arity, and the table registers the row at
  every arity from there to 7 (the call-site lookup's bound), so `IO::Path.add` is one row, not five.
  A call with more arguments reaches the row through the owner lookup. `MethodRow::arities` is the
  range.
- **`invoke_owner(interp, owners, method, args, target)`.** A row is found by `(owner, method,
  arity)` along a list of owners, for a receiver that has no shape: an instance of a user subclass of
  `IO::Path` (the instance dispatch walks its MRO to `IO::Path` and `native_io_path` asks for that
  owner), and a call the guard step declined (a named argument no row declares, an argument it does
  not admit). The arguments are not admitted: this is the slow path, which accepted any argument
  before the row existed. The target value is built only after the row is found.
- **Type-object-only shapes, and `DispatchShape::reaches`.** `IoSpecUnix`, `IoSpecWin32`,
  `IoSpecCygwin` and `IoSpecQnx` have no instances (`has_instances`): the cascades answer a name like
  `join` for an instance by stringifying it, and nothing reads an `IO::Spec` instance. A closed shape
  used to reach only its own type's rows; `reaches(owner)` names the ancestors it audited, which for
  the `IO::Spec` family is `IO::Spec::Unix` (and never `Any` or `Mu`).
- **Interpreter rows with named arguments.** An `IO::Path` row that needs the interpreter
  (`Handler::Interp`) reads its named arguments through `Named::pairs`, because the interpreter's
  primitives (`parse_io_flags_values`, `io_path_spurt`, ...) take one argument list with the `Pair`s
  in it. Each primitive is now one `Interpreter` method, called by the row and by the sub form of
  the same routine (`lines($path)`, `words($path)`).

**What the work taught.**

- **The cascades' by-name arms answer any receiver.** `IO::Spec::Unix.join()` with no argument is a
  `List.join` of the type object in the cascades (`(IO::Spec::Unix)`), so a row for that arity fails
  the debug cross-check. An `IO::Spec` row therefore takes the positionals Rakudo's signature
  requires and any number more, and a call with fewer is no longer answered with a lenient guess.
  A row that makes a new object on every call (`curupdir`) cannot be cross-checked either, and is an
  interpreter row.
- **A metadata list is a second dispatch.** `IO::Path`'s `native_methods` in `runtime_init.rs` listed
  `starts-with`, because the lexical funnel had an arm for it; it is `Cool`'s method, and with the arm
  gone the entry made the call fail. The entry is removed. The list goes with slice 5.
- **`Mu.perl` is `self.raku`.** `IO::Path` had a `"raku" | "perl"` arm; `perl` is not a method
  `IO::Path` declares, so `native_io_path` maps it to the `raku` row.

Behaviour changes toward Rakudo, each pinned in a focused test (`t/io/io-path-lexical-rows.t`,
`io-path-cwd-stat-rows.t`, `io-path-content-fs-rows.t`, `io-spec-method-rows.t`): an `IO::Spec`
method called with fewer positionals than its signature requires fails instead of answering a guess;
`IO::Path.starts-with` is `Cool`'s. Both are in the news entry. One regression is filed, not fixed:
`Cool` string methods on a user *subclass* of `IO::Path` read the instance's rendering rather than
the path ([#12149](https://github.com/tokuhirom/mutsu/issues/12149); `starts-with` joined `uc` and
`chars` there).

**Deferred, with the reason (37 declared `IO::Handle` rows, and the owners the snapshot lacks).**

- **`IO::Handle`** (37 rows, `runtime/native_io/io_handle.rs` and four VM fast paths in
  `vm_call_method_compiled_io.rs`). A handle's methods exist three times: the interpreter's
  `native_io_handle`, the VM's per-method fast paths (File and UTF-8 only, each declining the rest to
  the interpreter) and the user-subclass overlay that routes `print`/`say`/`get`/... through a user
  `WRITE`/`READ`. One row per method means one implementation of those three, and the choice of
  which to keep decides the shape of `Handler::Interp` for stateful receivers; it is a slice of its
  own.
- **`IO::CatHandle`, `IO::Pipe`, `IO::Special`, the sockets, `Proc::Async`, `Promise`, `Channel`,
  `Supply`, the schedulers, `Lock`, `Semaphore`, `Thread`**: the oracle snapshot lists none of these
  owners (it is generated from the owners the recognition table names), so each needs recognition
  rows first; their methods live in `runtime/native_methods/*.rs`, one `match` per class, with
  receivers that carry live OS state.

### 9.20 Slice 3E, part 2: `IO::Handle` (2026-10-06)

Branch `refactor/11276-3e-io-handle`. Owner: `IO::Handle`, the first of the slice's remainder (§9.19). 51 rows
registered (943 -> 994): the state methods (`DESTROY`, `path`, `IO`, `Str`, `gist`, `raku`, `nl-in`, `nl-out`,
`chomp`, `out-buffer`, `encoding`, `opened`, `t`, `tell`, `eof`, `seek`, `lock`, `unlock`, `flush`,
`close`, `native-descriptor`, ), the reads (`get`, `getc`, `readchars`, `lines`, `words`,
`read`, `slurp`, `slurp-rest`, `split`, `comb`, `Supply`), the writes (`print`, `put`, `say`, `printf`,
`print-nl`, `write`, `spurt`) and `open`. Of the 39 declared rows the inventory listed, 37 are
registered; `READ` and `WRITE` are Rakudo's stubs for a subclass to override and no cascade ever had an
arm for them.

**The decision the §9.19 deferral asked for.** A handle's methods existed three times: the interpreter's
`native_io_handle`, the VM's four per-method fast paths (`try_native_io_handle_{method,output,
byte_output,read}`, File and UTF-8 targets only, each declining the rest) and the user-subclass overlay
(`try_user_io_handle_method`, which routes `print`/`get`/... through a user `WRITE`/`READ`).

- **The interpreter's implementation is the one kept.** The ~800-line `match` of `native_io_handle` is
  one `Interpreter::io_handle_<name>` method per row (`runtime/native_io/io_handle_{state,read,write,
  open}.rs`), and `native_io_handle` itself is `invoke_owner` over `IO::Handle`'s rows. The sites that
  call it with attributes (`IO::CatHandle`, `IO::Path.spurt`, the subclass `open`) are unchanged.
- **The four VM fast paths are deleted**, with their ten call sites and the `IoHandleState` helpers
  only they used (`native_text_write`, `slurp_string_native`, `read_line_native`, ...). Each answered
  from handle state the interpreter's methods read through the same `IoHandleState` methods, so there
  was nothing to move. The env-pure branch of `try_env_pure_mut_dispatch` (the vendored `Test`
  module's `$output.say`) calls the `print`/`put`/`say`/`printf`/`print-nl` rows through
  `invoke_owner`; it keeps its guard and gains the built-in `.wrap` check below. What they did for a
  File target, the row does for every target: Stdout and Stderr no longer fall through to a second
  implementation.
- **The user-subclass overlay stays.** It is not a built-in method: it is Raku's own contract that an
  `IO::Handle` subclass with `WRITE`/`READ` has its text methods built on them, and its receivers have
  no handle in the table. It runs first, as before.

**Shape and handler.** `IoHandle` is a closed shape for the class `IO::Handle` itself. `IO::Socket::INET`,
`IO::Pipe` and user subclasses have other class names and reach the rows through their owner
(`invoke_owner`). Every row is `Handler::Interp` over `Interpreter::io_handle_*`, which take the receiver
and one argument list (positional first, the named `Pair`s after), so a body is the arm it was cut
from. Rows that take an argument are `ANY_ARGS`; `print`/`put`/`say`/`printf`/`split`/`comb` are
`SLURPY` from arity 0.

**What the work taught.**

- **`open` writes back over the receiver, and a shape lookup runs the handler on a copy.** `$fh.open`
  answers `self` with the opened handle's state; only the mutating dispatch entries
  (`native_io_handle_mut`) write the result back. The lane and `try_native_method` found the row
  first, and the receiver stayed closed. `RowFlags::OWNER_ONLY` registers a row for the owner lookup
  only; slice 3F's `Handler::Mut` retires it.
- **The "augmented or wrapped?" gate saw `Any` for an instance.** `native_lever_a_user_override_sym`
  keys on `value_type_name`, which is `Any` for every instance, so a `.wrap` of a built-in instance
  method (`$*OUT.^find_method('print').wrap`, `t/io/builtin-method-wrap-io-handle-print.t`) was invisible to
  it and the new row answered first. It now looks up the wrap chain by the instance's class (one bool
  while nothing is wrapped). An `augment` of an instance class is still keyed on `Any`: the same gate
  is wrong for it, and no test reaches it yet.
- **A fast path that declined a target hid the other implementation's bugs.** The VM paths decoded
  `slurp` through the shared NFC decoder, the interpreter arm through a strict UTF-8 one; the
  interpreter arm now uses the shared decoder (`t/io/io-handle-read-one-decoder-parity.t` caught it).
  A wrapper subclass's `nl-out` getter (`IO::MiddleMan`) was also answered by a fast path before it
  reached the interpreter; the row answers it when the receiver has no handle of its own.

Behaviour changes: none that a script can see beyond what `t/io/io-handle-method-rows.t` pins (the `.wrap`
and subclass cases above keep their old answers). Found, not fixed: `words(N)` on a handle that was
read with `get` leaves its last word buffered across a `seek(0)`, so the next `words` starts with a
stale word ([#12186](https://github.com/tokuhirom/mutsu/issues/12186)).

**Deferred.** `IO::CatHandle`, `IO::Pipe`, `IO::Special`, the sockets, `Proc::Async`, `Promise`, `Channel`,
`Supply`, the schedulers, `Lock`, `Semaphore` and `Thread` (§9.19: the snapshot lists none of the owners,
so each needs recognition rows first).

### 9.21 Slice 3E, part 3: `IO::Handle`'s `READ` and `WRITE` (2026-10-07)

Branch `refactor/11276-3e-io-handle-stubs`. Two rows registered (994 -> 996). §9.20 called `READ` and `WRITE`
"stubs for a subclass to override"; that was wrong for a handle that has a real file behind it. In Rakudo
they are the primitives the handle's own methods are built on: `READ(Int:D $bytes)` reads up to that many
bytes and answers a `Buf`, `WRITE(Blob:D $buf)` writes the raw bytes and answers `True`, and both bind their
argument strictly (`X::TypeCheck::Binding::Parameter`). Each is one `Interpreter` method
(`io_handle_read_primitive`, `io_handle_write_primitive`) beside the `read` and `write` ones, sharing the
byte-level handle helpers; unlike `.read`, a `READ` with no count is a bind failure, not "the rest".
The user-subclass overlay is unchanged: it still runs first and a user `WRITE`/`READ` still wins.

One more cascade copy went: `class_introspection.rs`'s hand-written list of `IO::Handle` method names
(which answered "is this a native method?" and had no `WRITE`/`READ`) now asks the table for the owner's
rows, so the list and the table cannot drift. Not done: a bare `class H is IO::Handle {}` has no handle and
Rakudo's `H.new.READ(1)` dies with an `X::AdHoc` about `MVMOSHandle`; mutsu answers `X::Method::NotFound`
because only the exact class `IO::Handle` is asked, as before.

### 9.22 Slice 3E, part 4: `IO::Special`, `Semaphore`, `Thread` (2026-10-07)

Branch `refactor/11276-3e-concurrency-primitives`. The first owners the oracle snapshot lacked: the three
classes gain recognition rows (`native_method_row_table.rs`) and snapshot lines
(`raku scripts/gen-rakudo-method-tables.raku`), then 29 rows are registered (996 -> 1025): `IO::Special`'s
17 declared methods plus `Mu.gist`, `Semaphore`'s `acquire`/`try_acquire`/`release`, and `Thread`'s `id`,
`Numeric`, `name`, `is-initial-thread`, `app_lifetime`, `Str`, `gist` and `finish`. None of the three has a
shape (an `IO::Special` is an instance of one class with a `what` attribute, a `Semaphore` and a `Thread`
carry an id into runtime tables), so every row is `RowFlags::OWNER_ONLY` and `native_io_special`,
`native_semaphore` and `native_thread` are `invoke_owner` plus the names the owner does not declare
(`new`, `Bool`, `defined`, `WHAT`, which are the constructor and `Mu`'s). `Semaphore`'s three methods are
one `Interpreter` method each (`semaphore_*_method`), and the hand-written name lists of `IO::Special`,
`Thread` and `IO::Handle` in `class_introspection.rs` ask the table (`owner_declares_row`).

Behaviour change toward Rakudo, pinned in `t/concurrency/thread-lock/semaphore-thread-method-rows.t` and
`t/io/io-special-method-rows.t`: `IO::Special.gist` is `Mu.gist`, the `raku` form, instead of the stream's
name; `Thread.gist` is `Immortal Thread #id (name)` (no `Immortal ` for an `app_lifetime` thread, no name
for an anonymous one) instead of `Str`'s `Thread<id>(name)`.

Not done: `Thread.finish` answers `True` where Rakudo answers the thread; `Thread`'s `join`/`run`/`start`/
`usage`/`yield` and the three constructors stay on their own paths (3G for `new`). The rest of the 3E
remainder (`IO::CatHandle`, `IO::Pipe`, the sockets, `Proc::Async`, `Promise`, `Channel`, `Supply`, the
schedulers, `Lock`) is unchanged: `IO::Pipe` and `IO::CatHandle` mix modes in one `match` (a pipe's
behaviour depends on whether it holds a child's stdin or stdout), and `Lock` is reached from six VM and
runtime blocks that name its methods.

### 9.23 Slice 3F: receiver-mutating methods (2026-10-07)

Branch `refactor/11276-3f-mutating`. This subsection records the survey and the decision of §8.1
first (the slice's first commit); what landed is appended below it as each family merges.

**What a mutator writes through.** The survey of the rows the recognition table flags
`MUTATES_RECEIVER` (`scripts/method-rows-report.py --inventory List,Array,Hash,BagHash,Str`) and of
every cascade site that names them found four kinds of receiver, not one:

1. **A shared node, no name needed.** `SetHash`/`BagHash`/`MixHash` `grab`/`grabpairs`/`set`/`unset`
   and `BagHash.add`/`remove` (`builtins/quanthash_mutators.rs`, `vm/vm_baghash_mutators.rs`) already
   write through the container's `Gc` node in place (container identity, §3: a mutable quant hash is a
   reference type), so a call needs only the value. They have three call sites each (`CallMethodMut`,
   `CallMethod`, the baggy-subclass delegate) and no state of their own.
2. **A named binding.** `@a.push`, `%h.push`, `$r.splice` where `$r` holds an array, `@!items.pop`.
   The array and hash mutators write the node in place too, but they key a second set of facts on the
   *name*: the declared element type (`var_type_constraint`) and `is default`, the thread-shared store
   (`push_to_shared_var`, `shared_array_extend`, the atomic lanes in `exec_call_method_mut_op_impl`), a
   compunit's unit-lexical cell and an `our` package array (`env_root_descended_mut`), a `:=`-bound cell,
   the native-integer element wrap, and a detached-rebuild fallback that writes a fresh container back
   under the name when the name does not resolve to one. `Str`'s `subst-mutate` and `substr-rw` replace
   the value and so must write it back under the name.
3. **The backing storage of an `is Array` instance** (`__mutsu_array_storage`): a plain untyped array
   with no name, mutated by `native_array_storage_mut` and written back into the instance.
4. **A by-value receiver** (`f().push(1)`, `[1,2].pop`, an element): `array_mutate_copy`, which writes
   the shared node in place (the node has no other holder when the receiver is a literal).

The same six array mutators are written **six times**: the `@`-sigil arm and the sigil-less arm of
`call_method_mut_with_values` (which differ in how they type-check and in the native-integer wrap),
`try_native_array_mut` and `try_native_array_splice` (the VM's fast path in front of that function),
`native_array_storage_mut`, `array_mutate_copy`, and the thread-shared lanes of the VM. `Hash`'s
`push`/`append` is written three times (the `%` arm, `try_native_hash_mut_bound`, the by-value arm).
Their answers differ where the copies drifted (an empty `pop` on a by-value array answers a plain
`Failure`, on a named native array one that names `array[num]`).

**Decision, §8.1: `ReceiverPlace` is an env name plus the receiver as the call read it, or a detached
container.**

```rust
pub(crate) enum ReceiverPlace<'a> {
    /// The call named a binding: `@a.push`, `$r.splice`, `%h.push`, `@!items.pop`.
    Var { name: &'a str, value: &'a Value },
    /// A container with no binding: an `is Array` instance's storage, a by-value receiver.
    Detached(&'a mut Value),
}

pub(crate) type MutFn = fn(&mut Interpreter, &mut ReceiverPlace<'_>, &[Value], Named<'_>)
    -> Option<Result<Value, RuntimeError>>;
```

- *Why not ADR-0097's binding descriptor.* It is `Proposed` and unbuilt, and the facts the mutators
  read about a binding (declared type, `is default`, the shared store, unit-lexical and `our` cells,
  a `:=` cell) are looked up **by name** by the interpreter's env helpers today. The place exposes
  them through accessors (`place.slot(interp)` is `env_root_descended_mut(name)`;
  `place.check_element_types(interp, values)` is the name-keyed check for a `Var` and the
  metadata-only check for a `Detached`), so the handlers are written once against the place. When the
  descriptor lands, those accessors change and the handlers do not.
- *Why the place is small.* Container identity (§3) already moved the array, hash and quant-hash
  writes to the shared node. What is left for the place is the residue: the name-keyed facts above,
  the cell descent, assigning a replacement value (`place.assign`: a `Str`'s `subst-mutate`, an
  `IO::Handle`'s `open`, which retires `RowFlags::OWNER_ONLY`) and the detached fallback.
- *The guard step does not change for pure rows.* A `Handler::Mut` row is registered in the owner
  map only (never in the per-shape map), so no shape lookup, call-site lane or `try_dispatch` finds
  it, and the debug cross-check never re-runs it (a second run would apply the mutation twice). It
  is reached by one new entry, `invoke_mut(interp, place, method, args)`, which looks the row up
  along the owner chain of the receiver's value kind (`Array` then `List`, `Hash` then `Map`, ...),
  splits the named arguments as `invoke_owner` does, and runs the handler. A handler returns `None`
  to decline and the call takes the cascades exactly as before, so a family migrates whole or not at
  all.
- *Where the entry is called.* The mutating dispatch entry (`call_method_mut_with_values`) at the
  position of the arms it replaces, so every receiver guard in front of it (immutable list, user
  override, augment, Proxy, tied container, `ContainerRef` cell) keeps its order; the VM's fast
  paths and thread-shared lanes at their own positions with their own preconditions; the by-value
  entry and the `is Array` storage path with a `Detached` place.

**Families, one commit each, in order** (a family that cannot be made green is deferred whole):
the quant-hash mutators (a shared node: the mechanism's proof), `Str.subst-mutate` and `substr-rw`
(`assign`), `Hash.push`/`append`, `Array`'s `push`/`append`/`unshift`/`prepend`/`pop`/`shift` with
`List`'s immutable rows, `Array.splice`, `Blob`/`Buf`'s mutators, and `IO::Handle.open`.

**What landed (1025 -> 1059 registered rows, 34 of them `Mut`; the cascades' quoted-name arms 1161 -> 1125, the VM's mutation helpers' 41 -> 26).** One commit per family, each with its
focused test and each checked against Rakudo and the roast files of its owners:

- [x] *The mechanism, and `BagHash.add`/`remove`* (`mutating/baghash.rs`). `Handler::Mut`,
  `ReceiverPlace`, `invoke_mut`, `RowFlags::ANY_NAMED`. A `Mut` row is registered in the owner map
  and in a per-name arity mask of its own (`Table::mut_arities`), never in the per-shape map, so
  `try_dispatch`, the call-site lane and the pure entries cannot find it and the debug cross-check
  cannot run it twice. `invoke_mut` is the guard step of the mutating entries: a bit test on the
  name and arity first; the owner chain of the receiver's value kind (`mutating::owners_of`); the
  `augment`/`.wrap` veto (`native_lever_a_user_override_sym`) before the handler, which has effects;
  named arguments the row does not declare dropped (a method's implicit `*%_`, ADR-0070), or all
  of them handed over for an `ANY_NAMED` row. No argument is admitted or refused: a mutator reads
  its arguments raw (`@a.push(@b)`).
- [x] *The QuantHash mutators* (`mutating/quanthash.rs`): `SetHash.set`/`unset`, `grab` and
  `grabpairs` on `SetHash`/`BagHash`/`MixHash`, and the `X::Immutable` rows of `Set`/`Bag`/`Mix`.
  `builtins/quanthash_mutators.rs` and the cascades' `grab` arms (`dispatch_1arg.rs`,
  `dispatch_core_range.rs`, the Bag and Mix halves of `dispatch_grab_method` and all of
  `dispatch_grabpairs_method`) are gone. The Callable count (`grab(* div 2)`) is resolved by the
  handler, which has the interpreter.
- [x] *`Str.subst-mutate` and `substr-rw`* (`mutating/text.rs`): the first rows that need the name.
  `place.assign` writes both halves of the VM's dual store; `substr-rw` hands back the write-through
  `Proxy`. A receiver with no name has no `Str` row (`owners_of` is asked with `has_name`).
- [x] *`Hash.push`/`append`* (`mutating/hash.rs`): the `%` arm of the by-name entry, the by-value
  block and `try_native_hash_mut_bound` were three copies of one body. The typed and object-hash
  path reads the declared key and value types off the container first and the variable second, and
  `%_` names a hash with no variable in the type error.
- [x] *The `Array` mutators* (`mutating/array.rs`): `push`, `append`, `unshift`, `prepend`, `pop`,
  `shift`, `splice` and `grab` on `Array`, and `List`'s six refusals (`X::Immutable`; a `List` reaches
  `Array.splice` through the owner chain and gets Rakudo's "no candidates"). The six copies named
  above are one implementation, and `try_native_array_mut`, `try_native_array_splice`,
  `array_mutate_copy`, `array_grab`, the sigil-less arm, the immutable-list reject and the E2
  shift/pop block are deleted. `native_array_storage_mut` is now the `Detached` call of the same
  rows, so the `is Array` storage, the mixin-wrapped array and the `nextsame` fallback share them.

**Where the entry is called.** Four places, each with the position the arms it replaced had, and
none after a guard the arms ran in front of: the VM's `CallMethodMut` (an early hook in front of the
scoped-env flatten for the plain `Array` receiver, #9494's lane, and one at the old native fast-path
position for every other kind); `CallMethod` (a `Detached` place, probed by receiver kind first so
a call on any other receiver pays one tag probe); the interpreter's by-name entry
(`call_method_mut_with_values`, a `Var` place with no chunk) and its by-value dispatcher
(`call_method_with_values_inner`, `Detached`).

**What the work taught.**

- **The receiver is settled by the time a family's old arm ran, not at the top.** The VM's
  `CallMethodMut` reifies a lazy array, vivifies an undefined receiver and unwraps `.VAR` between its
  first line and its native fast paths, so the hook for the array rows sits where the arms sat.
- **A fast path is a guard in front of the same body.** `try_native_array_mut` bailed for typed,
  `is default`, shared and non-`@` receivers and for `pop`/`shift` with an argument; the row's
  handler does each of those itself, so the guard in front of the VM's early hook shrinks to "an
  array, no Junction argument, not a thread-shared name".
- **The name-keyed constraint is only an element type for an `@` name.** `check_container_element_types`
  looked up `var_type_constraint(name)` for any name; for `my Int $x = [1]` that is the variable's
  type, not the elements'. The row asks it only of an `@` name and reads the container's metadata
  otherwise (the scalar-held arm already did).
- **A user subclass of a QuantHash reaches the storage through the delegate's own list**
  (`is_baggy_storage_method`); `set` and `unset` were missing from it.

Behaviour changes toward Rakudo, each pinned in a focused test (`t/collections/set-bag-mix/
baghash-mutator-rows.t`, `quanthash-mutator-rows.t`, `t/types/string/str-mutator-rows.t`,
`t/collections/hash/hash-push-append-rows.t`, `t/collections/array/array-mutator-rows.t`):
`MixHash.grab` is refused; `set`/`unset` on a `SetHash` subclass work; an undeclared named
argument is ignored by every mutator; a `Nil` pushed onto a scalar-held array decays to the element
default; a by-value `QuantHash` is mutated in place.

**Not done, and why.**

- **`Blob`/`Buf`'s seven mutators** (`buf_mutate_method`, `buf_pop_shift_splice`, `buf_reallocate`
  and the by-value block). Rakudo declares them on `Buf`, not on `Blob`, and the recognition table
  folds `Buf`/`Blob`/`utf8` into one owner: §8.3's question, which 3B's remainder (the `Blob`/`Buf`
  shapes) decides. The rows follow it.
- **`IO::Handle.open`** (`RowFlags::OWNER_ONLY` stays for it). Its write-back replaces the
  receiver's *attributes* in the instance's shared cell through
  `call_native_instance_method_mut_in_place`, whose `AttrPublisher` is the cross-thread publish point
  of #7923. A `Mut` row for it needs a third place kind (the attribute cell with its publisher), which
  no array, hash or string needed.
- **The VM's thread-shared lanes** (`shared_array_extend`, `shared_array_mutate` in front of the
  dispatch for a plain `@name` once a thread exists) and the `ArrayPush` opcode keep their own
  copies: they route through the name-keyed atomic store, a `Shared` place that this slice does not
  add. The thread lanes are the remaining quoted-name arms of the array owners
  (`vm_call_method_mut_ops.rs`); they and the opcode's body are slice 5's, with the cascades.
- **Receivers with no row**: a `Range` (the immutable-`Range` arm in the by-value block, owned by
  3C's `Range`), a `Seq`/`LazyList` (`vm_lazy_front_mutate`), an `Iterator` or `IterationBuffer`
  instance, a `Proxy` subclass's attribute array (`proxy_subclass_array_mutate`) and an `Any:U`
  (the autovivification arms), none of which has a shape.

### 9.70 The numeric conversions, `base` and `polymod` of the numbers (2026-10-11)

`refactor/12675-numeric-coercion-rows` (issue #12675). Measured with `method-rows-report.py --inventory` on every group not yet measured:
`numbers` still owed 33 declared rows, `text` 40 (spread over fifteen names), `time` 2, `match` 0, `quant hashes` 9. The numbers' conversion matrix was the
largest block with one obvious shape, so it went first: 43 rows in four new modules of `method_table/scalars/`.

- **`numeric_coerce`** (`Numeric`, `Real`, `Rat`, `FatRat`, `Complex` on `Int`, `Num`, `Rat`; `Numeric` and `Complex` on `Complex`; `Rat` and `FatRat`
  also with an `epsilon`). Each conversion is one `*_of` function that the row and the cascade arm both call; the arms keep `Str`, the aggregates and the
  type objects. The `epsilon` check (`Real $epsilon`, the `X::TypeCheck::Binding::Parameter` of #12043) moved here from `dispatch_1arg.rs`.
- **`numeric_base`** (`base` with one and two arguments on `Int`, `Num`, `Rat`; `base-repeating` with zero and one on `Rat`). `base_of` replaces the
  three copies in `dispatch_1arg.rs` / `dispatch_2arg.rs`; the `Instant`/`Duration` instance branches read their `value` attribute and call it too.
- **`numeric_polymod`** (`Int`, `Num`, `Rat`, `Instant`, `Duration`): an interpreter row, because a lazy divisor list is pulled. `Interpreter::polymod_answer`
  is shared with the `polymod` interceptor, which keeps receivers with no shape (a mixin, a subclass) and still declines a receiver whose ancestry
  declares no `polymod` (`Str`, `Complex`).
- **`complex_real`** (`Complex.Int`, `Num`, `Rat`, `FatRat`, `Real`, interpreter rows): `$*TOLERANCE` decides whether the imaginary part is negligible;
  the body is `Interpreter::dispatch_complex_to_real`, which the `Str`-that-numifies-to-a-`Complex` path of `call_method_with_values` calls too.
- **Behaviour changes toward Rakudo**: a big Int is no longer clamped by `.Rat` (it was `Rat 9223372036854775807.0`), `.Rat(eps)`/`.FatRat(eps)` of a big Int,
  big Rat or `Bool` answers the number instead of `0`, `Bool.Complex` is `1+0i`, `Bool.base` answers, `.base` of a big Int takes a digits count, a `NaN` or
  infinite `Num` fails with `Cannot convert NaN to Int` in every arity (the two-argument form answered the string `NaN`), a bad radix says
  `base argument to base out of range. Is: 37, should be in 2..36`, and `Rat.base-repeating` takes no argument.
- **The arm count did not fall** (368 to 369): `Numeric`, `Real`, `Rat`, `FatRat`, `Complex` and `base` are also answered, through the same functions, for `Str`,
  `Cool`, `Mu`, the aggregates, `FatRat`'s `Bool` and the type objects, none of which has a row yet. They go with their owners' rows.
- **Left** (each needs the owner, not the number): `Capture` on `Int`/`Num`/`Str`/`Version` (the answer differs per receiver and the messages differ from
  Rakudo's), `Bool.pick`/`Bool.roll` (declared on the type object `Bool:U` only, and a type object has no shape), `Mu.Numeric`/`Mu.Int` (user overrides),
  `Cool.Int`/`Num`/`Rat`/`UInt` and `Str.Numeric`/`Int`/`Num` (the `Str` arms carry the complex-string, empty-string and radix-prefix cases).

### 9.71 `Str` and `Cool` conversions, `Version`, `Capture` of scalars, `Mu.Numeric`/`Mu.Int`, `Bool.pick`/`Bool.roll` (2026-10-11)

`refactor/12678-cool-str-conversion-rows` (issue #12678, the remainder of §9.70). 16 of the 17 declared rows `--inventory` listed are rows now; the seventeenth, `Cool.Int`, is unreachable (below).

- **`str_numeric`**: `Str.Numeric`, `Str.Int`, `Str.Num` and `Cool.Num`, `Cool.Rat`, `Cool.UInt` (`Cool.UInt` is an interpreter row: it is reached by `Complex`, whose `UInt` is `$*TOLERANCE`-dependent and not declared by Rakudo on `Complex` itself). `numeric_of_str`, `int_of_str`, `num_of_str`,
  `rat_of_str`, `fat_rat_of_str` and `uint_of_int` are the one implementation: the rows call them, and so do the cascade arms (an allomorph, a subclass). The
  `Str` cases the numeric types do not have are kept: an empty string is zero, a string that numifies to a `Complex` (`"1+2i".Int`) is declined to the interpreter
  (`$*TOLERANCE`), a non-number is the lazy `X::Str::Numeric` `Failure`. A `Cool` row numifies through `numify` (a `List`, `Array`, `Hash` is its element count).
  `Str.Rat` now converts the *parsed number*, so `"0x1F".Rat` is `31.0`, `"NaN".Rat` is `0/0` and `":16<ff>".Rat` is `255.0` (the old text-to-Rat scanner, `str_to_rat`,
  answered `0.0`); the scanner is gone. `List.UInt`, `Hash.Num` and the aggregates' `Version` answer instead of `X::Method::NotFound`.
- **`Cool.Int` stays out**: every shape that inherits `Cool` declares an `Int` of its own (`List`, `Map`, `Str`, `Uni`, `Date`, `Match`, the numerics), so no shape would reach a `Cool.Int` row and `every_row_is_reached_and_answers` forbids it, as it forbade `Map` rows in §9.66. `Cool.UInt` calls the same `Int` routines. The inventory will keep listing it.
- **`Cool.Version` / `Str.Version`** (`version::version_of`): `Version.new(self.Str)`, for every `Cool`.
- **`Capture` of `Int`, `Num`, `Str`, `Version`** (`capture_none`): `X::Cannot::Capture` naming the value as Rakudo does (``Cannot unpack or Capture `42`.`` plus the two
  hint lines), `.what` the value. The cascade's `value_to_capture` raises the same error for those receivers and `Bool`/`*`; `Rat`, `Complex` and the aggregates
  were already right (the issue text said otherwise).
- **`Mu.Numeric` / `Mu.Int`** are `DEFERRAL_BASE` rows (`mu_base`): `callsame`/`nextsame` from a user `method Numeric`/`Int` ended at nothing (`Any`) and now ends at
  `Interpreter::mu_numeric_coercion` -- the same function the slow-path arm calls for a call on a class with no `Numeric` of its own (`X::Multi::NoMatch` for an
  instance, the uninitialized-value warning and `0` for a type object). A class with a number behind it (`is Int`) still defers to `Int`'s answer.
- **`Bool.pick` / `Bool.roll`** are `TYPE_OBJECT_OK` rows: `Bool.pick` samples `(False, True)`; a defined `True.pick` is `X::Multi::NoMatch`, as in Rakudo (it answered before).
- **Not done**: `Real`, `FatRat` and `Complex` of `Str`/`Cool` (Rakudo does not declare them there; the table serves them through `Cool`), `Complex` of the aggregates
  (`(1,2,3).Complex` is `3+0i` in Rakudo, `0+0i` here: #12688), and `Range.Version`/`Seq.UInt` (closed shapes; they need the `COOL_NUMERIC_FOR_COLLECTIONS` audit).

### 9.72 `Cool.Complex`; `Range` and `Seq` reach `Cool`'s `Complex`, `UInt` and `Version` (2026-10-11)

`fix/12688-aggregate-complex-version` (issue #12688, found by §9.71).

- **`Cool.Complex` is a `Handler::Narrow` row** (`str_numeric::complex_of_cool`): `self.Numeric.Complex`, so a `List`, `Array`, `Hash` or `Map` is its element
  count (`(1, 2, 3).Complex` is `3+0i`; it was `0+0i`) and a string parses as before. Rakudo declares `Complex` on `Cool` (`Cool.^method_table`), which the
  recognition table had missed; it gains the `("Cool", "Complex")` row. The cascade's `Complex` arm calls the same function for receivers with no shape and keeps
  `0+0i` for a type object.
- **`Complex`, `UInt` and `Version` join `COOL_NUMERIC_FOR_COLLECTIONS`**, so the closed `Range` and `Seq` shapes reach those three `Cool` rows:
  `(1..5).Version` is `v1.2.3.4.5`, `(1, 2, 3).Seq.UInt` is `3`. `Version` reads the string form, not `numify`; the audit is per method either way.
- **`Range.Numeric` of a fractional endpoint.** `range_elems_f64` counted `1..^5.5` as `5.5 - 1 + 1 - 1 = 4.5` elements (so `(1..^5.5).sqrt` was `2`); it now counts
  whole steps from the first element, as `.elems` does.
- **Left**: a string `Range` (`("a".."c").Complex` is `3+0i` in Rakudo, `0+0i` here: `numify` gives no count for it), and the uninitialized-value warning Rakudo
  emits for `List.Complex` on a type object.

### 9.73 The quant hashes' `Str` and the mutable ones' `clone` (2026-10-11)

`refactor/11276-quanthash-str-clone-rows`. The `quant hashes` group of `--inventory` (§9.70 counted 9 declared rows owed); after this it owes none.

- **`Str` on `Set`, `SetHash`, `Bag`, `BagHash`, `Mix`, `MixHash`** is a `Handler::Narrow` row (`render::quant_str`) over `Value::to_string_value`, the renderer
  the cascade's `Str` arm and prefix `~` use. It declines an element that may carry its own `Str`/`gist` (`has_object_element`, as `gist`/`raku` do).
- **A `Bag` or `Mix` renders an element by its `gist`, a `Set` by its `Str`** (Rakudo's `Baggy.Str` vs `Setty.Str`): `bag(a => 1).Str` is `a => 1` and
  `bag($[1, 2]).Str` is `[1 2]`; they were `a\t1` and `1 2`. Behaviour change toward Rakudo, in the one renderer.
- **`clone` on `SetHash`, `BagHash`, `MixHash`** is a `Handler::Narrow` row (`clone::quant_clone`) over `container_clone`, which gains the three quant-hash arms
  the cascade carried; the cascade arm now calls it. A pure row: a `SetHash[Int]`'s type parameter lives in the copied data, not in the interpreter's
  container registry. An immutable `Set`/`Bag`/`Mix` has no `clone` of its own in Rakudo (`Mu.clone`) and keeps the cascade's answer.
- **Found, not fixed**: an element whose class declares `Str`/`gist` is rendered by mutsu's default form in `.Str`, `~` and `.gist` (#12702), and a `Mix`
  weight that is a `Rat` is stored and rendered as an `f64` (`(a => 1/3).Mix.Str` is `a(0.3333333333333333)`, Rakudo `a(0.333333)`; #12703).

### 9.74 The text methods with an argument: `samecase`, `samemark`, `indent`, `encode`, `parse-base`, `substr-eq`, `naive-word-wrapper`, `Date`, `DateTime`, `Stringy` (2026-10-11)

`refactor/11276-text-pure-rows`. The pure half of the `text` group's declared rows: `--inventory text` went from 30 declared rows owed to 16, and every one left
is an interpreter row (`subst`, `trans`, `match`, `indices`, `IO`, `sprintf`, `printf`), `split`/`fmt` (named arguments and several arities), or `Cool.Int`
(unreachable, §9.71).

- **`method_table/scalars/str_args.rs`**: `samecase`, `samemark`, `indent`, `encode` on `Str` and `Cool` (one handler each, reading the receiver through
  `with_str`); `parse-base`, `substr-eq` (arities 1 and 2), `naive-word-wrapper` (a `Handler::Named` row binding `:max`/`:indent`), `Date`, `DateTime` and
  `Stringy` on `Str`; `encode` on `Uni`. The cascade arms call the same handlers.
- **`indent` keeps its warning**: removing more columns than a line has returns the `warn_signal_with_resume` error the arm returned, carrying the result.
- **Behaviour changes toward Rakudo**: `"abcd".substr-eq("bc", 10)` is `False` (it was an `X::OutOfRange` `Failure`; the interpreter's `:i`/`:m` path agrees now),
  `"11".parse-base("2")` and `.parse-base(2.0)` are `X::Multi::NoMatch` listing Rakudo's three candidates (they were `X::Method::NotFound`), and `Uni.encode`
  answers a `Buf` (it was `utf8`).
- **Left**: `"a".indent("x")` answers `"a"` (Rakudo: `X::Str::Numeric`, `indent` coerces its argument with `Int()`); a negative `substr-eq` position says
  `Index out of range` where Rakudo says `Position in calling 'substr-eq' out of range` (`RuntimeError::out_of_range_failure` ignores its `what`);
  `parse-base`'s `"camel"`/`"beer"` easter-egg candidates.

### 9.75 The text interpreter rows: `match`, `subst`, `trans`, `indices`, `IO`, `sprintf`, `printf` (2026-10-11)

`refactor/11276-text-interp-rows`. The interpreter half of the `text` group: after it and §9.74, `--inventory text` owes only `split` and `fmt` (named
arguments and several arities each) and the unreachable `Cool.Int` (§9.71).

- **`method_table/scalars/str_interp.rs`**: `Handler::Interp` rows for `match` on `Str` (as `Any.match`, §9.60); `subst` (arities 1 and 2), `trans`
  (slurpy), `indices` (arities 1 and 2) and `IO` (`:CWD`/`:SPEC`) on `Str` and `Cool`; `sprintf` and `printf` (slurpy) on `Cool`. `match`, `subst`, `trans`
  and `indices` bind any positional argument (a `Regex`, a positional `Pair`, a closure) and every named one (`RowFlags::ANY_NAMED`): the adverbs are the
  pattern's.
- **One routine each.** The by-name arms of `dispatch_method_by_name_1` for `subst`, `IO`, `sprintf` and `printf` became `Interpreter::dispatch_subst_row`,
  `dispatch_io_coerce`, `dispatch_sprintf_row` and `dispatch_printf_row` (`runtime/methods_text_rows.rs`); the arms and the rows call them, and the
  rows call the existing `dispatch_match_row`, `dispatch_trans` and `dispatch_indices`.
- **`subst` keeps the VM's native substitution.** A row runs ahead of the VM's "lever A" block, so the row calls `try_native_subst` first (a `Str`
  receiver, a plain pattern, a literal replacement, at most `:g`) and the interpreter's `dispatch_subst_row` otherwise: the same order a call that misses
  the table takes.
- No behaviour change was intended; a `raku` differential probe over these names on `Str`, `Int` and `List` receivers matched before and after, except two
  answers that were already wrong and are filed: a match inside a block called by another routine does not set the outer `$/` (#12720), and
  `sprintf("%d", "x")` answers `0` (#12721).

### 9.76 `split` on `Str`, `Cool` and `IO::Path`; `Cool.fmt` (2026-10-11)

`refactor/11276-split-fmt-rows`. The last declared rows of the `text` group (`--inventory text` now owes only the unreachable `Cool.Int`, §9.71) and
`IO::Path.split`.

- **`split` on `Str` and `Cool`** (arities 1 and 2) is a `Handler::Interp` row (`str_interp::split`) over `Interpreter::dispatch_split_row`: a regex
  splitter installed with its closure scope, then `handle_split_method`. The generic `split` handler of `call_method_with_values` calls the same routine.
  The pure cascade's `native_split_method` now reads its limit with `parse_split_limit` too (it truncated `to_f64`, so `*`, `Inf` and a `NaN` limit
  differed from the interpreter's), and the interpreter's string splitter is the pure one (`split::split_by_string`, which matches only at grapheme
  boundaries); its own copy, `split_by_string_static`, searched by codepoint and is gone.
- **`IO::Path.split` splits the file's contents** (Rakudo: `self.open(:$chomp, :$enc, :$nl-in).split(|c, :close)`); mutsu split the path string
  (`"x.txt".IO.split(",")` was `("x.txt",)`). The row (`io_path_content::split_row`) and the interpreter's handler call `Interpreter::io_path_split`
  (slurp with `:enc`, then `dispatch_split_row`); the pure cascades decline every `IO::Path` instead of only a `%?RESOURCES` entry.
- **`Cool.fmt`** (arities 0 and 1) is a `Handler::Narrow` row over `fmt_native`, the routine the collections' `fmt` rows and the arity cascades call.
- **Left**: `IO::Path::Parts`' `AT-KEY`/`AT-POS` (no dispatch shape), Rakudo's `IO::Path.split($s, 1)` answering a `List` (a quirk; mutsu answers a
  `Seq` as for every other limit).

### 9.77 `callsame`/`nextsame` into the `Any` interpreter rows; their call-site cost (2026-10-11)

`test/12388-callsame-into-any-rows` (closes #12388). The last item §9.60 left on #12388 needed no code: since slice 4's resolver work (#12387), a
user override that defers reaches the built-in row. `t/oo/method/any-interp-callsame-method-rows.t` pins `callsame` from an `is Array` subclass's
`map`, `grep`, `first`, `reduce`, `produce`, `rotor`, `skip`, `squish`, `classify`, `categorize`, `match` and `eager`, and `nextsame` from `map`,
against Rakudo's answers.

- **Call-site cost** (callgrind, `--profile profiling`, second run): `$l.map(* + 1)` on a scalar-held three-element `List`, 20,000 calls, is
  2,223 Ir per call in the row's handler (`any_interp::map`), of which 1,636 is `dispatch_map_method` and 587 the row wrapper
  (`deferred_to_interpreter` 129, `joined` 160, the decontainerization); the whole iteration, closure calls and `.elems` included, is ~34,900 Ir.
  `@a.grep(* > 1)` reaches its row through `call_method_with_values` at 2,074 Ir per call in the handler (1,644 in `dispatch_grep_method`), plus
  ~520 Ir of table lookup and guard. `@a.map(...)` on an `@` variable never reaches the row: it takes the VM's rw-writeback lane
  (`pull_map_grep_rest`), as §9.60 required.
- **Rakudo difference noticed, not a row matter**: `augment class Any { multi method map(|c) { ... } }` is never called in Rakudo for a `List`
  receiver (the built-in candidates are narrower than `|c`); mutsu calls the user candidate first. That is multi ranking, filed as #12751.

### 9.78 Slice 5 finding: a shapeless receiver reaches `Cool`/`Str` text methods only through the cascade (2026-10-11)

`refactor/11276-retire-text-arms` tried the first slice-5 deletion: the arms of `methods_0arg/dispatch_core_str.rs` (`trim`, `trim-leading`,
`trim-trailing`, `flip`, `codes`, `words`, `comb`, `naive-word-wrapper`, `Date`, `DateTime`, `fmt`), each of which only re-calls a row handler.
It was reverted: no arm can go yet.

- **Why.** The arms are the *only* path for a receiver with no `DispatchShape`. `class MyStr is Str { }; MyStr.new(value => "abc").flip` (pinned
  by `t/oo/method/str-text-method-rows.t`) answers `X::Method::NotFound` once the `flip` arm is gone, although `Str.flip` and `Cool.flip` are rows,
  because the table is keyed by shape and `invoke_owner` is called only for the receivers that name their owners (`IO::Spec`, RakuAST, the
  concurrency types). `uc`/`lc`/`tc`/`chars`/`ord` look as if they work for such an instance only because their arms live in other files
  (`dispatch_core_math.rs`, `dispatch_core_coerce.rs`).
- **What else the attempt showed** (all fixable once the above is settled):
  1. `IO::Path` (a closed shape) needs an audited `Cool` text list for `trim`, `trim-leading`, `trim-trailing`, `flip` and `codes`
     (`roast/S32-str/trim.t` calls `.IO.trim`), the way `Range`/`Seq` have `COOL_NUMERIC_FOR_COLLECTIONS`.
  2. The consistency tests `e11_slice2_owner_rows_are_backed_by_the_cascade` and `type_object_ok_rows_are_backed_by_a_real_type_object_probe` ask
     the cascade whether a row's arity/type-object claim is real; `native_method_arities` asks the table only for arities 1 and 2, and a type
     object's admission is decided by `cool_type_object_gate`, not by a row flag. Both need a table-first probe (arity 0, owner rows) before any
     arm is removed; they are slice 5's "E2 inverse probes" anyway.
- **What slice 5 needs first.** A *native-base resolution* for a shapeless receiver: an `Instance`/mixin whose class derives from a native type
  (`is Str`, `is Int`, `is List`...) is answered by the rows of that base's shape, with the receiver's backing value as the invocant, and a
  plain user class keeps `X::Method::NotFound`. Today each family of arms re-implements "stringify the instance" on its own. With that in
  place the arms of all eight families in `dispatch_core_families` can be deleted together, not one file at a time.

### 9.79 Slice 5: the native-base and shapeless-`Cool` resolution; the `Cool` text arms go (2026-10-12)

`refactor/12836-native-base-resolution` (closes #12836). The resolution §9.78 asked for is a step of the guard step (`method_table::dispatch`),
taken when a receiver has no shape:

1. **Native base.** An instance of a user class deriving from a built-in scalar type (`class MyStr is Str`, `is Int`, `is Num`, `is Rat`, a
   coercion-boxed `MyStr("x")`) is answered by the rows of its payload's shape, the payload as invocant (`builtins::native_base::redirect`;
   the module is the former `numeric_subclass`, now covering `Str` payloads, `value::numeric_payload::native_base_payload` reading the five
   reserved attributes). The methods about the instance itself (`WHAT`, `raku`, `gist`, `Str`, `clone`, ...) and the coercers that answer
   `self` (`MyInt.new(5).Int`) are not redirected. A plain user class has no payload and keeps `X::Method::NotFound`.
2. **Shapeless `Cool`.** An enum value, a `Seq`/`Slip`/`LazyList` without a settled shape, and an instance whose MRO reaches `Cool` before
   any built-in class that declares the method reach `Cool`'s own row, themselves as invocant (`method_table::answer_shapeless`). This step
   runs *after* the cascades and answers only the audited text names, because a receiver-specific answer in the cascade or the slow path
   comes first: an enum's `sign`, and a `class NotComplex is Cool` whose own `Numeric` the trig methods must call (`roast/S32-trig`,
   `roast/S32-num/cool-num.t`; a first cut that resolved every `Cool` row in front of the cascade broke both). An instance is asked of the
   interpreter's MRO (the VM's `try_native_method_raw` and `call_method_with_values` pass it), so the pure entry declines it;
   `class MyPath is IO::Path` keeps reaching `IO::Path.lines` by its owner, and a `Failure` is never answered.
3. **Closed shapes.** `Range`, `Seq` and `IO::Path` reach an audited list of `Cool`'s text rows (`COOL_TEXT_FOR_CLOSED_SHAPES`:
   `words`, `codes`, `lines`, `trim*`, `flip`, `chomp`, `chop`, `comb`, `fmt`, the case maps, `encode`, the normalization forms and the
   per-codepoint Unicode methods), the way §9.16 opened their numeric rows. `IO::Path`'s own `lines`, `words` and `comb` come first in its MRO.

With that the arms that only re-called a `Str`/`Cool` row handler are deleted: from `dispatch_core_str.rs` `naive-word-wrapper`, `words`,
`codes`, `lines`, `Date`, `DateTime`, `trim`, `trim-leading`, `trim-trailing`, `flip`, `chomp`, `chop`, `comb`, `fmt`; from
`dispatch_core_numeric.rs` `uc`, `lc`, `fc`, `tc`; from `dispatch_core_math.rs` `tclc`, `wordcase`, `encode`, `NFC`, `NFD`, `NFKC`, `NFKD`;
from `dispatch_core_unicode.rs` `ord`, `ords`, `uniname`, `uninames`, `uniprop`, `uniprops`, `unival`, `univals`. `method-rows-report.py
--arms`: the pure arity cascades go from 369 arms to 336.

- **Behaviour changes toward Rakudo** (pinned in `t/oo/method/shapeless-cool-method-rows.t`): a `Str` subclass instance answers
  `naive-word-wrapper`, `chomp` and `Date` (they were `X::Method::NotFound`); `IO::Path.chomp` is `Cool.chomp`; a receiver that is no
  `Cool` (`Pair`, `Set`, `Bag`, `Capture`, `Version`, `Sub`, `Regex`) answers `X::Method::NotFound` for these methods instead of a text
  made from its string form. A probe of the 34 deleted names over ~40 receiver kinds against the pre-change binary found no answer that
  matched Rakudo before and does not now.
- **The E2 probes** (§9.78 item 2): `native_method_arities` asks the table at arity 0 too, and the type-object probe accepts a name the
  `Cool` type-object gate (`cool_type_object_gate`) answers ahead of the table. `try_native_method_raw` asks the table again once it has
  stripped a named argument no method declares (`255.fmt(:zzz)`): the table was asked with it and declined, and the arm is gone.
  The recognition row `Str.encode` loses `TYPE_OBJECT_OK`, which only the deleted arm backed: Rakudo refuses `Str.encode` ("Cannot unbox
  a type object").
- **Left in `dispatch_core_str.rs`**: `so`, `not`, `is-lazy`, `lazy`, `join`, the `Mu`/`Any` answers every receiver reaches by name. They
  need the same step for `Mu` and `Any` (every receiver reaches their rows), which is a larger audit than `Cool`'s text rows: `Any` declares
  the iteration methods. `chars` stays in `dispatch_core_unicode.rs` because a `Blob` must raise `X::Buf::AsStr` and has no row for it.
- **Found, filed**: a `Cool` row on an instance reads the instance's built-in string form, not a user `Str` method (#12848); `Cool.fmt` and
  the text methods on a `Cool` type object (#12849).

### 9.80 Slice 5: `Any`'s list methods reach every receiver; the `dispatch_core_list` arms go (2026-10-12)

`refactor/11276-shapeless-any-resolution`. The arms of `methods_0arg/dispatch_core_list.rs` (`end`, `flat`, `sort`, `reverse`, `unique`,
`repeated`) answered every receiver by name. They are deleted; the receivers reach `Any`'s rows instead, the same three ways §9.79 opened
`Cool`'s text rows:

1. **The rows carry the arms' bodies.** `Any.end` is a `Narrow` handler with the arm's full body (the last index of whatever the receiver lists
   as; `X::Cannot::Lazy` for a lazy list; a `LazyList` still being produced declines to the interpreter), and `Any.reverse` falls back to
   `List.reverse`'s handler beyond a scalar or a hash. `Any`'s six list rows are `TYPE_OBJECT_OK`: Rakudo's `Any` candidates take `self.list`,
   and a type object lists as itself (`Str.sort` is `(Str,)`, `Int.end` is `0`).
2. **Closed shapes** reach an audited subset (`ANY_LIST_FOR_CLOSED_SHAPES`, `DispatchShape::reaches_audited_any_method`): none reaches
   `sort`, whose handler reads only a list; only a `Pair` reaches `reverse`; a `Capture` keeps its own `flat` (the `Capture` block of
   `methods_0arg`); the quant hashes keep reading as their pairs for `unique` (`hashlike_receiver_as_pairs_list`, the pair-view step
   §9.60 warned about); a `Match` reaches none (it answers on its matched string).
3. **Shapeless receivers** reach them in `method_table::answer_shapeless`, after the cascades: a code object, a regex, a promise, a channel,
   a RakuAST node, a type object with no shape (`Any`, a user class), an enum value, a `Seq` without a settled shape, and an instance whose
   MRO reaches `Any` before a built-in class that declares the method. A `Failure` is never answered there.

`method-rows-report.py --arms`: the pure arity cascades go from 336 arms to 330. A probe of the six names over ~45 receiver kinds against the
pre-change binary found no answer that changed except `MyStr.reverse` (now `("ab",)`, as in Rakudo, through §9.79's native base).
`t/oo/method/shapeless-any-list-method-rows.t` pins the receiver kinds.

- **Left**: `Capture.reverse` is `X::Method::NotFound` before and after (Rakudo: the positionals reversed); `List.reverse`'s handler does
  not read a `Capture`. `so`, `not`, `is-lazy`, `lazy` and `join` in `dispatch_core_str.rs` need the same step for `Mu`'s rows (done
  in §9.81).

### 9.81 Slice 5: `Mu`'s `so`/`not`/`is-lazy`, `Any.join` and `lazy` reach every receiver; `dispatch_core_str` goes (2026-10-12)

`refactor/11276-mu-any-dispatch-core-str`. The last arms of `methods_0arg/dispatch_core_str.rs` (`so`, `not`, `is-lazy`, `lazy`,
`join`) and the one-argument `join` arm of `methods_narg/dispatch_1arg.rs` are deleted, and the file with them. The receivers reach rows
the three ways §9.79 and §9.80 opened:

1. **`Mu`'s answers.** `Mu.so`, `Mu.not` and `Mu.is-lazy` are arity-0 rows (`ctors_mop/mu_answers.rs`, `TYPE_OBJECT_OK`) carrying the
   arms' bodies: `so`/`not` mark a `Failure` handled, `is-lazy` reads an `Iterator`'s stored flag, throws `X::Seq::Consumed` for a
   consumed `gather` list and asks `is_genuinely_lazy` of any other `LazyList`. They sit beside the arity-1 `OWNER_ONLY |
   DEFERRAL_BASE` rows of §9.56, which `owner_row` skips. Every closed shape reaches them (`MU_FOR_CLOSED_SHAPES`,
   `DispatchShape::may_reach_audited`), a shape's own row first (`Match.not`, `Range.is-lazy`). `answer_shapeless` answers them for
   every receiver without a shape, a `Failure` included; an instance answers from the first class in its MRO with a row for the name.
   A user `Bool` is still called before the table is asked (`WithBool.new.so` is `False`).
2. **`Any.join`.** The row is `TYPE_OBJECT_OK` and `join` joins `ANY_LIST_FOR_CLOSED_SHAPES`, which a `Match` now reaches for `join`
   only (it joins its positional captures; the `Match` block of `methods_0arg` no longer delegates `join` to the matched string).
   `join_core` reads a `Blob`/`Buf` as its elements and any receiver it does not read as a list as a one-element list (`self.list`).
   Any other built-in class with a shape (`IO::Path`, `IO::Handle`, `Version`, ...) is a one-element list of its `.Str`.
   An element that needs its own `Str` (an instance, a type object, `Nil`) declines the row to the interpreter's list join
   (`dispatch_join_method`), which now takes such a receiver as its one-element list, so a user `Str` is called and a type object
   warns. A `Thread` still declines to the thread-join primitive. `answer_shapeless` passes `join`'s separator, and the one-argument
   entries call it after the cascade as the zero-argument ones do.
3. **`Iterable`'s `lazy`.** Rakudo declares `lazy` on `List`, `Seq` and `Map` only (`5.lazy` is `X::Method::NotFound`, as before).
   The re-tag of a lazy `LazyList` (kept lazy when assigned to an array) moves into the shared `lazy` handler, and `answer_shapeless`
   answers a `LazyList` or unsettled `Seq` from `Seq.lazy` and a lazy or shaped array or a `Slip` from `List.lazy`
   (`ITERABLE_FOR_SHAPELESS`). `List.lazy` and `Map.lazy` are `TYPE_OBJECT_OK`: Rakudo lists a type object as itself
   (`Array.lazy` is `(Array).lazy.Seq`; the arm answered the type object).
4. **The E2 probes.** The recognition table gains `Mu.is-lazy` (Rakudo declares it on `Mu`). `native_method_arities` (the
   inverse probe of the recognition rows) also asks the guard step's settled receiver and `method_table::shapeless_row`, the
   row-finding half of `answer_shapeless`: `Any.join` on the `Any` type object finishes in the interpreter, but the row is found.

`method-rows-report.py --arms`: the pure arity cascades go from 330 arms to 324. A probe of the five names over ~50 receiver kinds (and
`join` with a separator) against the pre-change binary found these answers changed, each toward Rakudo and pinned in
`t/oo/method/shapeless-mu-any-method-rows.t`: an instance joins its own `Str` (`MyCool.new.join` is `"mc"`, was `"MyCool<42>"`); a
type object joins to `""` with the uninitialized-value warning (was `"(Int)"`); `Blob.join` joins its bytes (was its gist); `Match.join`
is its positional captures at both arities (the zero-argument form answered the matched string); the one-argument `join` of a code
object, a `Version`, an enum value or a type object answers instead of `X::Method::NotFound`. `Seq.lazy` and `Range.lazy` on the type
objects are `X::Method::NotFound` (were the type object; Rakudo raises `X::Parameter::InvalidConcreteness`). `Array.lazy`,
`List.lazy` and `Hash.lazy` are a lazy list of the type object (were the type object).

- **Left**: a mixin keeps joining what its inner value lists as through the runtime's mixin delegation (`join_core` declines it),
  so an instance mixin with its own `Str` still joins its default rendering (Rakudo: its `Str`), as before.
  `Thread.join`, `Seq.join` and `Mu.join` on the type objects answer `""` with a warning where Rakudo raises; `Regex.join`
  answers its source where Rakudo warns and answers `""` (both as before for the zero-argument form). A consumed `gather` list answers
  `.is-lazy` `False` where Rakudo throws `X::Seq::Consumed` (before and after: the arm's check is reached only for a list not yet
  marked consumed).

### 9.82 Slice 5: `Mu`'s `item`/`sink`, `Iterable`'s `race`/`hyper` and the `succ`/`pred` rows; the `dispatch_core_math` delegating arms go (2026-10-12)

`refactor/11276-mu-item-sink-iterable`. The arms of `methods_0arg/dispatch_core_math.rs` that answered every receiver by name (`item`,
`sink`, `race`, `hyper`, `succ`, `pred`) and the `so`/`not` arms of the `Match` block of `methods_0arg` are deleted, by the same three
ways as §9.81:

1. **`Mu.item` and `Mu.sink`** are arity-0 rows of `ctors_mop/mu_answers.rs` (`TYPE_OBJECT_OK`; `item` is the shared `lazy::item`
   handler, `sink` carries the arm's body: a plain array answers `Nil`, an unhandled `Failure` throws, a settled `Seq` marks itself
   consumed, a `Seq` with a deferred source or a `LazyList` declines to the interpreter, which pulls it). `MU_FOR_CLOSED_SHAPES` gains
   both, and the recognition table gains `Mu.item` and `Mu.sink` (Rakudo declares them on `Mu`).
2. **The `Match` block** of `methods_0arg` delegates a method to the matched string unless a `Mu` or `Any` row reaches the `Match`
   shape (`DispatchShape::reaches_audited_ancestor`), which replaces its `so`/`not` arms and the `join` exclusion of §9.81: `Match.item`
   is the `Match` (it was the matched string).
3. **`race` and `hyper`** join `lazy` in `ITERABLE_FOR_SHAPELESS`: a shapeless `LazyList`/`Seq` reaches `Seq`'s rows and a lazy or shaped
   array or a `Slip` `List`'s, and the `Iterable` role's type object `List`'s (`Iterable.hyper.configuration`, which the `hyperize`
   dist reads); any other receiver is `X::Method::NotFound`, as in Rakudo. An instance of a user class that `does Iterable` was
   answered a one-element list by the arm and is `X::Method::NotFound` now; Rakudo reads it through its `iterator` (#12929).
4. **`succ`/`pred`**: every receiver the arm answered has its own row (`Str`, `Int`, `Num`, `Rat`, `FatRat`, `Complex`, `Bool`, the
   temporal classes, `IO::Path`); the arm only reached type objects. `Bool.succ`/`Bool.pred` are `TYPE_OBJECT_OK` (`Bool.succ` is
   `True`, as in Rakudo); the `Int`/`Str`/`Num`/`Rat`/`Complex` recognition rows lose `TYPE_OBJECT_OK`, which only the arm backed.
   `$x++` on an undefined `Int` is unchanged (the increment operator does not call `.succ` on the type object).

`method-rows-report.py --arms`: the pure arity cascades go from 324 arms to 316. A probe of the six names over ~45 receiver kinds against
the pre-change binary found these answers changed, each toward Rakudo and pinned in `t/oo/method/shapeless-mu-item-sink-method-rows.t`:
`Match.item` is the `Match`; `Bool.succ`/`Bool.pred` on the type object are `True`/`False` (were `Bool`); `Int.succ`, `Str.succ` and
their `pred` are `X::Method::NotFound` (were the type object; Rakudo raises `X::Numeric::Uninitialized` /
`X::Parameter::InvalidConcreteness`); `Range.race`/`Range.hyper` on the type object are `X::Method::NotFound` (were `(Range,)`; Rakudo
raises `X::AdHoc`). Sinking a `gather`, a `map`, a handled and an unhandled `Failure` answers as before.

- **Left in the arity-0 cascade's delegating arms**: `pick`, `roll`, `pickpairs` and `minmax` of `dispatch_core_range.rs`.

### 9.83 Slice 5: `Any.pick`/`Any.roll`/`Any.minmax` reach every receiver; the sampling and `minmax` arms go (2026-10-12)

`refactor/11276-range-sampling-rows`. The delegating `pick`, `roll`, `pickpairs` and `minmax` arms of
`methods_0arg/dispatch_core_range.rs`, the `pick`/`roll`/`pickpairs` arms of `methods_narg/dispatch_1arg.rs` and the `minmax` arm of
`methods_0arg/collection.rs` are deleted:

1. **`Any.pick` and `Any.roll`** (both arities) are `TYPE_OBJECT_OK` rows (`Int.pick` is `Int`, as in Rakudo, where `Any`'s candidate
   is `self.list.pick`); the `List`, `Range`, `Map` and quant-hash rows keep needing an instance.
2. **`ANY_LIST_FOR_CLOSED_SHAPES` gains `pick`, `roll` and `minmax`**, so a `Capture`, a `Blob`, an `IO::Path`, a `Version` or a
   `Pair` reaches `Any`'s row, and `answer_shapeless` answers a `Seq`, a `Slip`, a shaped or lazy array, a code object, a promise, an
   instance and a type object with no shape. It admits a count for `pick` and `roll` as it does a separator for `join`.
3. **`Mu` is no `Any`**: `answer_shapeless` refuses the `Mu` type object for `Any`'s list methods, so `Mu.pick`, `Mu.roll` and
   `Mu.flat` are `X::Method::NotFound` (they answered `Mu`), as in Rakudo.
4. **`pickpairs`** is declared only on the quant hashes, whose rows answer it; the arm answered nothing else.
5. The sub forms `pick($n, @list)` / `roll($n, @list)` called the 1-arg cascade; they call `sampling::pick`/`sampling::roll`, the one
   implementation, directly.

`method-rows-report.py --arms`: the pure arity cascades go from 316 arms to 308. A probe of the four names (both arities) over ~50
receiver kinds against the pre-change binary changed only `Mu.pick`/`Mu.roll` (pinned in `t/types/any-sampling-minmax-rows.t`); the
quant-hash answers differ run to run on the old binary too (hash order). **Found, not fixed** (#12958): `Int.roll(2)`/`Any.roll(2)`
are `X::Method::NotFound` and `Int.pick(2)` is `((Int))`, because `try_native_method_raw` sends a type object's `pick`/`roll` to the
interpreter, whose 1-arg native entry never asks the table; `Mu.unique` and `Mu.minmax` are still answered.

- **Left in `dispatch_core_range.rs`**: `head`, `tail`, `first`, `min`, `max`, `of`, `keyof` and the `Range` accessors, which carry
  real bodies.

### 9.84 Slice 5: `head` and `tail` are rows at both arities; their arms go (2026-10-12)

`refactor/11276-head-tail-rows`. The `head`/`tail` arms of `methods_0arg/dispatch_core_range.rs` (with real bodies) and the
delegating `head`/`tail` arm of `methods_narg/dispatch_1arg.rs` (with its `head_tail.rs`) are deleted:

1. **The bodies move** into `method_table/collections/terminals.rs` as `head_one`/`tail_one`, the one implementation of the argless
   forms. They are the rows `Any.head`, `List.head`, `Map.head`, `Any.tail`, `List.tail` and `Array.tail` at arity 0, the owners
   Rakudo declares; `Seq.head`'s row calls `head_one` too (its private copy is gone).
2. **`Any.head` and `Any.tail` are `TYPE_OBJECT_OK` at both arities** (`Int.head` is `Int`, `Int.tail(2)` is `(Int,)`): the arm
   answered shaped type objects only because nothing ahead of it did.
3. **`ANY_LIST_FOR_CLOSED_SHAPES` gains `head` and `tail`**, and `answer_shapeless` admits their count. A user-class instance (any
   class with no dispatch shape, `Supply` included) is declined, as the arm declined every instance: its own accessor, or its
   `iterator`, is the interpreter's. An instance of a built-in class with a shape (`Date`) is now answered, as itself.

`method-rows-report.py --arms`: the pure arity cascades go from 308 arms to 303. The probe (both arities, ~55 receiver kinds) changed,
each toward Rakudo: `Mu.head` is `X::Method::NotFound` (was `Mu`); `IO::Path.head` and `Date.head` answer the receiver (were
`X::Method::NotFound`). `class S is Str` instances now answer `Str` for `.tail` as they already did for `.head` (Rakudo answers the
`S`); that and `Mu.tail` answering `Mu` are recorded on #12958. Pinned in `t/types/any-head-tail-rows.t`.

- **Left in `dispatch_core_range.rs`**: `first`, `min`, `max`, `of`, `keyof` and the `Range` accessors.

### 9.85 Slice 5: `.first` with no matcher, `min` and `max` lose their arms (2026-10-12)

`refactor/11276-first-min-max-rows`. The `first`, `min` and `max` arms of `methods_0arg/dispatch_core_range.rs` are deleted:

1. **`first`**: the arm answered an `Array` only; the `Any.first` row (an interpreter row) answered every other receiver with itself
   and fell through to the arm for an `Array`. The row's argless branch now answers `method_table::terminals::first_one`, the first
   element of the invocant's list, for every receiver: a `Range`, a `Hash` or quant hash (a `Pair`), a `Capture`, a `Blob`, a `Seq`,
   a list mixin (what it wraps). A user-class instance stays its own one-element list. Toward Rakudo except `Range.first`, which
   Rakudo answers with a binding error (its `Range.first` requires a matcher); mutsu answers the first element.
2. **`min`/`max`**: the arm's body (an `Array` walk skipping undefined elements, a `Range` delegating to its row, a scalar answering
   itself) is replaced by the rows already there: `Any.min`/`Any.max` answer plain lists and scalars and now read a `Pair` as its one
   element (`ANY_LIST_FOR_CLOSED_SHAPES` gains `min`/`max`, reached by the `Pair` shape only); a list holding undefined or non-plain
   elements declines to the interpreter's walk, as a `Seq` already did.

`method-rows-report.py --arms`: the pure arity cascades go from 303 arms to 300. The probe (the three names, both arities, ~60
receiver kinds) changed only `first` with no matcher (above) and `(1..3).lazy.min`/`.max`, which answered the `Seq` itself and now
answer `1`/`3`; Rakudo raises `X::Cannot::Lazy` (#12971). Pinned in `t/types/any-first-min-max-rows.t`.

- **Left in `dispatch_core_range.rs`**: `of`, `keyof` and the `Range` accessors (`excludes-min`, `excludes-max`, `bounds`, `is-int`,
  `infinite`).

### 9.86 Slice 5: `of` and `keyof` are rows; role-declared rows; a type-object branch; `dispatch_core_range.rs` goes (2026-10-12)

`refactor/11276-of-keyof-rows`. The last two arms of `methods_0arg/dispatch_core_range.rs` answered `of`/`keyof` for type objects
by *name* (`"Positional"` → `Mu`, `Bag[Int]` → `Int`, `buf16` → `uint16`) and declined the container instances to four
name-matching blocks of `call_method_with_values` and a VM bypass. Two design questions had no answer in the table: Rakudo declares
most of these methods on **roles** (`Positional.of`, `Associative.of`/`keyof`, `Callable.of`, `Baggy`/`Setty`/`Mixy.of`,
`QuantHash.keyof`; only `Hash.of`/`keyof`, `Array.of` and `Blob.of` are on classes), and a role or a **parameterized type object**
(`Positional`, `Sub`, `Bag[Int]`, `Hash[Int,Str]`) has no `DispatchShape`. Decisions:

1. **A row may be owned by a role.** The registration walk (`table_const.rs`) visits, after each class of a shape's catalog MRO,
   the roles that class composes (`catalog::composes_role_at`: listed in its catalog `roles` and not in its parent's, so
   `Positional` is composed at `List`, `Associative` at `Map`), because Rakudo flattens a role's methods into the class that does it:
   they lose to the class's own rows and win over its parent's. A role's rows reach every shape whose type composes it, open or
   closed, once audited for all of them: `DispatchShape::reaches_role` reads `ROLES_WITH_ROWS`, the seven roles above. A role that
   gains a row is added there only after that audit.
2. **A type object with no shape is answered through `answer_shapeless`**, by a branch that runs before the audited name lists
   (`type_object_row`): the base name (before `[`) is looked up in the catalog and its MRO walked as in 1; a role is its own owner,
   followed by the roles it does (`ROLE_PARENTS`: `Mixy` does `Baggy`, `QuantHash`, `Associative`); a buffer name (`buf16`,
   `Buf[uint32]`, `utf8`) is `Blob`. The branch passes the interpreter on, so a code type object reaches `Code.of` (an interpreter
   row) and answers `Mu`. A user type is not in the catalog and declines to the interpreter, which answers `class A does
   Positional[Int]` from its composition as before. A shaped or lazy array (no `Array` shape) reaches `Array.of` the same way.
3. **The parameters stay in the type object's name.** A parameterized type object is a `Package` whose name is its spelling
   throughout the runtime (type checks, `.^name`, the constructors); making the parameterization structural data on the type object
   would touch every one of those readers for no gain here. The handlers parse the name once per call (`Hash[Int,Str]`,
   `Hash[Int{Str}]`, nested brackets through `split_balanced_comma_list`).
4. **An instance answers from the descriptor it carries** (`runtime::embedded_container_type_info`, the pure half of
   `container_type_metadata_with`; an `Instance`'s side-table descriptor stays the interpreter's), so every handler is pure.
   `tag_container_metadata` no longer lets a variable-constraint registration erase a quant hash's declared type: `my %b is
   BagHash[Int]` registers `BagHash[Int]` as the variable's constraint, which re-tagged the value with no declared type and left
   `QuantHash.keyof` nothing to read.

Deleted: the two arms and the file (its random-range helpers move to `builtins/sampling/range.rs`, their one user), the six
`Set`/`Bag`/`Mix` `of` rows (now `Baggy`/`Setty`/`Mixy`'s), the `of`/`keyof` blocks of `call_method_with_values` for `Array`/`Hash`
instances, parametric type objects and quant hashes, the parametric-type-object branch and the quant-hash `keyof` block of the
by-name mutating dispatch, and the `keyof` half of the VM's hash bypass. Kept, with the interpreter: the answers for a user class
composing `Positional[V]`/`Associative[V,K]` and for a mixin that declares `of`, and the by-name `@`/`%` variable fallback to the
variable's constraint.

`method-rows-report.py --arms`: the pure arity cascades lose 2 arms and a file (241 → 239 after §9.87). The probe (both names over ~90
receivers, against the pre-change binary and `raku`) changed only toward Rakudo: `Map.of`, `Associative.keyof`, `Associative[..].keyof`
(`Str(Any)`/the key parameter), `Range.of`, `Pair.of`/`keyof`, `Callable[Int].of`, `Regex.of`, `QuantHash`/`Baggy`/`Setty`/`Mixy`
`of`/`keyof`, `Stash`/`PseudoStash` `of`/`keyof`, `Bag[Int].keyof`/`Set[Str].keyof` (were `Mu`), `SetHash[Int].new.keyof`
(was `Mu`), `Hash[Int,Str]` bound to `%x` (`.of` was `Int,Str`) and `Map.new.of` (was the empty type name). Pinned in
`t/routines/signature/parametric-of-keyof-rows.t`. Still differing from Rakudo, out of scope: `Junction.of`/`keyof` (Rakudo `Mu`; mutsu has no `Junction`
owner) and `keyof` of a user class composing `Associative[V,K]` (`X::Method::NotFound`, #12989); `.^can`/`.^methods` do not yet
walk a class's composed roles, so `List.^can('of')` is empty (#12990).

### 9.87 Slice 5: a `Date`/`DateTime` subclass instance reaches the rows through their owner; `temporal_dispatch`'s arms go (2026-10-12)

`refactor/11276-temporal-subclass-rows`. `methods_0arg/temporal_dispatch.rs` held two name `match`es (`date_method_0arg`,
`datetime_method_0arg`, 59 arms). For an instance of a user subclass of `Date`/`DateTime`, which has no shape, they called the row
handlers of `instances/{date,dateish,datetime,temporal}.rs` by name. They are replaced by one lookup in the 0-arg cascade,
`temporal_owner_row`: `owner_row("DateTime" | "Date", method, 0)`, then `invoke` with the instance as invocant. Every one of those
rows reads the instance's attributes, which a subclass instance carries too. `perl` asks the `raku` row (`Mu.perl` is `.raku`).
The `DateTime` subclass `utc` exception (the interpreter re-blesses the result) stays in front of it.

`method-rows-report.py --arms`: the pure arity cascades lose 59 arms (300 → 241 on this branch's base). A probe of all 76 argless
methods on a `Date` and a `DateTime` subclass instance was identical before and after. Pinned in
`t/types/temporal/date-subclass-owner-rows.t`, for the 67 answers that agree with Rakudo. **Found, not fixed** (#12994): `succ`,
`pred`, `raku`, `WHICH` and `utc` of a subclass instance answer with the base class name, as before.

### 9.88 Slice 5: the cascades decline a call the table answers; a shapeless `Match` answers by owner (2026-10-12)

`refactor/11276-shaped-forwarding-arms`. Most arms left in the pure cascades after §9.87 were *shadows*: arms for a receiver that
has a shape and a row, which call the row's own handler. `native_method_0arg` asks the table before the cascade, so on that route
they never ran. They survived for the two callers that run the cascade without the table, the debug cross-check
(`debug_assert_matches_full_path`) and the VM entry whose user-override veto refused the row (`try_native_method_raw`): with the
shadow gone, a later generic arm claimed the call (`\(1, :a(2)).hash` reached the list path and answered `{1 => 2}`), and the
cross-check failed. Decisions:

1. **One decline step per cascade.** After the probes that unwrap a receiver (the itemized hash, the hash-as-pairs list, the
   label, the lazy match, the `.VAR` descriptor, the `Scalar`, the `Seq` state, the baggy and `Pair` subclass payloads, the
   `Failure` rows, the native base and the mixin), the 0-arg cascade returns `None` when `method_table::answer` answers the call;
   the 1-arg cascade does the same in front of its name `match`. The probes above the step still run under the cross-check, so it
   still catches one that claims a row's call; below it, a row's call has no arm to reach. In the VM's vetoed route the cascade
   now declines too, so the interpreter answers the user's `.wrap`/`augment`, as that route's comment already required (the
   shadow arms answered the built-in there).
2. **`native_method_1arg` asks the table first**, as `native_method_0arg` does, and `native_method_1arg_without_table` is the
   cascade the two callers above use. Its receiver probes re-enter `native_method_1arg` with the unwrapped receiver (a `Scalar`'s
   value, an itemized hash made plain, the 0-arg entry's and `call_method_with_values`'s #10744 rule), so the decline step asks
   the table about the receiver the table was asked about: the VM refuses `$map.contains('a')` on the itemized form, and before
   this rule only the deleted `contains` arm answered it (the interpreter's route loses the resumable warning). The 1-arg callers that reached the cascade directly (the routine forms, `.^can`'s
   recognition probe, `Complex`/`Instant` delegation) get the row now; the debug cross-check already held the two answers equal.
3. **A shapeless `Match` answers by owner** (`match_owner_row`: `owner_row("Match", ..)` + `invoke`, the §9.87 mechanism): a
   grammar cursor, a lazy cursor and an instance of a `Match` subclass. It replaces the forwarding arms of the lazy-match fast path
   and of the `Match`-instance block (`from`, `to`, `pos`, `Str`, `Bool`, `orig`/`target`, `made`/`ast`, `clone`, `prematch`,
   `postmatch`, `actions`, `caps`, `chunks`, `gist`, `raku`/`perl`, `WHICH`). The lazy path still answers before the probes
   that would materialize the match.

Deleted: the `Uni` shadows (`elems`/`codes`/`Int`/`Numeric`/`Str`/`list`/`gist`/`raku`), the `Capture` shadows (`hash`, `list`,
`elems`/`Numeric`, `keys`, `values`, `kv`, `pairs`, `antipairs`, `raku`, `gist`, `Str`), `Version`'s `parts`/`plus`/`whatever`,
the 0-arg `re`/`im`/`reals`, `key`/`value`/`antipair`, `contents` and `squish` arms, and the 1-arg `base` and `base-repeating`
arms. Kept, each because a receiver or argument
no row admits still reaches it (found by deleting the arm and probing): the rest of `collection.rs`/`coercion.rs` (type objects,
`Nil`, a shaped array, a stash, an `IO::Path`), the `Capture` arms with no row (`Hash`, `Int`, `perl`, `Bool`, `WHAT`, `flat`,
`Seq`, `List`), `Uni`'s `chars`/`comb`/`perl`, `subbuf` (a user class composing `Blob` over a `VMArray` body has no shape),
`comb` and `sprintf` (no 1-arg row on `Str`/`Cool`: the arm is the implementation), `contains`, `index`, `rindex`, `substr`,
`split` and `fmt` (the `Match` shape reaches no `Cool` row, and the recognition table's `Match` rows of these names are
answered by the arm on the matched string; `native_method_row`'s `*_rows_are_backed_by_the_cascade` tests pin them), and the
1-arg arms whose argument is not a plain scalar (`in-range` with a
`Range`, `round` and `parse-base` on an allomorph or a `Slip`, `AT-POS`, ...).

`method-rows-report.py --arms`: the pure arity cascades go from 239 to 189 arms. A probe of 34 0-arg and 36 1-arg method names
(with 1 to 5 arguments each, Regex, Range, WhateverCode and Junction arguments among them) over 50 receivers (every shape, type
objects, user subclasses of `Int`/`Str`/`Array`/`Hash`, a plain instance, an enum, a mixin, an allomorph, code, an exception, a
lazy list) gave the same answers before and after, apart from object ids and hash order; the debug `prove t/` over the whole
suite found the two receivers the probe lacked (the itemized `Map` and the `VMArray` buffer class), and the recognition tests
the arms the interpreter had been answering identically (the `Match` receiver, `comb`/`sprintf`). Pinned in
`t/oo/method/method-table-cascade-decline.t`. The cost of the step is one more table lookup on a call the table did not answer
(a bit test when no row has the name); a handler that declines its receiver runs twice on that call (§9.7 waives the measurement).

### 9.89 Slice 5: a `Match` reaches `Cool`'s search and text rows; the 1-arg search, split and format arms go (2026-10-12)

`refactor/11276-match-cool-text-rows`. §9.88 kept the 1-arg `contains`, `index`, `rindex`, `substr`, `split` and `fmt` arms because
the `Match` shape reached no `Cool` row, so the arm, run on the matched string, was the implementation of the recognition table's
`Match` rows of those names. Decisions:

1. **The `Match` shape reaches `Cool`'s rows per audited name.** Rakudo's `Match` is `Capture is Cool`, and neither `Match` nor
   `Capture` declares a text method, so each is `Cool`'s candidate (`self.Str.METHOD`), whose handler here reads the receiver's
   string form, the matched text. `DispatchShape::may_reach_audited` admits `Cool` for `Match`, and `reaches_audited` lets it
   through for `COOL_TEXT_FOR_CLOSED_SHAPES` (the 0-arg text methods and `fmt`) and `COOL_TEXT_FOR_MATCH`: `contains`,
   `starts-with`, `ends-with`, `index`, `rindex`, `substr`, `split`, `samecase`, `samemark`, `indent`, `subst`, `trans`, `indices`,
   `sprintf` and `IO`. Not the numeric rows: `Match` declares its own `Int`/`Num`/`Numeric`, and a `Cool` numeric handler would read
   the receiver through `numify`, which has no `Match` branch.
2. **A match with no shape resolves as a `Match`.** `match_owner_row` (§9.88) asked `owner_row("Match", ..)`, which finds only the
   rows `Match` declares. It is now `method_table::answer_as(DispatchShape::Match, ..)`: the row a plain match resolves to, its own or
   an audited ancestor's, with the arguments admitted as the guard step admits them. The 1-arg cascade asks it too, after its decline
   step, so a grammar cursor or a `Match`-subclass instance reaches the same rows at both arities.
3. **`comb` with an `Int` or `Str` matcher is a row** on `Str` and `Cool` (`Handler::Narrow(comb::native_comb_method)`, which declines
   a `Regex`, a code object and a `Pair`), so the arm that was its implementation goes.

Deleted: the 1-arg `contains`, `index`, `rindex`, `substr`, `split`, `fmt`, `comb`, `sprintf` and `starts-with`/`ends-with` arms.
Each served, besides the receivers a row now answers, the receivers no row reaches (a `Range`, a `Seq`, a `Pair`, a `Capture`, a quant
hash, an enum, a code object, a `Version`, a `Uni`, a `Cool` subclass instance, `Nil`); deleting the arm hands those to the
interpreter's `dispatch_contains`/`dispatch_index`/`split`/`fmt`/`sprintf`/`comb`, whose answers were the same on every probe. The one
difference was a type-object needle of `contains`, which the arm refused and the interpreter searched for; `dispatch_contains` now
refuses it as `dispatch_index` already did (`X::AdHoc`, where Rakudo raises `X::Multi::NoMatch`). Kept: `uniprop`, `uniprops`,
`unimatch`, `samemark`, `samecase` and `indent`, whose arms are the only implementation for the receivers with no row (deleting them
made `(1..3).indent(2)`, an enum's `samecase` and a `Cool` subclass's `uniprop` answer `X::Method::NotFound`).

`method-rows-report.py --arms`: the pure arity cascades go from 189 to 180 arms. A probe of 47 calls (the search, split, format,
`comb` and text methods with string, numeric, regex, junction, type-object, range and named arguments) over 38 receivers, and of
`sprintf`/`comb` with 13 argument kinds, gave the same answers before and after apart from hash order and the type-object needle above.
Pinned in `t/regex/match/match-cool-text-rows.t`. Still differing from Rakudo, out of scope: the non-`Cool` receivers (`Pair`,
`Capture`, the quant hashes, `Blob`, `Version`, `Uni`, code objects, a plain instance) answer these methods on their string form,
where Rakudo has no candidate (`X::Method::NotFound`), and a `Cool` subclass instance with its own `Str` is searched by its default
rendering (#13035).

### 9.90 Slice 5: every `Cool` receiver reaches the 1-arg Unicode, case and indent rows; their arms go (2026-10-12)

`refactor/11276-cool-unicode-args-rows`. §9.89 kept the 1-arg `uniprop`, `uniprops`, `unimatch`, `samemark`, `samecase` and
`indent` arms, the only implementation for the `Cool` receivers no row reached: a `Range`, a `Seq`, an `IO::Path`, an enum and an
instance of a `Cool` subclass. Decisions:

1. **`COOL_TEXT_FOR_CLOSED_SHAPES` gains `unimatch`, `samecase`, `samemark` and `indent`** (Rakudo declares all four on `Cool`, and
   their `Cool` candidates read `self.Str`), so the closed `Range`, `Seq`, `IO::Path` and `Match` shapes reach them; `uniprop` and
   `uniprops` were already there, at every arity. `Cool` gains the `unimatch` rows `Str` and `Int` had, and the recognition table a
   `Cool.unimatch` row.
2. **`answer_shapeless` admits these six names at arity 1**, besides `join`/`pick`/`roll`/`head`/`tail`: an enum and a `Cool`
   subclass instance reach `Cool`'s row. The instance walk now asks each class of the MRO for a row of the call's arity, not arity 0,
   so a built-in class declaring only the argument form stops the walk too.
3. **An allomorph reads its `Str` part** through a 1-arg probe in the cascade, as the 0-arg cascade's allomorph block does: the
   allomorph has no shape, and the interpreter would read its number (`<65>.uniprop("Script")` is `Common`, the script of "6").
4. **`indent` admits any plain argument** (`RowFlags::ANY_ARGS`): `indent(*)` takes the common indentation away.

Behaviour changes, toward Rakudo: a receiver that is not `Cool` (`Pair`, `Capture`, the quant hashes, `Version`, `Uni`, code, a plain
instance) answers `X::Method::NotFound` for these methods instead of reading its string form, and so does a `Str`/`Int` type object
(Rakudo: `X::Multi::NoMatch`); the recognition rows of `Str`/`Cool` `samecase`/`samemark` drop `TYPE_OBJECT_OK`. `Nil` answers `Nil`
(was the empty string; Rakudo raises `X::Multi::NoMatch`, except `Nil.indent`, which is `""`).

`method-rows-report.py --arms`: the pure arity cascades go from 180 to 174 arms. A probe of the six names (with property, pattern,
junction, Whatever and negative arguments) over 25 receivers changed only the answers above. Pinned in
`t/types/cool-text-argument-rows.t`.

### 9.91 Slice 5: `Mu`'s `self`, `defined`, `DEFINITE`, `Bool`, `WHICH`, `WHERE` and `clone` are rows (2026-10-12)

`refactor/11276-coerce-type-object-rows`. `methods_0arg/dispatch_core_coerce.rs` answered these seven names by name for every
receiver no row reached: the closed shapes, the type objects with no shape, a `Junction`, a code object, an enum, a lazy or shaped
array, an unsettled `Seq`, a `Failure` and an instance. Rakudo declares all seven on `Mu`. Decisions:

1. **They are arity-0 `Mu` rows** (`method_table::ctors_mop::mu_identity`, `TYPE_OBJECT_OK`), with the arms' bodies: `self` explodes
   an unhandled `Failure` and deitemizes, `defined` collapses a `Junction`'s eigenstates and handles a `Failure`, `Bool` declines a
   `Real`/`Numeric` instance and a regex (the interpreter's), `WHERE` keeps the NativeCall `Pointer` address. They join
   `MU_FOR_CLOSED_SHAPES`, so every closed shape reaches them (its own row first: `Capture.Bool`, `Match.WHICH`), and
   `answer_shapeless` answers every receiver with no shape, an instance from the first class of its MRO that declares the name.
   `self` gains a `Mu` recognition row (the table had it on `Any`).
2. **The `WHICH` rows answer every receiver.** `Int`/`Str`/... `WHICH` (`scalars::render`) and the collections' (`collections::render`)
   declined anything but their own value kinds, which before this slice fell to the arm: an instance of a user subclass (`class S is
   Int`, `is Array`, a `Stash`), which `answer_shapeless`'s MRO walk now hands to the subclass's built-in ancestor's row, and a `Pair`
   holding a container. Both are `which_of`, the one identity routine; the decline step (§9.88) makes the debug cross-check agree.
3. **`Mu.clone` copies an instance** through `InstanceAttrs::to_clone_map` (a copy of the attributes with a `:=`-bound slot
   snapshotted), the copy the interpreter's clone with `:attr(v)` twiddles (`native_instance_clone_value`) now starts from too, so
   every closed shape whose values are instances (`IO::Path`, `IO::Handle`, `Blob`, `Instant`, `Backtrace`, ...) is answered. A call
   with twiddles declines (the row declares no named argument) to the interpreter, which applies them (`Date.clone(day => 5)`).
4. **`Pair.WHICH` is its own row, flagged `RANDOM`**: a `Pair` holding a container mints a fresh identity per call (`which_of`),
   so the call-site lane's cross-check, which runs the call twice, must skip it.
5. **`Mu.WHERE` is an interpreter row.** A NativeCall handle (a `nativecast`ed `CStruct`/`CArray`, a native array) answers the
   address of its storage, which only the class registry tells (`Interpreter::try_native_handle_repr_where`); the interpreter asked it
   before the cascade, but a row is found before the interpreter is, and `NativeHelpers::Blob`'s `OBJECT_BODY` dereferenced the
   identity hash (a `SIGSEGV` in `t/nativecall/`). `answer_shapeless` hands the interpreter to `Mu`'s rows (the other rows it finds
   stay pure). The `WHICH`/`WHERE` deferral-base rows and `self.Mu::WHICH` call `Mu`'s row instead of the cascade, which no longer
   answers an instance; `shapeless_row` declines a `.VAR` descriptor, whose methods ask the value its container holds (ADR-0064).
6. **The recognition probe** (`native_method_arities`, used by the `*_rows_are_backed_by_the_cascade` tests) counts `Mu`'s audited
   rows for every receiver: an instance reaches them through its MRO, which `shapeless_row` reads only from an interpreter the probe
   does not have.

`method-rows-report.py --arms`: the pure arity cascades go from 174 to 167 arms. The probe (15 names over 95 receivers: every shape,
the type objects of 30 built-in types and a user class, `Junction`s, code, enums, a mixin, allomorphs, an unhandled `Failure`, a
lazy and a shaped array, user subclasses of `Int`/`Str`/`Array`/`Hash`) changed only toward Rakudo: `Match.self` is the match (was its
string), `Capture.clone` and `Uni.clone` answer (were `X::Method::NotFound`). Pinned in `t/types/mu-identity-rows.t`.

**Not done: the numeric and string coercions of the same file** (`Str`/`Stringy`, `Int`, `UInt`, `Version`, `Num`, `Real`,
`Numeric`, and the `Range`/lazy/`Blob` blocks in front of them). Deleting them and probing shows which receivers they are still the
only implementation for, so each needs its owner's row first: `Range`'s `Int`/`Num`/`Real` (Range declares `Numeric`; its handler,
`range::numeric_coercion`, already exists), `FatRat`'s `Real`/`Numeric` (no row reaches a `FatRat`: Rakudo has them on the
`Rational` role), the collections' `Real` (`Array`, `List`, `Hash`, `Map`, `Seq`), `Seq`/`Slip` `Num`, `Blob`/`Buf` `Int`/`Numeric`,
`Match`'s `UInt`/`Real`, `Str.Real`, `Complex.UInt`, the lazy and shaped arrays, a `Junction`'s `Int`/`Num`, a `Stash`'s
`Numeric`, the type objects of `Int`/`Str`/`Num`/`Rat`/`FatRat`/`Complex`/`UInt`/`RatStr` (`Int.Int` is the type object,
`Rat.Int` throws `X::Parameter::InvalidConcreteness`, `Int.Real` warns and is `0`), and `Stringy` of a `Hash` subclass, a `Promise`,
a `Channel` and an unhandled `Failure`. `Version` is answered by rows already: deleting its arm changed no probe.

Found, not fixed: a quoted call of `WHICH`/`WHERE`/`DEFINITE`/`WHAT` on a variable (`$x."WHICH"()`) answers
`X::Method::NotFound` (#13055).

### 9.92 Slice 5: `Cool.Real`, `Cool.Num`, `Cool.UInt`, `Mu.Int` and `Mu.Numeric` answer the receivers the coercion arms did (2026-10-13)

`refactor/11276-coerce-numeric-rows`. The numeric coercion arms of `dispatch_core_coerce.rs` (`Int`, `UInt`, `Version`, `Num`,
`Real`, `Numeric`, and the `Range`, lazy-count and `Int.Int`/`Num.Num` blocks in front of them) were the only implementation for the
receivers §9.91 lists, and the interpreter's slow path does not answer those names (deleting an arm made `"a".Real` warn about a "type
a" and `[1,2].Real` throw `X::Method::NotFound`). Each moved to the owner Rakudo declares it on:

1. **`Cool.Real`** (`scalars::cool_coerce`, `TYPE_OBJECT_OK`): a `Str` parses, an aggregate, a `Seq` or a `Range` is its count, a lazy
   list fails with `X::Cannot::Lazy`, a numeric type object warns and is its type's zero, a `Complex` declines to
   `dispatch_complex_to_real`. `Mu` gains its `Real` recognition row, without which the `Any`/`Mu` Cool-only gate
   (`any_cool_method_gate`) refused `Any.Real`.
2. **`Cool.Num`** takes the arm's type objects (`Num.Num` is `Num`; `Int`, `Str`, `Complex`, `Rat`, `FatRat` and `UInt` throw
   `X::Parameter::InvalidConcreteness`) and the lazy failure, and becomes `TYPE_OBJECT_OK`. **`Cool.UInt`** takes the lazy failure and a
   `Match`'s matched text; it stays an interpreter row (a `Complex` reads `$*TOLERANCE`), so `dispatch_complex_to_real` now asks the
   table with the interpreter for the `Num` it converts to.
3. **`Mu.Int` and `Mu.Numeric`** (`ctors_mop::mu_numeric`, `TYPE_OBJECT_OK`) carry the arms' bodies: `Int.Int`/`UInt.Int` are the type
   object, a concrete-only type object throws, `.Numeric` of a numeric type object warns and is its zero, a `Range` is its count, a
   `FatRat` keeps its type, a `Blob`/`Buf` is its length, a `StrDistance` its edit distance, a `Stash` its symbol count. A built-in
   class with a shape and none of its own (`DateTime.Int`, `IO::Handle.Numeric`, which reach `Mu` through their MRO) is Rakudo's
   `X::Multi::NoMatch`; every other instance declines to `Interpreter::mu_numeric_coercion`.
4. **Reach.** `Range` and `Seq` reach `Cool.Real`/`Num` through `COOL_NUMERIC_FOR_COLLECTIONS`; a `Match` reaches `Cool.UInt`; a `Range`
   reaches `Mu.Int` and a `Blob`/`Buf` both `Mu` rows through a per-name audit (`DispatchShape::reaches_mu_numeric`). A receiver with no
   shape reaches all five through `answer_shapeless`'s `COOL_COERCE_FOR_SHAPELESS`: an enum, a lazy or shaped array, an unsettled `Seq`
   or `Slip`, an instance (its MRO walk falls back to `Cool`, then to `Mu` for `Int`/`Numeric`, whose handlers decline what they do not
   know) and a `Cool` type object with no shape (`RatStr`, `IntStr`; `UInt` reads as `Int`). That branch hands the interpreter to
   these rows too, for `Cool.UInt`.

`method-rows-report.py --arms`: the pure arity cascades go from 167 to 149 arms. The probe (§9.91's 15 names over 95 receivers) changed
only toward Rakudo: `(1..Inf).UInt` is the `X::Numeric::CannotConvert` `Failure` (was `X::AdHoc`) and `slip(1,2).UInt` is `2` (was
`X::Method::NotFound`). Pinned in `t/oo/method/coercion-method-rows.t`.

Left in `dispatch_core_coerce.rs`: `Str`/`Stringy` (a deletion probe lost only `Stringy` of a `Hash` subclass, a `Promise`, a `Channel` and
an unhandled `Failure`, but `Str` is the most common method there is, so it wants its own slice and a full debug run), the
`Buf`/`Blob` reinterpret block (the `Blob`/`Buf` rows' implementation, reached by a buffer class with no shape) and `Complex.Complex`.

### 9.93 Slice 5: `Mu.Str` and `Mu.Stringy` are rows (2026-10-13)

`refactor/11276-mu-str-rows`. The `Str`/`Stringy` arm of `dispatch_core_coerce.rs` rendered every receiver no `Str` row reached, and
`methods_0arg/mod.rs` kept a second `Str`/`Stringy` block for buffers. Rakudo declares both names on `Mu` (`Stringy` is `self.Str`)
and `Str` again on most built-in types; every closed shape but the `IO::Spec` type objects has its own `Str` row already, and only
`Str` (`Str.Stringy` is `self`), `Seq` and now `Blob`/`Buf` have their own `Stringy`. Decisions:

1. **`Mu.Str` is a pure row** (`method_table::ctors_mop::mu_str`, `TYPE_OBJECT_OK`) with the arm's body: an unhandled `Failure`
   throws, a lazy array is `...`, a collection holding a zero-denominator Rational dies, everything else with no shape (a
   `Junction`, a `Promise`, a `Channel`, `Nil`) renders. A built-in type object warns and resumes with `""` through
   `warn_signal_with_resume`, the mechanism `Mu.Numeric` uses (§9.92). It declines what needs the interpreter: an instance or a
   mixin (a class or role may declare `Str` or `message`), a user type object, an enum, a lazy list, a list or quant hash with an
   object element, a regex (it warns) and a code object (`Code.Str`).
2. **`Mu.Stringy` is an interpreter row that calls `Str`** on the receiver, which is Rakudo's definition. A closed shape with no
   `Stringy` (`Array`, `Hash`, `Date`, `IO::Path`, ...) reaches it through `MU_FOR_CLOSED_SHAPES`, so its `Stringy` is its `Str`.
   It cannot answer a pure entry; no pure caller asks `Stringy` (the debug `prove t/` run over the whole suite agrees).
3. **A subclass instance keeps the interpreter's answer.** `answer_shapeless`'s MRO walk would find a built-in ancestor's `Str` row
   (`Exception.Str`, `Str.Str`), which reads the ancestor's state: an `Exception` subclass with its own `message` rendered "E with
   no message" and a `Str` subclass lost its type. For `Str`/`Stringy` the walk answers only when it reaches `Mu`; a built-in
   ancestor that declares the name declines to the interpreter, as before. (The first debug run found these four regressions.)
4. **`Blob.Stringy`/`Buf.Stringy` are rows** (`str_or_stringy`, which names `Stringy` in `X::Buf::AsStr`'s message), and the
   slow path's buffer guard no longer refuses `.Str`: `$u."Str"()` on a `utf8` variable threw `X::Buf::AsStr` where `$u.Str`
   decoded, because the quoted call takes `call_method_with_values` and its guard ignored `utf8`.
5. **`Stringy` of a lazy list renders `...`** like `Str`: the VM's lazy-placeholder intercepts named `gist`/`Str`/`raku`/`perl`,
   so `@lazy.Stringy` and `(1..*).lazy.Stringy` forced the list and threw `X::Cannot::Lazy`.

`method-rows-report.py --arms`: the pure arity cascades go from 149 to 148 arms (the `Str`/`Stringy` arm; the buffer block was a
`method ==` test the report does not count). The probe (`Str`/`Stringy`/`gist` over 110 receivers) changed only toward Rakudo:
`Stringy` of an `IO::Special`, an `IO::Handle`, an `Exception`, a `NumStr`, a `Signature`, `1/0` and a lazy array now agree with their
`Str`. Pinned in `t/types/mu-str-rows.t`.

Found, not fixed: an allomorph's `.Stringy` is a plain `Str` (#13108) and `Signature.Str` renders the source form (#13109).

Left in `dispatch_core_coerce.rs`: the `Buf`/`Blob` reinterpret block, `Complex.Complex` and the `Any.serial` arm.

### 9.94 Slice 5: `Complex.Complex`, `Any.serial` and the buffer reinterpretations; `dispatch_core_coerce.rs` has no arm left (2026-10-13)

`refactor/11276-coerce-tail`. After §9.93 the file's `dispatch` kept three blocks. Deleting them and probing `Buf`/`Blob`/`Complex`/`serial`
over 38 receivers (buffers of every width, their type objects, numbers, collections, code, a `Promise`, a lazy list) showed what each was
still the only answer for:

1. **`Complex.Complex` of the type object** (`method Complex() { self }`). The `Complex.Complex` row becomes `TYPE_OBJECT_OK` with a handler
   that answers a type object itself (`numeric_coerce::complex_self`).
2. **`serial` of almost everything.** The `Any.serial` row was reached only by receivers the cascade did not already answer. It becomes
   `TYPE_OBJECT_OK` and joins `ANY_LIST_FOR_CLOSED_SHAPES`, so every closed shape and every receiver with no shape reaches it; `Mu.serial`
   is `X::Method::NotFound`, as in Rakudo (`shapeless_row` refuses `Mu` for `Any`'s methods).
3. **`.Blob` of a `Blob`, a `utf8` or a `blobN`.** Rakudo's `Blob` declares `Buf` but not `Blob` (`Buf` declares both; §9.86's
   `COERCE_ROWS`), so the block answered calls Rakudo refuses. With it gone they are `X::Method::NotFound`; `.Buf` of every buffer and
   `.Blob` of a `Buf` still answer from the rows. `t/vm/codegen/create-slots-resources-buf-strict-escape.t` pinned the old answer for
   `"ab".encode.Blob` and now pins Rakudo's.

The `dispatch` function and its `try_dispatch!` entry are deleted; the file keeps the exception builders the coercion rows share
(`X::Str::Numeric`, `X::Numeric::Real`). `method-rows-report.py --arms`: the pure arity cascades go from 148 to 147 arms. Pinned in
`t/oo/method/coerce-tail-method-rows.t`.

Found, not fixed: `.Buf`/`.Blob` of a wide buffer drop its element type (#13115); `.Complex` of a non-`Cool` receiver, a concrete-only
type object or a lazy list answers `0+0i` (#13116).

### 9.95 Slice 5: a `Match` reaches `Capture`'s rows (2026-10-13)

`refactor/11276-match-0arg`. The `Match` block of `methods_0arg/mod.rs` answered `keys`, `values`, `kv`, `pairs`, `hash` and `elems`
by name, and every other name it did not know on the matched string, so `Match.antipairs` was `"ab".antipairs` and `Match.serial`
a `Str`. Rakudo's `Match` is a `Capture` and declares none of them. Decisions:

1. **The `Match` shape reaches `Capture`'s rows per name** (`CAPTURE_FOR_MATCH`: `keys`, `values`, `kv`, `pairs`, `antipairs`,
   `hash`, `elems`; `may_reach_audited` admits the `Capture` owner for `Match`), and `Any.serial` (§9.94) next to `join`.
2. **The `Capture` handlers read a `Match`** through one `parts` reader: its positional captures (an unbound one as `Mu`) and its
   named captures in key order. A `Match` is asked first, so a lazy one (a grammar cursor, reached through `match_owner_row`) is
   read from its capture node. `elems` counts the positional captures without collecting them.
3. **`values` and `kv` flatten a list or array value** that is not itemized, which the `Match` block did for a quantified capture
   and Rakudo does for a plain `Capture` too (`\([1, 2], 3).values` is `(1, 2, 3)`; `\($[1, 2]).values` keeps it whole). The
   `Capture` rows did not; they now do, a behaviour change toward Rakudo.
4. The guard test `a_new_shape_reaches_only_its_own_rows` allowed only a closed shape's own `elems`; it now allows an audited
   ancestor's, still never `Any`'s.

Left in the block: `list` (a live view of the positional captures), the positional coercions (`Array`, `List`, `Slip`, `Seq`, `flat`,
`cache`, `eager`, `reverse`), `Hash`, `Capture` and the fallback that answers any other name on the matched string. That fallback is
the next remainder: each name it serves is a `Cool` method Rakudo reaches through `Match`'s `Str` coercion only where `Cool`
declares it, and the audit lists (`COOL_TEXT_FOR_MATCH`) already name most of them. `method-rows-report.py --arms`: the pure arity
cascades go from 147 to 142 arms. Pinned in `t/regex/match/match-capture-method-rows.t`.

### 9.96 Slice 5: a `Match` answers the names it does not declare on itself, not on its matched text (2026-10-13)

`refactor/11276-match-fallback`. After §9.95 the `Match` block of `methods_0arg/mod.rs` still answered every name it did not
know on the matched string (`native_method_0arg(&str_val, ...)`), so `$m.head` was the whole text, `$m.pick` a `Str`,
`$m.naive-word-wrapper` (a `Str` method) answered, and `$m.pairup` failed on an odd one-element list. Probed: every name declared
on `Cool`, `Str`, `Any`, `Mu` and `List`, over a plain match and a grammar match, on the pre-change binary, the new one and
`raku`. Decisions:

1. **The fallback falls through**: a name the block does not answer reaches the rows and the generic arms below, on the `Match`
   itself. `head`, `pick`, `roll`, `pairup`, `permutations`, `combinations`, `tree` and the grammar match's `minmax` now give
   Rakudo's answers; `naive-word-wrapper` is `X::Method::NotFound`.
2. **The `Match` shape reaches `Cool`'s numeric methods** (`COOL_NUMERIC_FOR_COLLECTIONS`) and `COOL_COERCE_FOR_MATCH`
   (`Rat`, `FatRat`, `Failure`, `parse-names`, `uniparse`, `AST`), which the fallback used to serve on the string. `numify` reads a
   `Match` as its text (`Match.Numeric` is `self.Str.Numeric`), and so does `Cool.Real`.
3. **`Cool.FatRat` and `Cool.AST` are rows** (Rakudo declares both on `Cool`; the recognition table gains them). `FatRat` reads its
   receiver as `Cool.Rat` does, `AST` (an interpreter row: the parse scans `use`d modules with the interpreter's paths) parses
   the receiver's string form.
4. **`Any.end` reaches a `Match`** and counts its positional captures (`1` for two, `-1` for a grammar match with none).
5. **`list` and `Capture` are `Capture`'s rows** (`CAPTURE_FOR_MATCH`): `Capture.list` returns a `Match`'s live positional view
   (`match_list_view`), `Capture.Capture` the `Match` itself. `reaches_audited_ancestor`, the fallback's only reader, is gone.

Left in the block: the positional coercions (`Array`, `List`, `Slip`, `Seq`, `flat`, `cache`, `eager`, `reverse`) and `Hash`,
which are `Any`'s methods in Rakudo and have no `Any` row yet. Found and filed: #13160 (`+` and `.sum` read a non-numeric `Match` as
`0`), #13161 (`min`/`max`/`minmax`/`sort`/`unique`/`combinations`/`chrs` of a `Match` do not read its positional captures; they were
wrong on the string before too). `method-rows-report.py --arms`: the pure arity cascades go from 142 to 140 arms. Pinned in
`t/regex/match/match-cool-method-rows.t`.

### 9.97 Slice 5: the numeric type objects' `.Range` are rows (2026-10-13)

`refactor/11276-type-object-range`. The block after the `Match` block of `methods_0arg/mod.rs` answered `.Range` for the type objects
`Real`, `Num`, `Rational`, `Rat`, `FatRat` and `BigRat` (`-Inf..Inf`), `Int` (`-Inf^..^Inf`) and the native integer types (their
bounds) by name. Rakudo declares `Range` on `Int` (native integer types inherit it, and its body reads their bounds), on `Num` and on
the `Rational` role, which `Rat` and `FatRat` compose. Decisions:

1. **Five rows** (`scalars::numeric_range`, `TYPE_OBJECT_OK`): `Int`, `Num`, `Rat`, `FatRat` and `Rational`. The recognition table
   gains `Int`, `Num`, `Rat` (declared `only`) and `Rational` (a role, like `Positional`). `Rational`'s row is `OWNER_ONLY`: no shape is
   the role, so only its type object reaches it, as `Callable.of` does.
2. **The type-object walk (`type_object_row`) answers `Range`**: a native integer type object (`int8`) walks its catalog MRO to `Int`.
   A native integer type the catalog does not list (NativeCall's `long`) is an `Int`. A role with no catalog MRO (`Rational`) answers
   its own rows.
3. **Behaviour changes toward Rakudo**: `Real.Range` is `X::Method::NotFound` (the block answered it). An instance's `.Range`
   (`5.Range`, `2.5.Range`) throws `X::Parameter::InvalidConcreteness` naming its type, where it was `X::Method::NotFound`.

Not done: `UInt.Range` (`0..^Inf` in Rakudo) stays `X::Method::NotFound`, because the catalog gives `UInt` the MRO `(UInt Any Mu)`, not
`Int`'s (#13178). `method-rows-report.py --arms` is unchanged (the block was `if` chains, which it does not count). Pinned in
`t/types/numeric/numeric-type-object-range-rows.t`.

### 9.98 Slice 5: a `Match` answers `Any`'s `self.list.METHOD` methods on its positional captures (2026-10-13)

`refactor/11276-match-positional`, with #13161. After §9.96 the `Match` block of `methods_0arg/mod.rs` still answered the
positional coercions (`Array`, `List`, `Slip`, `Seq`, `flat`, `cache`, `eager`, `reverse`) on the positional captures and `Hash` on
the named ones, while `min`, `max`, `minmax`, `unique`, `squish`, `combinations`, `permutations`, `chrs`, `Set`/`Bag`/`Mix` and the
rest of `Any`'s list methods answered on the `Match` as one element (#13161). The interpreter path carried a twin
(`is_capture_list_method` in `call_method_with_values`) for `grep`/`map`/`first`/`head`/`tail`/`sort`/`pick`/`roll`. Decisions:

1. **One receiver rewrite, not rows.** Rakudo declares these on `Any` as `self.list.METHOD` (`Cool.chrs` too), and `Capture.list`
   is the positional captures, so the answer is the *list's* method, not an `Any` handler reading a `Match`. Both native entries
   (`try_native_method` and `call_method_with_values`) swap the receiver for its positional list
   (`Interpreter::match_list_receiver`, over `Value::match_list_receiver` and `match_view::is_capture_list_method`) before
   anything answers the call, as they already do for a user subclass of `IO::Path` (#12149) and an `Array` enum's base. A
   grammar that declares the method keeps its own (the memoized `has_user_method` probe). `Hash` reads the named captures
   (`self.hash`); for `List` and `Hash` the swapped-in value is the answer itself (`match_list_answers_itself`), so an unbound capture's hole is not read as `Nil` by `List.List`. The pure entry `native_method_0arg` applies the same rewrite (without the grammar probe), so the recognition rows `Match.Array`/`Match.Hash` stay recognized.
2. **The `Match` shape still reaches only `join`, `end` and `serial` of `Any`'s rows** (§9.96): the rewrite runs first, so no
   call-site lane caches a `Match` for the other names.
3. **`minpairs`/`maxpairs` are not in the set**: Rakudo reads them through `self.pairs`, named captures included.

The two quoted-name arms of the block are gone; what is left of it is the `match_owner_row` call for a match with no shape.
`method-rows-report.py --arms`: the pure arity cascades go from 140 to 138 arms. Pinned in
`t/regex/match/match-any-list-methods.t`. The remaining differences from Rakudo are `List`'s own over a list of `Match`es
(#13195): `minmax` and `sum` do not numify the elements, `chrs` does not refuse them, `minpairs`/`maxpairs` answer a `Seq`, and
`iterator`/`batch()`/`rotor()` differ.

### 9.99 Slice 5: the 1-arg `Rat`/`FatRat` arms are gone; `Cool.Rat` takes no epsilon (2026-10-13)

`refactor/11276-rat-epsilon-arms`. `methods_narg/dispatch_1arg.rs` kept three arms for `.Rat($epsilon)` and `.FatRat($epsilon)`:
one that re-asked an allomorph's numeric value, and one per name that answered a `Str` (parsed, then rationalized with the
epsilon), an aggregate or any other receiver (`0`), and declined a `Complex`, `Instant` and `Duration`. Every receiver Rakudo
answers already has a row: `Int`/`Num`/`Rat` (`numeric_coerce`'s `with_epsilon` rows, §9.70), `Complex` (`complex_real`),
`Instant`/`Duration` (§9.18), and an allomorph through its numeric value. Rakudo's `Cool.Rat` and `Cool.FatRat` take no epsilon
(`"1".Rat(0.1)` is "Too many positionals passed"), so the receivers only the arms answered were answering wrongly. Decisions:

1. **The three arms are deleted**, with no new row. A `Str`, a `List`, a `Match` or a `Str` type object refuses the epsilon.
   mutsu raises `X::Method::NotFound` where Rakudo raises the bind failure; that is general to every built-in method called
   with too many positionals and is filed as #13203.
2. **The recognition table follows**: `Str`, `Match` and `Cool`'s `Rat`/`FatRat` rows claim arity 0 only.
3. **A declined argument reaches an `ANY_ARGS` row** (`method_table::invoke_unadmitted`). The arms also answered a number whose
   epsilon the guard step does not admit (an allomorph `<1>`, a `Duration`), which `t/types/numeric/rat-fatrat-epsilon-real-check.t`
   pins. A row flagged `ANY_ARGS` binds its own arguments, so `call_method_with_values`, once the cascades have declined, asks the
   row the receiver's shape resolves when every declined argument is a built-in value (an allomorph, a `Duration` or an `Instant`). A user object stays the slow path's (`"3".exp(NotComplex.new)` calls its `.Numeric`; `roast/S32-num/cool-num.t` caught a first version that admitted it), as do a named argument, a `Junction` and a `Failure`. It is
   the shape counterpart of `invoke_owner`, and it serves every `ANY_ARGS` row, not only these two.

`method-rows-report.py --arms`: `dispatch_1arg.rs` goes from 29 to 27 arms (the allomorph arm was a guarded `|` arm the report
counts once with the `Rat` arm). Pinned in `t/types/coercion/coerce-rat-epsilon-rows.t`.

### 9.100 Slice 5: `Any.combinations` and `Any.batch` reach the closed shapes; three 1-arg arms are gone (2026-10-13)

`refactor/11276-shapeless-1arg`. Three arms of `methods_narg/dispatch_1arg.rs` called a row's handler "for the receivers the
table has no shape for": `combinations` (`List.combinations`' `combinations_of`), `batch` (`Any.batch`) and `round` (`Cool.round`'s
`round_to`, for an allomorph). They answered a `Seq`, a `Range`, a `Str`, a `Pair` or a quant hash, whose closed shapes did not
reach `Any`'s rows for these names, and `batch(:elems(N))`, whose named argument the arity-1 row could not take. Decisions:

1. **`Any.combinations($of)` is a row** (Rakudo declares it `only` on `Any`; the recognition table gains it), `TYPE_OBJECT_OK`
   (`Str.combinations(1)` is `(((Str)))` in Rakudo too). Its handler counts the list first, so an infinite `Range` or a lazy list
   is `X::Cannot::Lazy` where the arm hung.
2. **`combinations` and `batch` join `ANY_LIST_FOR_CLOSED_SHAPES`**: their handlers read any receiver through
   `value_to_list_for_receiver`, so every closed shape with instances reaches them, and so does a receiver with no shape
   (`shapeless_row` takes them with one argument).
3. **`Any.batch(:$elems!)` is an arity-0 `Handler::Named` row**, Rakudo's named candidate. Without `:elems` it raises
   `X::Multi::NoMatch`, as Rakudo does (`.batch()` used to be `X::Method::NotFound`).
4. **`round` needed nothing**: an allomorph already reaches `Cool.round($scale)` through its numeric value.
5. **The sub form calls the handler**: `combinations($n, $k)` asked the method entry on its list, which the guard step declines
   for an allomorph `$k` (`roast/S32-list/combinations.t`: `combinations(|<3.5 2e0>)`); it now calls `combinations_of`, the
   rows' one implementation. The handler reads an itemized `$k` (`my $k = 1..2`) as its value, and `invoke_unadmitted` (§9.99)
   hands an `ANY_ARGS` row an itemized plain argument without its `Scalar`, so `$seq.combinations($k)` answers too.

`method-rows-report.py --arms`: `dispatch_1arg.rs` goes from 27 to 24 arms. Pinned in `t/types/any-combinations-batch-rows.t`.

### 9.101 Slice 5: an allomorph is answered by the rows, on the part each reads (2026-10-13)

`refactor/11276-allomorph-shape`. An allomorph (`IntStr`, `RatStr`, `NumStr`, `ComplexStr`) is its number with the source string
mixed in (`Value::Mixin`), which has no dispatch shape, so the `Mixin` block of `methods_0arg/mod.rs` answered it by name: a ~30-name
list of string methods on the `Str` part, then every other name on the number (`native_method_0arg(inner, ...)`), and the 1-arg
cascade kept a twin for `uniprop`/`uniprops`/`unimatch`/`samemark`/`samecase`/`indent`. Probed: every name declared on `Str`, `Int`,
`Rat`, `Num`, `Complex`, `Cool`, `Allomorph`, `Any`, `Mu`, `Real`, `Numeric`, `Stringy` and `Rational` (283 after dropping the I/O and
effectful ones), with no argument, on `<1>`, `<1.5>`, `<1e0>`, `<1+2i>`, `IntStr.new(5, "zero")` and `<0x10>`, plus 45 one-argument
calls, on `raku`, the pre-change binary and the new one. Decisions:

1. **No new shape: a receiver redirect** (`method_table::allomorph`, called by the guard step after the native-base redirect of
   §9.79). Rakudo's MRO is `IntStr Allomorph Str Int Cool Any Mu`, so the row is looked up along it: `Str`'s own rows first, invoked on
   the string part; then the numeric type's shape (open, so it reaches `Cool`, `Any` and `Mu`), whose own rows (`Int`, `Real`,
   `Rational`, `Num`, `Complex`, `Numeric`) are invoked on the number. A `Cool` row reads the number when it is one of `Cool`'s
   numeric methods (`self.Numeric.METHOD`, `COOL_NUMERIC_FOR_COLLECTIONS`) and the string otherwise (`self.Str.METHOD`); an `Any`
   row in `ANY_LIST_FOR_CLOSED_SHAPES` reads the allomorph itself as its one element; a `Mu` row declines. `Allomorph`'s own
   numeric overrides (`Bool`, the numeric coercions, `Bridge`, `succ`, `pred`, `narrow`) skip `Str`'s rows.
2. **What an allomorph answers about itself stays in the block**: `raku`/`perl`, `gist`, `Str`, `Stringy`, `WHICH`, `clone`,
   `Capture`, `VAR`, `wordcase` (Rakudo re-allomorphs its result) and `ACCEPTS` (a `Str` argument compares the string, anything
   else the number: `methods_narg::allomorph`). These are `Allomorph`'s methods, which neither part's row can answer.
3. **Only a plain allomorph is redirected**: one carrying a role (`<42> but R`) or a value mixed with a `Str` (`5 but "x"`) keeps the
   mixin dispatch.
4. **The cascades decline what the table answers** (`allomorph::answers`, asked at the top of the `Mixin` block and in the 1-arg
   cascade), so the debug cross-check compares a row with a declining cascade, as for every migrated name.
5. **`Any.join` reads an allomorph** as the one element it is, stringified to its string part (`<1e0>.join` is `"1e0"`; it was `"1"`).

Behaviour changes toward Rakudo: `Any`'s list methods (`head`, `tail`, `pick`, `roll`, `min`, `max`, `minmax`, `sort`, `serial`,
`unique`) keep the allomorph where they answered the bare number; `Str.Version` parses the string (`<1e0>.Version` is `v1.e.0`, was
`v1`); `Str.Date`/`DateTime` parse the string (`X::Temporal::InvalidFormat`, was `X::Method::NotFound`); `naive-word-wrapper` answers.
Not changed: a numeric row that returns its invocant answers the bare number where Rakudo returns the allomorph (`<1>.floor`,
`<1.5>.abs`, #13236); `.unique` on any scalar answers the scalar, not a one-element `Seq` (#13235). `method-rows-report.py --arms`:
the pure arity cascades go from 133 to 132 arms (the string list was one `match` arm). Pinned in `t/oo/method/allomorph-method-rows.t`.

### 9.102 Slice 5: `words($limit)`, `lines($limit)` and `chop($n)` are rows (2026-10-13)

`refactor/11276-text-count-args`, stacked on §9.101. `methods_narg/dispatch_1arg.rs` answered `chop($n)`, `lines($limit)`,
`lines(:chomp)`/`lines(:count)` and `words($limit)` for any receiver by its string form. Rakudo declares each on `Str` and again on
`Cool` (`self.Str.METHOD`). Probed: the three names on thirteen receivers (`Str`s, `Int`, `Rat`, a `List`, an `Array`, an allomorph,
a `Range`, the `Str` type object, a `Pair`, a `Hash`, an `IO::Path`) with eleven arguments each, on `raku`, the pre-change binary and the
new one. Decisions:

1. **Rows on `Str` and `Cool`**: `words` at arity 1, `lines` at arity 0 (now `Handler::Named`, declaring `chomp` and `count`) and 1
   (declaring `chomp`), and `chop` at arity 1. The arity-1 rows are `ANY_ARGS`. A receiver with no shape reaches them through
   `shapeless_row`, which now takes these three names with one argument.
2. **The count is read as `.Int`** (`value::str_iter::parse_limit`, the one reader, and `chop`'s own): a `Str` parses, a `Rat`
   truncates, an allomorph reads its number, a negative count is `0`; `*` and `Inf` cap nothing. Before, a `Rat`, `Str` or allomorph
   limit was `X::Method::NotFound`.
3. **An allomorph receiver with an argument the guard step declines** (`<1e0>.words(<1>)`) is answered by `invoke_unadmitted`, which now
   redirects an allomorph as the guard step does (§9.101); the mixin dispatch asks it before delegating the call to the number, which
   answered `("1",)`.

Behaviour changes toward Rakudo: a `Pair` (not a `Cool`) refuses `words`/`lines`/`chop` with `X::Method::NotFound` (they answered on its
`.Str`); the limits above. Not changed: `chop(*)`/`chop(Inf)` answer where Rakudo raises a bind failure, and a positional `Pair` argument
is read as a named one (ADR-0021). `method-rows-report.py --arms`: `dispatch_1arg.rs` goes from 24 to 21 arms. Pinned in
`t/types/string/str-count-arg-rows.t`.

### 9.103 Slice 5: the 1-arg `parse-base` and `in-range` arms are gone (2026-10-13)

`refactor/11276-parse-base-in-range`, stacked on §9.102. Both arms called a row's handler (`str_args::parse_base`,
`range::in_range_value`) for the receivers the table did not reach. After §9.101 an allomorph reaches `Str.parse-base` on its string,
and `Range.in-range` is `ANY_ARGS`, so `invoke_unadmitted` hands it the arguments the guard step declines; probed on twelve receivers
and two radixes against `raku`, only `parse-base` needed more:

1. **`Str.parse-base` is `ANY_ARGS` and `TYPE_OBJECT_OK`**: an `IntStr` radix binds `Int:D` (`"ff".parse-base(<16>)` is 255; it was
   `X::Multi::NoMatch`), and the `Str` type object raises Rakudo's `Cannot resolve caller parse-base(Str:U: Int:D)` (the arm answered
   `X::Str::Numeric`). Any other argument is the bind failure the handler already raised.

`method-rows-report.py --arms`: `dispatch_1arg.rs` goes from 21 to 19 arms. Pinned in `t/types/string/str-parse-base-rows.t` and
`t/collections/range-pair/range-in-range-rows.t`.

### 9.104 Slice 5: `flat($levels)`, `Int.Str(:superscript)`, `Mu.isa` and `FatRat.base` are rows; five 1-arg arms are gone (2026-10-13)

`refactor/11276-dispatch-1arg-rest`, one commit per family, each probed on receivers × arguments against `raku` and the binary
before the slice:

1. **`flat($levels, :hammer)`** is a `Named` row on `List`, `Array`, `Seq` and `Map` (Rakudo declares it on `Iterable` and again
   on `Array`; `Any` and `Range` declare only `flat()`). A negative level, `Inf` and `*` flatten every level, a `Str` level
   numifies (a non-numeric one dies), an `Array` without `:hammer` never reads the level, and a `Hash` flattens as its pairs (also
   for `flat(:hammer)`, which answered `({...},)`). `5.flat(1)`, `"ab".flat(1)` and `(1..3).flat(1)` now die as in Rakudo (the
   arm answered a one-element list). The 2-arg `flat` arm of `dispatch_2arg.rs` goes too. A `Slip` or an unsettled `Seq` reaches
   the row through `answer_shapeless`, which now **splits named arguments for a row that declares them** and is asked for 2-arg
   calls as well (both callers), so `$slip.flat(1, :hammer)` binds `:hammer` instead of reading it as a second positional.
2. **`Int.Str(:superscript, :subscript)`** is a named row (a big `Int` too; `:superscript` wins). Every `Str` row takes `*%_`
   (`ANY_NAMED`), as Rakudo's candidates do, so `1e3.Str(:superscript)`, `True.Str(:subscript)` and `"12".Str(:superscript)`
   ignore the adverb (the arm rendered the numified receiver raised). `<5>.Str(:superscript)` still answers the `Int` part (#13259).
3. **`zprintf` has no method form.** Rakudo declares neither the method nor the routine; the routine is a mutsu extension with its
   own tests and stays, but the 1-arg cascade arm and the runtime's by-name arm (`methods_dispatch_match.rs`) are deleted.
4. **`Mu.isa`** is a row (`ctors_mop/mu_isa.rs`), reached by every shape through its owners or `MU_FOR_CLOSED_SHAPES` and by a
   shapeless receiver through `answer_shapeless`. An instance, a type object and a mixin over an instance decline to the
   interpreter (user hierarchies, roles, subsets). No behaviour change (`Int.isa(3)` stays wrong: #13260).
5. **`FatRat` has the `base` rows** (Rakudo declares `base` on `Rational`); the 1-arg `base` arm, which re-called the rows'
   implementation, is deleted. A user object as the radix now has no candidate (`X::Method::NotFound`; Rakudo raises
   `X::Multi::NoMatch`) where the arm answered a `Failure`. `FatRat.base-repeating` is still missing (#13258).

**Not done, and why.** The `ACCEPTS` arms of the quant hashes, `Pair` and `Range` and the `AT-POS`/`EXISTS-POS` arms of `Uni` and
`Capture` re-call their rows' handlers, but they also answer an argument the guard step does not admit (a user object): deleting
them turned `set(1,2).ACCEPTS(P.new)` from `False` into `X::Method::NotFound`, and `\(1,2).AT-POS(1..2)` from an error into `Nil`
(the generic `AT-POS` arm claims it). They go when a row can bind a user-object argument (an `Interp` handler that calls its
`.Numeric`). `subbuf` and the `read-*` arms serve a user class composing a buffer role over a `VMArray` body, which has no shape;
they go with the `Blob`/`Buf` user-instance mechanism (the same one `methods_narg/buf.rs`'s ten arms need).

`method-rows-report.py --arms`: pure cascades 127 -> 121 arms (`dispatch_1arg.rs` 19 -> 13; `dispatch_2arg.rs` loses `flat`).
Pinned in `t/oo/method/{flat-levels,int-str-script,mu-isa,fatrat-base}-method-rows.t` and `t/oo/method/zprintf-no-method.t`.

### 9.105 Slice 5: a positional index and an `ACCEPTS` topic bind like Rakudo's candidates; four 1-arg arms are gone (2026-10-13)

`refactor/11276-user-object-args`. The `AT-POS`/`EXISTS-POS` arms of `Uni` and `Capture` and the `ACCEPTS` arms of the quant
hashes, `Pair` and `Range` in `methods_narg/dispatch_1arg.rs` only re-called their rows' handlers, but they also answered the
arguments the guard step does not admit (a user object, a collection), so deleting them needed a way for a row to bind such an
argument (§9.104, "Not done"). Probed: seven receivers × nine arguments (a user object, one with `.Int`/`.Numeric`/`.Str`, a type
object, a `Range`, an `Int`, a `Str`, a `Set`, a `List`, a `Hash`) for the three names, then the edge cases (`Bool`, an enum, an
`Int` subclass, a negative index, `Nil`), on `raku`, the binary before the slice and the new one. Decisions:

1. **`RowFlags::INDEX_ARG`: a positional index is bound by Rakudo's candidates' rules** (`method_table::index_arg`). Rakudo's
   `Range`, `Uni` and `List` declare `AT-POS`/`EXISTS-POS` for `Int:D` only; any other index reaches `Any`'s candidates, where
   `AT-POS(Any:D \pos)` re-dispatches on `pos.Int`, `EXISTS-POS(Any:D \pos)` is `pos.Int == 0` (a non-`Int` index addresses the
   one-element list `Any` is: `(1, 2).EXISTS-POS("1")` is `False`), and a type object is `Cannot use 'T' as an index`. `Capture`
   declares `Int(Any) $pos` instead (`RowFlags::INDEX_COERCES`): everything is coerced, a type object to `0` with the warning. The
   guard step admits only an `Int` to such a row; `invoke_unadmitted` binds every other argument, calling its `.Int` through the
   interpreter (user code for a user object). A `Bool`, an `Int`-valued enum and an instance of an `Int` subclass are `Int:D` and
   bind the row's own candidate. The flag is on the `Range` and `Uni` rows of both names, `Capture`'s, and `List.EXISTS-POS`;
   `List.AT-POS`/`Array.AT-POS` stay `builtin_at_pos`'s (the subscript opcode's own binding). The cascade declines a call such a
   row resolves (`index_arg::binds`), so the generic `AT-POS` arm no longer answers `\(1, 2).AT-POS(1..2)` with `Nil`.
2. **`RowFlags::OBJECT_ARGS`: a row that binds `Mu \topic`** takes any argument from `invoke_unadmitted` but a `Junction`, a
   `Failure` and a named argument. The `ACCEPTS` rows of the six quant hashes, `Pair` and `Range` are now `Handler::Interp` rows
   with it (`collections/accepts.rs`): a quant hash coerces a topic of another kind to its own (`.Set`/`.Bag`/`.Mix`, so
   `set(1, 2).ACCEPTS(1..2)` and `bag(1, 2).ACCEPTS(set(1, 2))` are `True`; both were `False`); `Pair` follows
   `Type/Pair.rakudoc` (an `Associative` topic is looked up and smartmatched, any other topic is called the method the key names
   and the truths compared, so `(abs => 5).ACCEPTS(-5)` is `True` and `(a => 1).ACCEPTS(5)` is `X::Method::NotFound`; the arm
   compared values with `==`); `Range` compares a user object with the ends by `cmp` on its `.Stringy`, as Rakudo's
   `(topic cmp $!min)` does. The runtime's `Pair.ACCEPTS` arm for an instance topic (`methods_call_dispatch.rs`) is deleted too.

Not changed: `Set` has no `Int`/`Numeric` (#13281), so `(1..5).AT-POS(set(1, 2))` stays `X::Method::NotFound` (Rakudo: `3`) and
`(1..5).ACCEPTS(set(1, 2))` `False` (Rakudo: `True`); `Capture.ACCEPTS` and `Uni.ACCEPTS` are still missing (#13282); a positional
`Pair` argument is read as a named one (ADR-0021), so `(a => 1).ACCEPTS(a => 1)` is `X::Method::NotFound` where Rakudo's is
`X::Multi::NoMatch` (it answered `True`); `\(1, 2, 3).AT-POS("x")` raises `X::Str::Numeric` where Rakudo fails to unbox.
`method-rows-report.py --arms`: pure cascades 121 -> 116 arms (`dispatch_1arg.rs` 13 -> 8; the report counts the three guarded
`ACCEPTS` arms once). Pinned in `t/collections/subscript/at-pos-index-arg-rows.t` and `t/oo/method/accepts-method-rows.t`.

### 9.106 Slice 5: `Any`'s subscript protocol is a row (2026-10-13)

`refactor/11276-any-subscript-rows`, on §9.105's `INDEX_ARG`. The generic `AT-POS`, `EXISTS-POS`, `AT-KEY` and `EXISTS-KEY` arms
of `methods_narg/dispatch_1arg.rs` answered every receiver no row took: a `Str`, a `Match`, an instance, a value that is not
`Positional` (`is_one_element_under_positional_subscript`), the `Any`/`Mu` type objects and any other value failing the associative
subscript. Probed: forty receiver × argument pairs (scalars, type objects, user instances and their type objects, `Hash`, `Set`,
`Pair`, a `Match`, a lazy `Seq`, a shaped array, `Nil`, a buffer, `IO::Path::Parts`) on `raku`, the binary before the slice and the
new one. Decisions:

1. **Four `Any` rows** (`collections/any_subscript.rs`, `TYPE_OBJECT_OK`; the recognition table gains them): `AT-POS(Int:D)` is the
   receiver at 0 and an `X::OutOfRange` failure (`0..0`) elsewhere; `EXISTS-POS(Int:D)` is `pos == 0` for a defined receiver and
   `False` for a type object; `AT-KEY` fails the associative subscript for a defined receiver (naming an instance's class) and is
   `Any` for a type object; `EXISTS-KEY` is `False`. The positional pair is `INDEX_ARG`, so `index_arg` binds any other index, and
   now also for a receiver with no shape (through `shapeless_row`) and a type object (`Int.EXISTS-POS("0")` is `False`).
2. **Reach**: the open shapes by their MRO; the closed ones through `ANY_LIST_FOR_CLOSED_SHAPES` (a `Hash`, a quant hash, a `Pair`
   are not `Positional`), except `Blob`/`Buf`, which are and keep their cascade arms until their rows land; a receiver with no shape
   through `answer_shapeless` (an instance by its MRO, which declines at a built-in class such as `Map` or `Array`; a type object; an
   enum; a code object). `answer_shapeless` leaves to the interpreter a list with no settled shape (a lazy or shaped array, a `Seq`,
   a `Slip`, a buffer storage) for the positional pair, a `Junction`, a `Proxy`, `Nil` and a mixin for all four, and for the
   associative pair a package (its stash is `Associative`: `CORE::<&say>`), an instance (its `{ }` may be a user
   `postcircumfix:<{ }>` or a tied store, resolved first) and a hash with no settled shape (a `Map` subclass's raw storage, which
   keeps the associative rows' implementation in the cascade).
3. **A `Match` reaches `Capture`'s `AT-POS`/`EXISTS-POS`** (`CAPTURE_FOR_MATCH`): the handlers read its positional captures, and an
   optional group that did not match does not exist. The `Match` branch of the cascade arm goes.
4. **The associative rows of the collections take a user-object key** (`OBJECT_ARGS` on `subscript.rs`'s rows); their delegation
   from the cascade arm goes for every settled shape.
5. **The subscript opcode keeps the cascade's instance branch of `AT-POS`** (`$obj[0]` of a `Positional` class with no `AT-POS`),
   which now calls the row's handler: the opcode asks the cascade without an interpreter, so it cannot walk the instance's MRO to
   the row (a `TODO` in the arm).

Behaviour changes toward Rakudo: `Int.AT-POS(0)`, `C.AT-POS(0)`, `Int.EXISTS-POS(0)`, `Int.AT-KEY("x")` and
`C.new.EXISTS-POS(0)` answer (all were `X::Method::NotFound`); `5.AT-POS(N.new)`
reads the index's `.Int` (was `Nil`); `5.EXISTS-POS("0")` is `True`; `5.AT-POS(-1)` names `0..0`. Not changed: `Nil.EXISTS-POS` and
`Nil.EXISTS-KEY` answer `Nil` (Rakudo: `False`; the interpreter answers a method on `Nil` before the table); `Buf.EXISTS-POS` is still
missing (#13294, with the `Nil` answers); `C.AT-KEY("x")`, `C.new.AT-KEY("x")` and `C.new.EXISTS-KEY("x")` stay
`X::Method::NotFound` (Rakudo: `Any`, a failure, `False`; point 2); `%h.AT-KEY($obj)` keys a `Str`-keyed hash by the object's own string form, not its `.Str` (#13293). The arms stay
counted by `method-rows-report.py`: what is left of them is the list, buffer-instance, `IO::Path::Parts` and `Nil` branches. Pinned in
`t/collections/subscript/any-subscript-rows.t`.

### 9.107 Slice 5: a user class over a buffer reaches the `Blob`/`Buf` rows; the buffer arms are gone (2026-10-13)

`refactor/11276-buffer-instance-rows`. `subbuf`, the `read-*` arms (1- and 2-arg), `read-ubits`/`read-bits`, `subbuf-rw`, the
buffer branch of the `AT-POS` arm and `methods_narg/buf.rs`'s name table (`read_int_method_info`, ten quoted names) served a
receiver no row took: a user class composing a buffer role over a `VMArray` body (`class B does Blob[uint8] is repr('VMArray')`,
#9438), which has no shape. Probed: six receivers (two user classes, `Buf`, `buf8`, `blob8`, `utf8`) × thirty-eight calls, then
each method's argument edge cases, on `raku`, the binary before the slice and the new one. Decisions:

1. **`Receiver::of_settled` answers such an instance with the shape of the buffer type it composes**
   (`blob::user_buffer_receiver`: the buffer-class registry names `Blob[..]` or `Buf[..]`), and so does a built-in buffer under
   an alias class name, which has no shape either (`blob8.allocate(20)` is a `blob8`: OpenSSL's `sha1` digests, which lost
   `.raku`/`.bytes` when the cascade's buffer branches went; the names are matched exactly, not by the `buf`/`blob` prefix
   predicates, so a user `class buffer` is not a buffer). The instance stays the invocant;
   every handler keeps its class (`B.new(1, 2).subbuf(1)` is a `B`). The entries that read `of_settled` run after the user's own
   methods were looked up, so a method the class declares still answers first; the call-site lane reads `Receiver::of` and does
   not see it. Every `Blob`/`Buf` row now answers it: `Str` raises `X::Buf::AsStr` (it rendered the gist), `of`, `Blob`/`Buf`
   and `join` answer (all were `X::Method::NotFound` or the gist).
2. **`read-ubits`/`read-bits` are rows** on the `buf_bits::read_bits` implementation (the cascade's second copy is deleted, and
   so is the runtime's by-name arm in `methods_mut_dispatch.rs`); they bind `int $pos, uint $bits` and raise Rakudo's messages
   (`Can only read 1..N bits from position P in buffer, you tried: B`, without the receiver's variable name, which a method does
   not know). The `read-*` rows take the width and signedness from their table (`Reads`), bind `int $offset` (a `Str`, `Num` or
   `Rat` offset, a type object and a big integer raise Rakudo's unboxing errors; all were read through `.Int` or declined) and
   `Endian $endian` (an `Int` is `X::TypeCheck::Binding::Parameter`; it was read as the enum's value), and report a negative
   offset as given (`offset -1`; the message read `offset 0`).
3. **`subbuf` binds Rakudo's candidates**: `Range:D` (integer bounds, else `Must specify a Range with integer bounds to subbuf`;
   `1^..^4` excludes both ends, it answered the whole buffer, and `1..*` panicked), `Int:D`, `(Int:D, Int:D)`, `(\from,
   Whatever)`, `(\from, Numeric)` (truncated), `&From`/`&End`, and `X::Multi::NoMatch` for anything else (a `Str` or `Rat`
   start was read as a number); the out-of-range messages are Rakudo's (`"From argument to subbuf ...`, `Len element to subbuf
   ...`). The rows are `Handler::Interp`: a `Callable` bound is called with the length by the handler
   (`resolve_subbuf_callable_args`, no longer called by the two native entries before the cascade). The runtime's
   `dispatch_subbuf` (a third copy, which clamped a negative start) is deleted.
4. **`subbuf-rw` is `Buf`'s** (rows at arities 0-2 for its rvalue; the lvalue proxy is unchanged and now resolves a `Callable`
   end as `subbuf`'s end index, not `substr`'s length): `Blob.new.subbuf-rw(...)` and `utf8`'s are `X::Method::NotFound` as in
   Rakudo (the cascade arm answered them), `Buf.subbuf-rw(1)` and `subbuf-rw()` answer (they were `X::Method::NotFound`).
5. **`AT-POS`/`EXISTS-POS` rows on `Blob` and `Buf`** (`blob_pos.rs`, `INDEX_ARG`), and `reaches_audited_any_method` no longer
   withholds `Any`'s subscript rows from the two shapes, so a buffer type object answers `Any`'s (`Buf.AT-POS(0)` is `Buf`). A
   `Buf` reads `0` past its end, a `Blob` (and `utf8`, `blob16`) raises `X::OutOfRange` (it read `0`), a negative position is
   out of range `0..elems-1` (it named `0..^Inf`), a big integer raises the unboxing error, and `EXISTS-POS` answers (it was
   missing, #13294's `Buf` half). The cascade's `AT-POS` branch keeps the native-backed `CArray` only.
6. **`.Blob`/`.Buf` keep the element type** (`buf16.new(1).Blob` is a `Blob[uint16]`; it was a `Blob`, #13115), and refuse a
   type object with `X::Parameter::InvalidConcreteness` (they were `X::Method::NotFound`). For that, `user_buffer_receiver` also
   answers a buffer type object with no shape of its own (`buf16`, `Blob[uint32]`, a user buffer class) with its shape's type
   object, so it reaches the `TYPE_OBJECT_OK` rows (`of`, the coercions, `Any`'s and `Mu`'s).
7. The 0-arg cascade's buffer branches for `gist`/`raku`, `bytes` and `list` are deleted (the rows answer every buffer now);
   `perl` (Mu's, which Rakudo's `Blob` does not declare) and `values` (`Any`'s `self.list`) keep a one-line branch each.

Not changed: `Blob.new(1, 2)[5]` (the subscript opcode's own read) answers `0` where Rakudo raises `X::OutOfRange`; a buffer
answers `contains`/`starts-with` and the method form of `unpack` where Rakudo has none (see the PR's findings).
`method-rows-report.py --arms`: pure cascades 116 -> 102 arms (`buf.rs` 10 -> 0, `dispatch_1arg.rs` 8 -> 7, `dispatch_2arg.rs`
7 -> 4), runtime slow path 542 -> 541. Pinned in `t/oo/method/buffer-instance-method-rows.t`.

### 9.108 Slice 5: `ACCEPTS` of the open shapes, `Uni`, the buffers, `Match` and `Any` are rows (2026-10-13)

`refactor/13282-accepts-rows`, #13282. The runtime's generic `Mu.ACCEPTS` arm (`methods_call_dispatch.rs`) answered
`$x.ACCEPTS($y)` with `$y ~~ $x` for the plain scalars and the buffers only; every other receiver without a row of its own (a
`List`, a `Hash`, a `Date`, a `Duration`, a `FatRat`, a `Capture`, a `Uni`, a `Seq`, a `Match`, a user instance) was
`X::Method::NotFound`. Probed: 28 receivers × 20 topics, then the edge cases (junction topics, type objects, user `ACCEPTS` in a
class and a role, subclasses), on `raku` and the new binary; Rakudo's owners read from
`.^find_method('ACCEPTS').candidates`. Decisions:

1. **Rows on the owners Rakudo declares**, in `collections/accepts_open.rs`: `Numeric` (a role, added to `ROLES_WITH_ROWS`: it
   reaches `Int`, `Num`, `Rat`, `FatRat`, `Complex`, `Duration` and `Instant`; Rakudo's `Int.ACCEPTS(Int:D)` agrees with it),
   `Str`, `List` (and `Array`), `Map` (and `Hash`), `Date`, `DateTime`, `Seq` and `IO::Path` answer the smartmatch operator
   (`$topic ~~ self`, the one implementation of both); `Bool` is the receiver whatever the topic; `Uni` compares codepoints,
   whatever the normalization form (`"ab".NFC.ACCEPTS("ab".NFKC)` is `True`, a `Str` topic `False`); `Blob`/`Buf` compare a
   buffer topic's elements and are `False` for anything else; `Match` is the match itself (`Match:D: Mu $`). All bind
   `Mu \topic` (`OBJECT_ARGS`), so a `Junction` topic still autothreads.
2. **`Any.ACCEPTS` is `self === topic`** (`values_identical`), reaching a `Capture` through `ANY_LIST_FOR_CLOSED_SHAPES` (only
   it: `reaches_audited_any_method` withholds `ACCEPTS` from the other closed shapes) and an instance of a user class through
   `answer_shapeless`, which admits `ACCEPTS` at arity 1 for an `Instance` alone (a code object, a regex, an enum and a mixin
   keep the interpreter's). A class or role that declares `ACCEPTS` still answers first. `Capture` and `Uni` do not delegate to
   the operator, which disagrees with Rakudo there (`1 ~~ \(1)` is `True` in mutsu).
3. **The recognition table** gains the rows; `Map.ACCEPTS` and `Any.ACCEPTS` are `SPECIAL` (a `Map` has no shape, and a type
   object's `ACCEPTS` is `Mu`'s type check, which the runtime's `Package` arm answers), so the cascade-backing tests do not
   expect a pure answer for their samples.
4. The runtime's scalar/buffer `Mu.ACCEPTS` arm is deleted.

Not changed: the operator's own disagreements with Rakudo, which the rows now share (`Duration` against a number, a numeric
`Str` against a `Complex`, a `Hash` against a `Map`, a `Str` against a buffer; #13332); an enum value, an `Exception`
instance and a subclass of a built-in class with its own row (`class DD is Date`) (#13333); the allomorphs keep
`allomorph.rs`'s path. Pinned in `t/oo/method/accepts-method-open-rows.t`.

Addendum (#13333): an instance whose class MRO ends at a built-in class (`class X2 is Exception`) reaches `Any`'s rows through
the catalog chain of that class (the MRO spells out `Exception` but not `Any`), and a subclass of a built-in class with its
own `ACCEPTS` row (`class DD is Date`) answers from that row, with the interpreter (`shapeless_row`). Pinned in
`t/oo/method/accepts-method-builtin-instances.t`.

### 9.109 Slice 5: `Any`'s views and list methods answer the collection arms' receivers; `methods_0arg/collection.rs` has no arm left (2026-10-15)

`refactor/11276-any-kv-view-arms`. The arms of `methods_0arg/collection.rs` (`hash`, `keys`, `values`, `kv`, `pairs`, `pairup`,
`antipairs`, `kxxv`, `invert`, `sum`, `permutations`, `combinations`, `cache`) are deleted; the file keeps only the helpers rows call
(`invert_value`, `combinations_seq`, `permutations_seq`). Before deleting them, a census build logged every call an arm answered over
the whole `t/` suite and the whitelisted roast: `Range` (all five views, `permutations`, `combinations`, `hash`), a `Seq` with no
settled shape (`cache`, `sum`, `permutations`, the views, `invert`), `Pair.hash`, shaped arrays, `Nil`, a stash's `kv`/`pairs`/`hash`,
an enum value's `kv`, `IO::Path`/`Blob`/a user instance's `pairs`, `List`/`Array`/`Hash` `cache`/`pairup`/`permutations`, a non-`Pair`
`List.invert`, and type objects (`hash`, `pairup`). Each now reaches a row:

1. **The rows carry the arms' bodies** (`method_table::collections::any_views`): `Any`'s `keys`, `values`, `kv`, `pairs`,
   `antipairs` and `invert` (formerly `Pure` handlers asserting a scalar receiver) answer every receiver the arm did, delegating to
   the `Map`/`Pair`/`List`/quant-hash handlers for those kinds; `Any.pairup`, `Any.cache`, `Any.permutations` and
   `Any.combinations` are new rows and take a type object, which lists as itself (`Array.cache` is `((Array))`). `Any.hash` takes
   the arm's type-object, `Nil`, `Hash`, quant-hash and stash cases. `List.invert` uses `Any.invert`'s handler, so a non-`Pair`
   element is the `X::TypeCheck` the arm raised.
2. **Closed shapes** reach them through `ANY_LIST_FOR_CLOSED_SHAPES` (eleven names added); a shape with its own row (`Pair.keys`,
   `Capture.hash`, `Bag.invert`, `Range.sum`, `Seq.cache`) answers it first, and a `Match` still reaches `Capture`'s.
3. **Shapeless receivers** reach them through `answer_shapeless`, as in §9.80; a stash reaches `Any`'s `kv`, `pairs` and `hash`
   whatever its MRO says.

Behaviour changes toward Rakudo, pinned in `t/oo/method/any-view-method-rows.t`: `Set.kxxv` and `Hash.kxxv` are
`X::Method::NotFound` (Rakudo declares `kxxv` on `Baggy` alone); `"a".invert` and `(1..2).invert` are `X::TypeCheck` (were
`X::Method::NotFound`; the recognition table gains `Any.invert`); `"x".IO.hash` is `X::Hash::Store::OddNumber` (was
`X::Method::NotFound`). `method-rows-report.py --arms`: the pure arity cascades go from 108 arms to 92.

### 9.110 Slice 5: `Any`'s list coercions, `Mu.Capture` and the type objects' coercions; `methods_0arg/coercion.rs` keeps only the zen slice (2026-10-15)

`refactor/11276-coercion-arms`. The arms of `methods_0arg/coercion.rs` for `List`, `list`, `Array`, `Slip`, `Supply`, `Seq`,
`Capture`, `Pair`, `Complex`, `isNaN` and `Range` are deleted, and the `List` arm of `dispatch_capture`. A census build (as
in §9.109) found the receivers they answered: a `Range` (`List`, `Array`, `Slip`, `Supply`), an unsettled `Seq` (`List`, `list`), a
`Supply` instance (`list`), `Int`/`Str` and the buffers, a lazy list, the quant hashes, a `Pair`, a stash, and type objects
(`Int.List`, `Pair.Pair`, `Int.Complex`, `Int.isNaN`, `Any.Capture`). Each now reaches a row:

1. **`Any`'s `List`, `list`, `Array`, `Slip` and `Supply`** (`method_table::collections::any_coerce`) carry the arms'
   bodies and take a type object (`Int.List` is `((Int))`). A `Capture` is `self.list`, its positionals, which moves
   `\(1, 2, :a(3)).Array`/`.Slip`/`.Supply` to Rakudo's answers. `Any.Supply` is `RANDOM` (a new `Supply` has an identity of its
   own) and reads a hash as its pairs. `Seq` is **not** a row: a row answered `@a.Seq` at the call-site lane and lost the
   array's element containers (`@a.Seq.map({ $_ = 5 })` must write through, `t/control/immutable-topic-receiver-oracle.t`,
   red in CI's release run only), so the arm's one case (a `.lazy` list stays lazy) moved into the shared
   `seq_coerce::to_seq_structural` that the interpreter and the VM already call.
2. **`Mu.Capture`** is a row (`value_to_capture`), reaching the closed shapes through `MU_FOR_CLOSED_SHAPES`; `Any.Capture` is
   `\()`. The capture-less scalars' `Capture` (`Int`, `Num`, `Str`, `Version`) take their type objects (`X::Cannot::Capture`).
3. **Type objects**: `Pair.Pair`, `Int.isNaN` (so `Bool.isNaN`), the numbers' `Complex` and `Cool.Complex` take them
   (`numeric_coerce::complex_of_type_object`, the arm's body); `Cool.Complex` also answers a lazy list's `X::Cannot::Lazy`.
4. **An instance of a built-in class with no shape** (`Supply`) answers from its own class's row in `answer_shapeless`
   (`Supply.list`); a stash reaches `Any.list`.

Behaviour changes toward Rakudo, pinned in `t/oo/method/any-coerce-method-rows.t`: `"a".isNaN` is `X::Method::NotFound` (was
`False`); `"abc".Range`, `[1, 2].Range` and `(1..3).Range` are `X::Method::NotFound` (Rakudo declares `Range` only on the
numbers); `Any.Capture` is `\()` (was `\(Any)`); `Int.Capture` is `X::Cannot::Capture`. Left: `Uni.List` answers `(Uni)` where
Rakudo reads `self.list` (the codepoints); a class type object's `Capture` answers `\(Foo)` where Rakudo cannot look up its
attributes. `__mutsu_zen_angle` (the `<>` zen slice) stays: it is an internal name, not a method, and belongs to the compiler.
`method-rows-report.py --arms`: the pure arity cascades go from 92 arms to 76.

### 9.111 Slice 5: the prologue of `methods_0arg/mod.rs` answers through rows and receiver adapters (2026-10-16)

`refactor/11276-0arg-prologue-arms`. Twenty quoted-name arms of `methods_0arg/mod.rs`'s receiver-specific prologue are
deleted. The census build of §9.109 (every arm logging the receiver it answered, over all of `t/` and the whitelisted roast)
found which receivers still reached each; each now answers through a row or through a receiver adapter that existed for
another kind:

1. **`Unicode.version` and `Unicode.NFG`** are rows of a new type-object-only shape, `DispatchShape::Unicode` (as the
   `IO::Spec` shapes are); the recognition table and the Rakudo snapshot gain the owner.
2. **`List.from` and `List.to`** are rows (Rakudo declares both on `List`); they ask the first or last element through
   ordinary dispatch, so an empty list answers `Nil` (was `0`) and `.pos`/`.ast` on a list are `X::Method::NotFound`.
3. **A `Backtrace` and a `Capture` answer `Any`'s `self.list.METHOD` methods on their own list**, through the adapter that
   hands a `Match`'s positional captures to them (`Value::match_list_receiver`, §9.98): a backtrace's frames, a capture's
   positionals (and `Hash` its named part). Any's audited list rows no longer reach the `Backtrace` shape: they read it as a
   one-element list, so `$bt.List.elems` was `1` and `$bt.Seq` a `List`. `seq_coerce::to_seq_structural` gains the
   `Capture` case it lacked, so `\(1, 2, :a(3)).Seq` is `(1, 2).Seq` on every path.
4. **An exception's `line`, `file` and `filename`** are its attributes, which the generic accessor answers; the arm hid
   that a typed parse error raised inside an `EVAL` (`X::Syntax::Confused`, whose exception is built lazily from its
   message) never got the `EVAL` unit's name. The `EVAL` backfill now records it on the error
   (`RuntimeErrorCold::source_file`), and the exception built from it reports `filename` and `file`.
5. **A user subclass of `Pair` or of a quant hash** answers the built-in methods on its payload through
   `builtins::native_base` (§9.79), whose payload now covers `__mutsu_pair_value` and `__baggy_data__`; the two delegation
   blocks with their name lists are gone, and `.Pair` of a `Pair` subclass is the instance.
6. **`True but False`** (a `Bool` whose only mixin is a `Bool`) is that `Bool` for every method: the four rendering arms
   become one receiver substitution, and `.Int`, `.Numeric` and `.not` follow the effective truth too, as in Rakudo.
7. **Dead arms**: the `Nil` absorber (`Nil.message`, `.payload`, ...) and the lazy `Match`'s `Capture` were reached by no
   receiver in the census: `Nil` absorbs the methods it does not define before native dispatch (`nil_absorbs_method`),
   and a match's `Capture` is `Capture`'s row. The recognition rows `Exception.line`/`Exception.file` (mutsu's own,
   Rakudo declares neither) go with the arm.

Pinned in `t/oo/method/prologue-method-rows.t`. Left in the prologue, each for its own PR: `Uni`'s `chars`/`comb`
(roast pins them on `NFC`/`NFD`, Rakudo declares neither, so no row can carry them) and `perl` (the rendering cascade has
no `Uni` case); `CompUnit::DependencySpecification`, which has two representations (a native value whose `.^name` is
`Any`, and an instance); `Distribution`, with the slow path's `methods_distribution.rs`; `IO::Path::Parts`; `__mutsu_zen_angle` (the
compiler's). `method-rows-report.py --arms`: the pure arity cascades go from 76 arms to 56.

### 9.112 Slice 5: `dispatch_core_math`, `dispatch_core_list` and `dispatch_core_unicode` are gone (2026-10-16)

`refactor/11276-core-math-arms`. The three family files of `methods_0arg` and `dispatch_core_numeric`'s `rand` arm are
deleted; the census of §9.111 named the receivers they still answered, and each reaches a row:

1. **`Cool.Rat` / `Cool.FatRat`** reach a `Range` and a settled `Seq` (both `Cool` in Rakudo: the two names join
   `COOL_NUMERIC_FOR_COLLECTIONS`) and a shapeless `Cool` (`COOL_COERCE_FOR_SHAPELESS`): an unsettled `Seq`, an enum, a
   `StrDistance`, which `numify` now reads as its edit distance (`StrDistance.Numeric` too). `cool_instance_numeric`
   asks shapes (`is_match_instance`, `DispatchShape::IoPath`) instead of matching class names.
2. **`Any.tree`** is a row (Rakudo declares it on `Any`); the closed shapes reach it through
   `ANY_LIST_FOR_CLOSED_SHAPES`, a `Match` among them (a non-`Iterable` is itself).
3. **`Cool`'s numeric methods** (`self.Numeric.METHOD`, [`cool_numeric_method`]) answer a `Seq` or `Slip` with no
   settled shape through `answer_shapeless`, on its element count: `Seq.sqrt`, `Seq.rand`.
4. **`Any.chrs`** is a row next to `List.chrs` (Rakudo declares both), so a number, a numeric string, a `Range` and a
   `Seq` reach it. **`Blob.bytes` / `Buf.bytes`** take a type object and throw `X::Parameter::InvalidConcreteness`
   (a parameterized one, `Buf[uint8]`, through `type_params::type_object_row`). **`IO::Path.chars`** reaches
   `Cool.chars` (an audited name of the `IoPath` shape). The `uniparse` arm was dead.

Pinned in `t/oo/method/core-math-method-rows.t`. Behaviour changes toward Rakudo: `StrDistance.Numeric` is its edit
distance; a numeric type object's `Rat`/`FatRat` (`Int.Rat`, answered `0` by the deleted arm) and every `:D`-only numeric
method of `Bool` (`Bool.abs`) throw `X::Parameter::InvalidConcreteness` (the numeric type-object check of
`dispatch_core_numeric` gains the two names and `Bool`). `method-rows-report.py --arms`: the pure arity cascades go from 56 arms to 43.
### 9.113 Slice 5: the two-argument cascade's `substr`, `fmt`, `base` and `unimatch` arms are gone (2026-10-16)

`refactor/11276-narg-arms`. The census of §9.111 found `dispatch_2arg`'s four arms answering calls their rows existed
for but did not admit: `"abcdef".substr(1, *)` (24,000 calls in one test), `.substr` of a `Match` or an `Int`, a
collection's `.fmt($format, $separator)` with a `Format` object, `Rat.base($radix, *)`. Each arm called the same function
as its row's handler, so the rows now take any plain argument (`RowFlags::ANY_ARGS` on every `substr` and collection `fmt`
row; the `base` rows had it) and the handler declines what only the interpreter can run. `unimatch` answered no receiver
the `Cool` row did not. `Duration`/`Instant` `.base($radix, $digits)` called the cascade on its seconds
(`native_method_2arg`); it calls `numeric_base::base_of` now. The recognition table no longer claims a separator for
`Str`/`Cool`/`Match`/`Int` `.fmt` (Rakudo's `Cool.fmt` has one parameter; the arity-2 bit was the arm's), and the
test-only arity probe asks the owner chain of a value with no shape (an immutable `Map`), as `invoke_owner` does. Pinned
in `t/oo/method/two-arg-method-rows.t`.
`method-rows-report.py --arms`: the pure arity cascades lose four arms.

### 9.114 Slice 5: `IO::Path::Parts` has a shape; its parts are rows (2026-10-16)

`refactor/11276-io-path-parts`. An `IO::Path::Parts` (`IO::Spec.split`, `IO::Path.parts`) was answered by a block of
eight arms in `methods_0arg/mod.rs` and two subscript arms in `dispatch_1arg`. It is now a closed instance shape,
`DispatchShape::IoPathParts`, with the three rows Rakudo declares on the class: `AT-POS` (the `volume`, `dirname` and
`basename` pairs by position, `Nil` past them), `AT-KEY` (a part by name) and `flat` (the three pairs). The type catalog
gains the class (MRO `IO::Path::Parts, Any, Mu`; the method table resolves rows along it). `Any`'s audited list rows
reach the shape and read it as one item, which is Rakudo's answer (`$parts.elems` is `1`, `.List` is `(parts,)`), and
`.hash`/`.Hash`/`.Map` read the parts by name through `map_hash_coerce::hash_contents`, the hook a package stash already
used. Behaviour changes toward Rakudo, pinned in `t/io/io-path-parts-method-rows.t`: `.Slip`, `.cache` and `.eager` are
one item (were the three pairs). `Mu.Str`/`Stringy` do not reach the shape: the object renders through the
interpreter's default representation. Left: `.^roles` lists nothing (the class's bootstrap `ClassDef` declares no roles),
and `.Str` is `IO::Path::Parts()` where Rakudo writes `IO::Path::Parts<id>`. `method-rows-report.py --arms`: the pure
arity cascades go to 31 arms.

### 9.115 Slice 5: `CompUnit::DependencySpecification` has one representation and its methods are rows (2026-10-16)

`refactor/11276-depspec-rows`. A depspec had two representations: a native value kind (`ValueRepr::CompUnitDepSpec`,
built when the constructor got no matcher), whose `.^name` was `Any` and which `methods_0arg/mod.rs` answered through five
arms, and an instance (built when a matcher was given), which the slow path's `methods_instance_ops` answered through
five more. The native kind is gone (`Kind`, `ValueRepr`, `ValueView`, serde and the nanbox codecs): every depspec is an
instance. A closed `DispatchShape::CompUnitDepSpec` reaches the rows Rakudo declares on the class (`short-name`,
`version-matcher`, `auth-matcher`, `api-matcher`, `from`, `Str`, `raku`), and both sets of arms are deleted. Behaviour
changes toward Rakudo, pinned in `t/oo/method/depspec-method-rows.t`: `.^name` is the class; `.Str` names the given
matchers (`Foo:ver<1.0+>:auth<zef:x>`, was `Foo`); `.raku` renders the constructor call; `.from` is `Perl6`. Left:
`.gist` is the default instance rendering where Rakudo's is `.raku`, and a version matcher stays the string it was given
where Rakudo coerces it to a `Version`.
### 9.116 Slice 5: `Uni`'s `chars`, `comb` and `perl` arms are gone (2026-10-16)

`refactor/11276-uni-arms`. The last three `Uni` arms of `methods_0arg/mod.rs`: `chars` and `comb` are rows of `Uni`
(the codepoint count, and the codepoints as one-character strings). Current Rakudo declares neither on `Uni`, but roast
pins both (`S15-string-types/NF-types.t`, `NFK-types.t`: `NFC.chars`, `NFC.comb`), and roast is the spec, so the rows
exist and `rows_are_declared_by_rakudo` names the two as its one roast-pinned exemption (the recognition table does not
claim them, so `.^can` keeps Rakudo's answer). `perl` is `Mu.perl`, i.e. `.raku`: the rendering cascade's `Uni` case
answers it with the `Uni.raku` row's handler. Pinned in `t/oo/method/prologue-method-rows.t`.

### 9.117 Slice 5: the `CompUnit` family's methods are rows, found by their owner (2026-10-16)

`refactor/11276-distribution-rows`. The repository API was answered by name in four places: a block of
`methods_instance_ops` (the `CompUnit::Repository` base methods, `resolve`, and the whole of
`CompUnit::Repository::FileSystem`), `dispatch_cur_installation_method`, `dispatch_compunit_method` with the
`CompUnit::Handle.globalish-package` arm, and the distributions' `dispatch_distribution_method` plus a `Distribution`
block in `methods_0arg/mod.rs`. Every one is now an `OWNER_ONLY` row of the class Rakudo declares it on, in three
family modules (`instances/compunit.rs`, `compunit_repo.rs`, `distribution.rs`), reached through the owner lookup of
the method-call fallback as `CompUnit::Handle.export-package` already was (`compunit::owners_of` names every owner of
the family). No class of the family gets a dispatch shape: the calls are not hot, several take a user object or a
named argument (`install($dist, :force)`, `files($file, :ver)`) that the guard step would refuse, and the owner entry
accepts any argument as the arms did. A repository's handlers copy its attributes before they run, because
`next-repo` writes the lazily built bundled chain back into the instance.

Two representations change toward Rakudo. A module's `$?DISTRIBUTION` was an instance of the `Distribution` *role*
carrying `$!meta`; it is a `CompUnit::Repository::Distribution` with a `meta` attribute. An installed distribution was
mutsu's own `Distribution::Installation`; it is a `CompUnit::Repository::Installation::LazyDistribution` (a `my class`
in Rakudo, so its recognition rows claim no only-method bit: the snapshot cannot list it). Both carry the catalog's
roles, so `~~ Distribution` holds.

Behaviour changes toward Rakudo, pinned in `t/modules/compunit/{compunit-repo,compunit,distribution}-method-rows.t`:
`FileSystem.prefix` is an `IO::Path` and `FileSystem.short-id` a row; a FileSystem repository has no `install`
(`X::Method::NotFound`, was an `X::AdHoc`); `CompUnit.name` and `CompUnit.globalish-package` and the type-object arms
`CompUnit.handle` / `CompUnit::Handle.globalish-package` (which Rakudo refuses) are gone; a FileSystem `need`'s
`CompUnit.from` is `Perl6`; `$?DISTRIBUTION.Str` and `.dist-id` are the identity
`name:ver<..>:auth<..>:api<..>` (was `Distribution<Distribution>`); `Distribution::Hash.Str`/`.gist` are the default
instance rendering (were the distribution's name). Left: `.content` answers an `IO::Path` where Rakudo opens an
`IO::Handle`; `CompUnit::Repository::Distribution.id`/`.files`/`.repo` and the other repository methods mutsu never
implemented (`abspath`, `resource`, `name`, ...) have no row. `method-rows-report.py --arms`: the pure arity cascades go
from 23 arms to 20, the runtime slow path from 536 to 497.

### 9.118 Slice 3E remainder: `Promise`, `Promise::Vow` and `Channel` are rows, found by their owner (2026-10-16)

`refactor/11276-promise-channel-rows`. A promise and a channel are native value kinds shared between threads
(`ValueView::Promise`, `ValueView::Channel`), answered near the end of `call_method_with_values` by
`dispatch_promise_method` and `dispatch_channel_method` (32 quoted-name arms in `methods_promise.rs`); a vow is a
`Promise::Vow` instance whose `keep`/`break` had two copies, one there and one in the native-methods tier
(`native_promise_vow`). Every method Rakudo declares on the three classes that mutsu implemented is now an `OWNER_ONLY`
row (`io_concurrency/promise_channel.rs`): `Promise`'s `get-await-handle`, `result`, `status`, `scheduler`, `then`,
`andthen`, `orelse`, `keep`, `break`, `cause`, `Bool`, `vow`; the vow's `keep`/`break`; `Channel`'s
`get-await-handle`, `send`, `receive`, `poll`, `close`, `fail`, `list`, `closed`, `elems`, `Supply`. The rows are found
by their owner at the place the arms were (`promise_channel::owners_of`), so the order against the rest of
`call_method_with_values` is unchanged; the vow's two entries (the native-methods tier and `call_method_with_values`)
reach the same row. The implementations are `Interpreter` methods in `methods_promise.rs`, one per method.

No shape: the calls block, run user callbacks or wake other threads, so none is a candidate for the pure entries, and the
owner entry takes the callable and object arguments the guard step would refuse. `Channel`'s `Seq`, `List` and `Array`
are `Any`'s `self.list.METHOD` coercions in Rakudo (no row can be `Channel`'s); the interpreter answers them as
`channel_list_coercion` (`.Seq` lazy, the other two draining until the close), which `.Seq`'s coercion entry and
`.Capture` share. The arms for names neither class declares (`WHAT`, `isa`, `Str`, `gist`, `raku`) are gone: the
general paths answer them. `Channel.elems` keeps a pure-cascade case, which calls the row's handler.

Behaviour changes toward Rakudo, pinned in `t/concurrency/promise/promise-channel-method-rows.t`: `Channel.elems`
dies with Rakudo's message (`Cannot determine number of elements on a Channel`); `Promise::Vow.Str` is the default
instance rendering (`Promise::Vow<id>`, was `(Vow)`); `Channel.Str` is `Channel` from the value's own rendering (Rakudo:
`Channel<id>`). Left: `Promise.Str` is `Promise(Planned)` from `value::display` where Rakudo writes `Promise<id>`; a
user subclass of `Promise` or `Channel` is an instance answered by the native-methods tier (`native_promise`,
`native_channel_mut`) on its attributes, a second representation in which `await` fails (#13634); the vow's internal
`__mutsu_run_start`/`__mutsu_run_dispatch` names stay with the 3G remainder's `__mutsu_*` names.
`method-rows-report.py --arms`: the runtime slow path loses 32 arms.

### 9.119 A user subclass of `Promise` or `Channel` carries the native value (2026-10-16)

`fix/13634-promise-subclass-payload` (issue #13634). §9.118 left a second representation: `P.new` for
`class P is Promise` built an instance whose built-in methods the native-methods tier answered on its attributes
(`native_promise`, `native_promise_mut`; `native_channel`, `native_channel_mut` for `Channel`), with no
`get-await-handle` (so `await` died), `vow`, `andthen` or `cause`. The instance now carries a native promise
(`__mutsu_promise_value`, bound to the scheduler a `:scheduler` argument names) or channel (`__mutsu_channel_value`),
seeded where the scalar subclasses' payloads are (§9.79), and the `Promise`/`Channel` rows answer its built-in methods on
that payload (`promise_channel::receiver_of`): from the native-methods tier, which the classes' `native_methods` lists
still route to, and from the owner lookup in `call_method_with_values`. A `Channel` subclass's `Seq`/`List`/`Array` read
its channel before `Any`'s rows read the instance as one item. The attribute representation, `make_promise_instance`
and `await`'s two branches for an instance of `Promise` are deleted. Pinned in
`t/concurrency/promise/promise-channel-subclass.t`.

### 9.120 Slice 3E remainder: `Promise`'s class methods are rows (2026-10-16)

`refactor/11276-promise-class-rows`. `Promise.start`, `.in`, `.at`, `.kept`, `.broken`, `.allof` and `.anyof` were six
quoted-name arms of `dispatch_method_by_name_1` (and the `Promise` part of its `start` arm), each asking whether the
receiver is a `Promise` class (`promise_class_name`). They are `OWNER_ONLY | TYPE_OBJECT_OK` rows of `Promise`
(`io_concurrency/promise_class.rs`; `allof`/`anyof` `SLURPY`, `kept`/`broken` at arity 0 and 1), found by their owner at
the top of that dispatch for a type-object receiver; a handler declines a type object that is no `Promise` class and the
dispatch goes on as before. The handlers hand the implementations the positional and named arguments back as one list,
which is how they read `:scheduler`. The `start` arm keeps `Supply.start` and `Thread.start`. No behaviour change;
pinned in `t/concurrency/promise/promise-class-method-rows.t`.

### 9.121 Slice 5: `Attribute`, `IterationBuffer` and `Routine` are rows, found by their owner (2026-10-16)

`refactor/11276-instance-ops-rows`. Three families were answered by name in `methods_instance_ops.rs`: the
`Attribute` metaobject's methods and `IterationBuffer`'s, each a block of `dispatch_instance_and_fallback` behind a
class-name test, and `Routine`'s `is_dispatcher`, `dispatcher`, `multi` and `package` in the arms that end the method
call when nothing else answered; `dispatch_routine_method` (`methods_sub.rs`) kept a second copy of the first three for a
`&name` handle. Every one is now an `OWNER_ONLY` row of the class Rakudo declares it on, with §9.117's mechanism: no
shape, an `owners_of` per family, and `invoke_owner` at the place the arms were, so the order against the rest of the
dispatch is unchanged.

- `Attribute` (`ctors_mop/attribute.rs`): `name`, `type`, `has_accessor`, `rw`, `readonly`, `required`, `package`,
  `gist`, `Str`, `container`, `set_rw`, `build`, `set_build`, `get_value`, `set_value`. The ones that read the registry
  or the class table are `Interpreter` methods in `runtime/methods_attribute_meta.rs`. Rakudo declares no `raku` on
  `Attribute` (`Attribute.new` is `Mu.raku`), so the arm is gone with no row; the default instance rendering answers
  `BOOTSTRAPATTR.new` for a bootstrap attribute (`Int.^attributes`), as Rakudo does.
- `IterationBuffer` (`collections/iteration_buffer.rs`): `elems`, `AT-POS`, `BIND-POS`, `push`, `unshift`, `append`,
  `prepend`, `clear`, `List`, `Slip`, `Seq`, `iterator`, for a buffer or an instance of a subclass that does not declare
  the method itself (ValueList's throwing `BIND-POS`/`push` still win). The snapshot gains `IterationBuffer`.
- `Routine` (`instances/routine.rs`, implementations in `runtime/methods_routine_rows.rs`): one implementation per
  method answers a `Sub` and a `Routine` handle, reached from `dispatch_callable_method` next to the `Code` rows, and
  `package` answers the code-class instances `.^methods` builds from the fallback. The snapshot gains `Routine`. The
  catalog folds `Routine` into `Code` (`canonical_builtin_owner`), so `rows_are_declared_by_rakudo` now checks a row
  under its own owner before its folded one.
- `Code.set_name` is one row for a `Sub` (`rename_code`) and a regex (`set_regex_name`); the `dispatch_sub_method`
  arm and the fallback's regex arm are gone. The fallback's regex arms for `line`, `file`, `of`, `returns`,
  `signature`, `arity` and `count` are gone too: the `Code` rows (ADR-12523) already answer a regex, and the fallback
  asks them (`code::answer`, which now takes the call's arguments).

No behaviour change; pinned in `t/oo/attribute/attribute-method-rows.t`, `t/collections/lazy-seq/iterationbuffer-rows.t`
and `t/routines/code-routine-rows.t`. Left in `methods_instance_ops.rs`: the `Proc::Async` routing to the native-methods
tier (its implementation is the name `match` of `native_methods/proc.rs`; 3E remainder), the fallback's universal
answers (`DUMP`, `gist`, `raku`, `name`, `isa`, `REPR`, `Str`, `Numeric`, `EVAL`, `new_type`, for type objects,
junctions and plain values), and the proxy-subclass array mutators. `method-rows-report.py --arms`: the runtime slow
path goes from 459 arms to 428.

### 9.122 Slice 5: built-in classes' class methods are rows (2026-10-16)

`refactor/11276-class-method-rows`. `dispatch_method_by_name_3` answered thirteen class methods by name, each arm asking
whether the receiver is one class's type object: `Supply`'s `from-list` and `on-demand`,
`CompUnit::RepositoryRegistry`'s `repository-for-name`, `repository-for-spec` and `run-script`, `Encoding::Registry`'s
`find` and `register`, `IO::Socket::INET`'s `connect` and `listen`, and `IO::Socket::Async`'s `connect`, `listen`,
`bind-udp` and `udp`. They are `OWNER_ONLY | TYPE_OBJECT_OK | SLURPY` rows of those classes
(`io_concurrency/class_methods.rs`), found by their owner at the top of that dispatch for the owner's own type object
(`class_methods::owners_of`), as §9.120 did for `Promise`. The implementations read the positional and named arguments
as one list, so the handlers hand them back that way; the registry's two lookups are `Interpreter` methods in
`runtime/methods_repo_registry.rs`. The snapshot gains `CompUnit::RepositoryRegistry`, `Encoding::Registry`,
`IO::Socket::INET` and `IO::Socket::Async`. A second, unreachable `Numeric` arm of the same dispatch is deleted.

Behaviour toward Rakudo: `from-list` and `on-demand` called on a `Supply` instance die with Rakudo's "Invocant of method
'on-demand' must be a type object of type 'Supply', ..." (was mutsu's own "Cannot call on-demand on a Supply instance",
and nothing for `from-list`); `owners_of` names `Supply` for an instance too, so the handlers see it.
Pinned in `t/oo/method/class-method-rows.t`. Left: `Supply.signal` (Rakudo declares no such method; the arm stays as
mutsu's spelling of `signal()`), `DateTime.now`, `Date.today`, `Date.new-from-daycount` and the type objects'
`Date`/`DateTime` coercions (the instance rows of the same names in `instances/date.rs`/`datetime.rs` would need
`TYPE_OBJECT_OK` and a receiver test, and a user subclass's type object needs its MRO), `Thread.run` and `Supply.live`.
Found on the way: #13651 (`Encoding::Registry.find(...).name` spells `utf-8`, Rakudo `utf8`); `repository-for-name`
of a `file#` path answers a `FileSystem` repository where Rakudo answers `Nil` (kept). `method-rows-report.py --arms`:
the runtime slow path goes from 428 arms to 416.

### 9.123 `invoke_owner` admits a type object only to a `TYPE_OBJECT_OK` row; `Date`'s and `DateTime`'s class methods are rows (2026-10-16)

`refactor/11276-temporal-class-rows`. The owner entry (`invoke_owner`, §9.19) found a row by owner, name and arity and
called it whatever the receiver was, so a type object could reach an instance row's handler, which the guard step
never lets happen; every owner whose class methods became rows so far (§9.120, §9.122) had handlers that happened to
decline one. The entry now refuses a type object (`ValueView::Package`) for a row without `TYPE_OBJECT_OK`, as the guard
step does, and the call goes on as if no row matched. No caller relied on the old behaviour (the `IO::Spec` rows, the
`Promise` and §9.122 class-method rows are `TYPE_OBJECT_OK`; the other owner lookups pass instances).

That makes owners with instance rows safe to look up by owner for a type object: `Date.today`, `Date.new-from-daycount`
and `DateTime.now`, three arms of `dispatch_method_by_name_3`, are `OWNER_ONLY | TYPE_OBJECT_OK` rows
(`instances/temporal_class.rs`, implementations in `runtime/methods_temporal_class.rs`), found by owner for the type
object of the class or of a user subclass (`Interpreter::temporal_class_owners` reads a subclass's cached MRO); a
subclass's `today`/`new-from-daycount` build through its constructor, as before. Their recognition rows claim arity `N`:
no pure cascade answers them. Not moved: the type objects' `Date`/`DateTime` coercions, whose Rakudo answers are
irregular (`Date.Date` is `(Date)`, `D.Date` of a subclass dies reading attributes; mutsu answers the type object for
both). No behaviour change; pinned in `t/oo/method/date-class-method-rows.t`, which also pins that `Date.year` and a
subclass's `D.year` die. `method-rows-report.py --arms`: the runtime slow path loses 3 arms.

### 9.124 `Seq.from-loop`, `Thread.start`, `Thread.is-initial-thread`, `X::AdHoc.from-slurpy` and `Supply.start` are rows (2026-10-16)

`refactor/11276-thread-seq-class-rows`. Four arms of `dispatch_method_by_name_1` asked which type object the receiver
was (`from-loop` and its `from_loop` spelling on `Seq`, `start` on `Thread` or a `Supply` instance through
`dispatch_promise_start`, `is-initial-thread` on `Thread`, `from-slurpy` on `X::AdHoc`). `Seq.from-loop`,
`Thread.start` and `X::AdHoc.from-slurpy` are `OWNER_ONLY | TYPE_OBJECT_OK | SLURPY` rows next to §9.122's
(`io_concurrency/class_methods.rs`), found by the early owner lookup at the top of that dispatch, which named only
`Promise` (§9.120) and now names `Seq`, `Thread` and `X::AdHoc` for their own type objects (`early_owners_of`) and
`Promise` for any other. `Supply.start` is an instance row of `Supply`, found by the later lookup of
`dispatch_method_by_name_3` (§9.122's `owners_of` already names a `Supply` instance). `Thread.is-initial-thread` had a
row for an instance (§9.22); the row admits the type object now and answers for the calling thread there.
`dispatch_promise_start` is deleted. Behaviour toward Rakudo: `Seq.from_loop` is gone (Rakudo has no such method; no
caller in the tree). Pinned in `t/concurrency/thread-lock/thread-seq-class-method-rows.t`.

### 9.125 An enum member's methods are rows of `Enumeration`; the base type answers what it does not declare (2026-10-16)

`refactor/11276-enumeration-rows`. `dispatch_enum_method` answered an enum member's methods by name (`key`, `value`,
`kv`, `pair`, `gist`, `raku`/`perl`, `Str`, `pred`, `succ`, `ACCEPTS`, the numeric coercions, `WHAT`: 14 arms). The
§9.105-era status notes left it for a design, because Rakudo composes the methods per enum type: every enum type does
the role `Enumeration`, and `NumericEnumeration` (an `Int`-valued enum) or `StringyEnumeration` (a `Str`-valued one).

Decision. mutsu's member is one value kind for every enum (`ValueView::Enum`), so the rows belong to the roles, not to
the enum types: `Enumeration`'s `key`, `value`, `kv`, `pair`, `gist`, `raku`, `pred`, `succ`, `ACCEPTS`, `Int`,
`Numeric`, `Real`, and the `Str` of `NumericEnumeration` (the key) and `StringyEnumeration` (the value)
(`instances/enumeration.rs`). They are `OWNER_ONLY` rows found by owner from `dispatch_enum_method`;
`enumeration::owners_of` names a member's roles, the more specific first. No dispatch shape: an enum type is a user
declaration, and the receiver kind already identifies the roles. The recognition table records the roles' declared
names (from `Enumeration.^pun.^method_table`), including the ones with no row yet (`WHICH`, `clone`, `pick`, `roll`,
`new`, `enums`, `CALL-ME`, `COERCE`); the snapshot generator skips roles, so the rows carry no `ONLY_METHOD` bit (an
`augment` cannot target a role).

A member is a value of its base type (Rakudo's enum type inherits from it), so a method the three roles do not declare
is now the base type's row, answered on the member's value: `Int`'s rows for an `Int`-valued member (`y.Num`,
`y.Rat`, `y.Complex`, which were arms forwarding to the value, and every other `Int` row, `y.is-prime`), `Str`'s for a
`Str`-valued one (`p.uc`). The recognition table's `DECLARED` bits for `Enumeration` keep a name the role declares
(`WHICH` answers `E|1`, not `Int`'s `Int|1`) from reaching the base. `perl` is looked up as `raku` (`Mu.perl` is
`self.raku`), as the `IO::Path` rows do. The `WHAT` arm is gone: the general pseudo-method answers it. An
`Array`/`Hash`-valued member keeps `enum_aggregate_base`, whose name list is the base type's collection methods.

Pinned in `t/oo/method/enumeration-method-rows.t`. Left: an `Array`-valued member's `Str` is its key where Rakudo
answers the array's (`1 2`), as before; the enum type object's `pairs`/`keys`/`values`/`kv`/`antipairs`/`invert` stay
in `call_method_with_values` (Rakudo answers them through the enum type's HOW, not `Enumeration`).
`method-rows-report.py --arms`: the runtime slow path loses 14 arms.

### 9.126 A built-in iterator's protocol methods are rows of `Iterator` and `PredictiveIterator` (2026-10-16)

`refactor/11276-iterator-protocol-rows`. The protocol of mutsu's built-in iterator (one class, `Iterator`, for every
kind) was answered by name in three copies: the mutating method call (`methods_mut_dispatch.rs`, a variable receiver),
the method-call fallback (`methods_call_dispatch.rs`, an array element or a temporary) and the VM's
`try_native_iterator` (`CallMethodMut`, a subset of the names re-implemented). Session 30's status read the first copy
as needing `Handler::Mut` because it wrote the advanced state back to the variable. It did not need to: an iterator's
state is its instance's shared attribute cell, and every copy already committed through it (the variable write-back was
a re-insert of a value sharing the same cell).

Decision. A protocol method on a reference object whose state lives in its shared attribute cell is a pure-entry row
(`Handler::Interp`), not a `Handler::Mut`: the place of the receiver is never needed, because no copy of the receiver
exists to update. The protocol methods are `OWNER_ONLY` rows of the roles Rakudo declares them on
(`instances/iterator.rs`): `Iterator`'s `pull-one`, `push-exactly`, `push-at-least`, `push-all`, `push-until-lazy`,
`sink-all`, `skip-one`, `skip-at-least`, `skip-at-least-pull-one`, and `PredictiveIterator`'s `count-only` and
`bool-only`. Every handler is the one implementation (`runtime/iterator_protocol_call.rs`) for the three kinds the
instance's attributes tell apart: a `.map`/`.grep` stream, a `squish` scan with callbacks, an array-backed cursor topped
up from a lazy source. The stepping logic (`iterator_protocol::step`, `needed_len`, the stream's call) takes an
`IterOp`, not the method's name. The three dispatch places ask `iterator::owners_of` and `invoke_owner`; the VM's copy
is gone.

`owners_of` names `PredictiveIterator` only for a kind that knows its count, as Rakudo's iterator classes do: an
array-backed cursor with no lazy source or a known count. A stream (Rakudo's map iterator), a `squish` scan
(`Any::Squish-With`) and a cursor over a lazy source with no known count (a `gather`, an unending range) are `Iterator`
only. `.can` of a built-in iterator answers from the same owners (`collect_can_methods`, `owner_declares`), so
`Test::Iterator`'s `$iterator.can('count-only')` guard reads what the iterator does. The recognition table gains the
two roles' declared names.

Behaviour toward Rakudo, pinned in `t/collections/lazy-seq/iterator-protocol-rows.t`: `count-only`/`bool-only` of a
`squish` scan with callbacks are `X::Method::NotFound` (were a count); `.can('count-only')` of a map stream is empty; a
`squish` scan's `push-exactly` answers the count it pushed (was `Nil`); a `squish` scan held in an array element or a
temporary now steps (the fallback's copy knew only the array-backed kind, and answered `IterationEnd`). `.can` of a
protocol name answers a `Method` of the role whatever the receiver (a variable receiver answered a list holding the
name as a `Str`). `method-rows-report.py --arms`: the runtime slow path goes
from 399 arms to 383.
### 9.127 A `Date`/`DateTime` instance the shape lookup did not take answers through the owner rows (2026-10-16)

`refactor/11276-temporal-instance-rows`. `runtime/methods_temporal.rs` answered `later`/`earlier`/`truncated-to`/
`in-timezone` and a name `match` (`yyyy-mm-dd`, `mm-dd-yyyy`, `dd-mm-yyyy` with a separator, `clone`, `Date`'s
`in-timezone`, `first-date-in-month`, `last-date-in-month`, `utc`, `posix($x)`) for any instance carrying date
attributes: a subclass instance, which has no shape, or a call the guard step declined (named overrides, a list of
pairs). Session 30 left it for a mechanism that keeps the subclass in results. The mechanism was already there: every
derived-value handler re-blesses into the receiver's class (`rebless_date_result`/`rebless_datetime_result`), so what
was missing were rows for the names only the `match` answered.

- New rows: `Date.clone` and `DateTime.clone` (`Handler::Named`, `ANY_NAMED`: the overrides are `*%_`); `later` and
  `earlier` of arity 1 (the units as one positional list of pairs, `ANY_ARGS`); `DateTime.posix` at arity 0 and 1 with
  `:real`. `DateTime.utc` re-blesses a subclass instance's result, so the 0-arg cascade no longer declines `utc` for a
  subclass, and the method-call fallback's `local` arm (a copy of the `local` row) is gone.
- `dispatch_temporal_method` is one owner lookup (`Interpreter::temporal_instance_owners`: `Date` or `DateTime` for an
  instance of the class or of a subclass, by the class's MRO, not by its attribute names, so a user class with a
  `year`/`month`/`day` no longer reaches these handlers).

Behaviour toward Rakudo, pinned in `t/types/temporal/date-subclass-narg-rows.t` (which passes under `raku`):
`Date.in-timezone` is `X::Method::NotFound` (Rakudo has no such method; mutsu answered a `DateTime`);
`DateTime.posix($ignore-timezone)` reads its positional as Rakudo's `$ignore-timezone` (mutsu read it as `:real`), and
`posix(:real)` answers a `Rat` (was a `Num`). Not followed: Rakudo dies on a `Date` subclass's `truncated-to`,
`first-date-in-month` and `last-date-in-month` (`P6opaque: no such attribute '$!day'`); mutsu answers them.
`method-rows-report.py --arms`: the runtime slow path loses 10 arms.

### 9.128 `Proc::Async`'s methods are rows, found by their owner (2026-10-16)

`refactor/11276-proc-async-rows`. `Proc::Async` was answered by name in six places: two name lists in the instance
dispatch (`methods_instance_ops.rs`) that sent the mutating names to `call_native_instance_method_mut_in_place` and the
reading ones to `call_native_instance_method`, the `Proc::Async` arms and MRO candidates of both native dispatchers
(`native_methods/mod.rs`), the name `match`es of the implementation (`native_proc_async_mut`, `native_proc_async`), and
the VM's two skip lists (`vm_call_method_ops.rs`, `vm_call_method_mut_ops.rs`) that kept the cascades off those names.
§9.23 left it for "a third place kind" because its mutators publish through `AttrPublisher` (#7923). As with the
iterators (§9.126) no place is needed: the publisher writes the instance's shared attribute cell, which every alias
reads, so a pure-entry row commits for all of them.

Decision. `Proc::Async`'s `start`, `kill`, `write`, `print`, `put`, `say`, `close-stdin`, `bind-stdin`, `bind-stdout`,
`bind-stderr`, `ready`, `stdout`, `stderr`, `Supply`, `command`, `started`, `w` and `pid` are
`OWNER_ONLY | SLURPY` `Handler::Interp` rows (`io_concurrency/proc_async.rs`; the methods read positional and named
arguments as one list, `start(:ENV, :cwd)`, `stdout(:bin)`). Their one handler, `Interpreter::proc_async_call`, runs
the implementation on a working copy and commits it through the cell with an `AttrPublisher`, as
`call_native_instance_method_mut_in_place` did; the implementation dispatches on a `ProcAsyncOp`, not on the name. The
instance dispatch and the mutating method call find the rows by owner where the lists and the `is_native_method`
gate were; the native dispatchers no longer name `Proc::Async`; the VM's skip lists ask `owner_declares`. The reading
methods' copies of `stdout`, `stderr` and `Supply` (unreachable: the lists sent those names to the mutating path) are
gone. The recognition table and the oracle snapshot gain `Proc::Async`.

No behaviour change; pinned in `t/concurrency/proc-async-method-rows.t` (a start through an array element is seen
through the variable). Follow-up #13665 (a user subclass of `Proc::Async` could not be constructed with positional
arguments, so `proc_async::owners_of` named only the class itself) is resolved by #13741. `method-rows-report.py --arms` does not count the
removed lists (they are `matches!` lists and the implementation's file is not a cascade).

### 9.129 A declined call walks the receiver's MRO; `Str`'s and `Cool`'s text rows take every argument form (2026-10-16)

`refactor/11276-cool-text-arms`. `dispatch_method_by_name_1` answered fifteen text methods by name for every receiver
the rows had not taken: `match`, `subst`, `wordcase`, `comb`, `IO`, `contains`, `starts-with`, `ends-with`, `index`,
`indices`, `rindex`, `substr-eq`, `substr`, `substr-rw` and `trans`. Two kinds of call reached them. A `Str`/`Cool`
receiver whose argument the pure rows did not bind (a `Regex` needle, `:i`, a position, a `Range`, a `:filter`), and a
receiver the rows did not reach at all: an `IO::Path`, a `Range` or `Map` with a regex, an enum member, a `Seq`, a
type object, a `Cool` instance, but also a `Pair`, a `Version`, a `Capture` and a plain instance, which Rakudo refuses
(`X::Method::NotFound`, they are not `Cool`) and the arms answered from their string form.

Decisions.

1. **The rows take every argument form.** `contains`/`index`/`rindex` (arity 1 and 2), `starts-with`, `ends-with`,
   `substr` (1, 2), `substr-eq` (1, 2), `comb` (1, 2) and `wordcase` (0, `:filter`/`:where`) of `Str` and `Cool` are
   `Handler::Interp` rows (`scalars/str_search_interp.rs`, `ANY_ARGS | ANY_NAMED`) whose handler answers a plain
   needle by the pure candidate it replaced and every other form by the interpreter routine the arm called. The
   pure rows of those names are gone; the pure candidates stay as functions.
2. **A declined call walks the receiver's MRO** (`method_table::invoke_mro`): the owners `.^can` walks
   (`classhow_mro_unhidden_names`), the first row that binds the call answers. A row binds when it takes the arity,
   declares the named arguments (or `ANY_NAMED`), admits a type object only with `TYPE_OBJECT_OK`, admits the
   arguments (an interpreter row any but a `Junction`, a pure row as the guard step does) and its handler does not
   decline; otherwise the walk goes on, as a parent's multi candidate stays a candidate in Rakudo
   (`(1..5).contains(/3/)` is `Cool`'s although `Range` declares `contains`). When an owner declares the name but no
   owner has a row at the call's arity, the answer is `X::Multi::NoMatch` (`"".subst`). An instance with a `Str` of
   its own whose method is `Cool`'s or `Any`'s is read through it (`self.Str.METHOD`). An enum member walks its base
   type's owners, because `.^mro` omits the base (#13667). The step answers only the fifteen names above
   (`TEXT_METHODS_BY_MRO`), where the arms were; widening it to other names is the next use of the mechanism.
3. `Str.IO`/`Cool.IO` and `Any.match` are `TYPE_OBJECT_OK` (`Str.IO`, `Int.IO`, `Str.match(/b/)` answer as in Rakudo,
   which the arms did).

Behaviour toward Rakudo, pinned in `t/oo/method/cool-text-methods-by-mro.t` (which passes under `raku`): a plain
instance, a `Pair`, a `Version` and a `Capture` have none of these methods (`X::Method::NotFound`, they answered a
text made of their string form); a `Cool` instance's `IO` and `match` read its own `Str`. A probe of 22 calls (every
argument form above) over 26 receivers (`Str`, `Int`, `Rat`, a `Cool` instance with a `Str`, a `Str` subclass, a plain
instance, an enum member, a `List`, an `Array`, a `Hash`, a `Pair`, a `Set`, a `Range`, a `Seq`, four type objects,
`Nil`, a junction, a `Match`, an `IO::Path`, a `Version`, a `Capture`, a `Num`, an allomorph, a lazy `Seq`) against
the pre-change binary and `raku`: no answer that matched Rakudo before differs now, and 26 answers moved to Rakudo's.
`method-rows-report.py --arms`: the runtime slow path goes from 373 to 368 arms.
### 9.130 `Supply`'s methods are rows, found by their owner (2026-10-16)

`refactor/11276-supply-rows` (3E remainder). A `Supply` instance's methods were answered by name in a dozen places that
each asked whether the receiver was a `Supply`: the transform list of `dispatch_method_by_name_2` (`head`, `flat`,
`batch`, `throttle`, `comb`, `words`, `wait`, `zip`, `zip-latest`, `stable`), the `Supply` branches of the generic
`map`/`reduce`/`produce`/`elems`/`min`/`max`/`sort`/`rotor`/`squish`/`skip`/`grab`/`first` routines and of the `live`,
`rotate`, `unique`/`repeated`, `encode`, `tail` and `split` arms, the class-method arms for `Supply.merge`/`zip`/
`zip-latest` and the `delayed(0)` shortcut of `call_method_with_values`, and the native dispatchers reached through the
`is_native_method` gate (where a variable receiver took the mutating path and a temporary the reading one).

Decision. Every method mutsu implements of the 63 Rakudo declares on `Supply` is an `OWNER_ONLY | SLURPY` row of
`Supply` (`instances/supply.rs`, besides the `list` row of §9.55 and the `start`, `from-list` and `on-demand` rows of
§9.122/§9.124), found by owner (`supply_owners_of`) at the top of the by-name dispatch, in the mutating method call
and in the instance dispatch. One handler, `Interpreter::supply_call` (`runtime/methods_supply_rows.rs`), dispatches on
a `SupplyOp` to the routine that answered the method before: the dedicated `dispatch_supply_*` routines, the
subscribing `tap`/`act`/`on-close` through the instance's shared cell (`call_native_instance_method_mut_in_place`), and
the transforms through `dispatch_supply_transform`. `merge`, `zip` and `zip-latest` are `TYPE_OBJECT_OK` and answer the
type object with the class routines. The `Supply` branches listed above are deleted. `serialize` and `sanitize` are new
rows answering the supply itself (a synchronous supply is already serial and well-formed), as Rakudo's do for one.

No other behaviour change: a probe of 71 calls (every row, the class methods, the type object's refusals) answers as
before but for `serialize`/`sanitize` (were `X::Method::NotFound`). Pinned in `t/concurrency/supply/supply-method-rows.t`.
Left: the generic `grep` and `minmax` routines keep their `Supply` branches (a live supplier-backed `grep`, the 0-arg
`minmax`); the type object's instance-method refusals stay in `call_method_with_values` (Rakudo's
`X::Parameter::InvalidConcreteness`); `Supply.snip` (Rakudo has none) still answers. The recognition table gains the 50
names.

### 9.131 `Collation`'s methods are rows, found by their owner (2026-10-16)

`refactor/11276-collation-rows`. `$*COLLATION`'s `primary`, `secondary`, `tertiary`, `quaternary`, `set`, `gist` and
`raku` were answered by one name `match` (`dispatch_collation_method`), reached from an arm of
`dispatch_method_by_name_2` and, for `set` on a variable, from a special case of the mutating method call that wrote
the result back to the variable. They are `OWNER_ONLY` rows of `Collation` (`instances/collation.rs`), found by owner
next to `Supply`'s (§9.130) and in the mutating method call; `collation-level`, which Rakudo declares and mutsu lacked,
is a row too. `set` writes the instance's shared attribute cell, so it is a pure-entry row (§9.126): the variable
write-back was a re-insert of a value sharing the cell, and is gone with the special case. Rakudo declares no `raku`
on `Collation` (its default `.raku` lists the one `$!collation-level` attribute), so `raku` is no row: the instance
rendering that answers `Supplier`'s and the schedulers' `.raku` re-encodes the level, since mutsu stores the four
levels. `dispatch_collation_method` is deleted.

Pinned in `t/oo/method/collation-method-rows.t` (which passes under `raku`): `collation-level` answers (was
`X::Method::NotFound`), and a `set` through an array element is seen through `$*COLLATION`. The oracle snapshot gains
`Collation`.
### 9.132 `IO::CatHandle`'s methods are rows, found by their owner (2026-10-16)

`refactor/11276-cathandle-rows` (3E remainder). A cat handle's methods were routed by two name gates: the instance
dispatch's `class_name == "IO::CatHandle" && is_native_method` block (lazy `.lines`/`.handles`, then the mutating
native path) and the mutating method call's `IO::CatHandle` special case inside its `is_native_method` block. The 42
methods mutsu implements of those Rakudo declares on `IO::CatHandle` are `OWNER_ONLY | SLURPY` rows
(`io_concurrency/cathandle.rs`), found by owner next to `Proc::Async`'s and `Supply`'s in both entries. One handler,
`Interpreter::cathandle_call`, hands out the lazy list for `.lines`/`.handles` and runs every other method on a working
copy committed through the shared cell, as both gates did; no row needs a place (§9.126). The two gates are gone; the
native dispatcher's `IO::CatHandle` arm stays as the implementation's entry. A user subclass of `IO::CatHandle`
keeps the inherited native path (`owners_of` names the class itself only), as before.

No behaviour change; pinned in `t/io/cathandle-method-rows.t` (a `get` through an array element advances the cat the
variable holds; also passes under `raku`). The recognition table and the oracle snapshot gain `IO::CatHandle`.

### 9.133 `Supplier`'s and the schedulers' methods are rows; a row names the native path it runs (2026-10-16)

`refactor/11276-supplier-scheduler-rows` (3E remainder). `Supplier`, `ThreadPoolScheduler` and
`CurrentThreadScheduler` were reached through the `is_native_method` gates, where the receiver picked the
implementation: a variable took the mutating native path first (falling back to the reading one), a temporary or an
attribute (`$!supplier.emit`) the reading one. For `Supplier.emit`, `done` and `quit` those are two copies, and the
reading copy lacks the serialize guard of `Supply.serialize` and the timed buffer the mutating one keeps; for a
scheduler's `uncaught_handler` the mutating copy is the setter, which a variable receiver's getter call could reach with
no argument and so clear the handler.

Decision. Their methods are `OWNER_ONLY` rows (`io_concurrency/supplier_scheduler.rs`): `Supplier`'s `emit`, `done`,
`quit` and `Supply` (`Supplier::Preserving` reaches them through `owners_of`), the schedulers' `cue`, `loads`,
`max_threads`, `usage` and `uncaught_handler`. A row states which native path it runs, mutating (a working copy
committed through the shared cell) or reading (a snapshot of the attributes), through one handler,
`Interpreter::native_owner_call`; `uncaught_handler` is a reading row at arity 0 and a mutating row at arity 1. They are
found by owner next to `Proc::Async`'s and `Supply`'s in the instance dispatch and the mutating method call. The
reading copies of `emit`/`done`/`quit` are now reached by no row; they stay for the dispatcher's other callers.

Behaviour: an attribute or temporary receiver's `emit`/`done`/`quit` runs the mutating copy (serialized like a
variable's). Pinned in `t/concurrency/supply/supplier-scheduler-method-rows.t` (passes under `raku`). The recognition
table and the oracle snapshot gain the three owners.

Follow-up (`fix/supplier-subclass-rows`). An instance of a user subclass of `Supplier` (`class T is Supplier`) was
built with none of a supplier's state, so its `emit` reached no tap. A supplier's state is its attributes, so
`seed_concurrency_payload` seeds them at construction (`Interpreter::seed_supplier_state`, shared with
`Supplier.new`'s constructor) and marks the instance (`SUPPLIER_SUBCLASS`); `owners_of` answers `Supplier` for a marked
instance, so the rows reach it as they reach `Supplier::Preserving`. With every supplier receiver on the rows, the
reading copies of `emit`/`done`/`quit` in `native_supplier` had no caller left and are deleted. Pinned in
`t/concurrency/supply/supplier-subclass-rows.t`.

### 9.134 `Any`'s iteration methods are rows found by the MRO; the early entry, not the row, defers (2026-10-16)

`refactor/11276-any-list-rows`. `Any`'s `Handler::Interp` rows (`collections/any_interp.rs`: `map`, `grep`, `first`,
`reduce`, `produce`, `iterator`, `eager`, `squish`, `rotor`, `skip`, `collate`, `classify`, `categorize`,
`classify-list`, `categorize-list`) each call the one `dispatch_*` routine the by-name cascades' arm called too, so the
arms stayed only for the calls the table entry does not answer: an instance, a type object, a call the row declined.
The `map`/`grep`/`first` rows declined a receiver whose elements live behind a user `iterator`, an `IterationBuffer`,
a mixin's `iterator` or a lazy-pipe source, because the VM's native entry (`try_native_method`) asks the table before
`call_method_with_values` runs the probes that turn such a receiver into its elements. The §9.129 MRO walk could not
replace the arms while the rows declined those receivers: the walk runs after the probes, where the decline is wrong.

Decision. Deferring is a property of the call site, not of the row. `early_entry_defers` is the VM entry's veto
(beside the `augment` veto, in both of its `try_dispatch_in` calls); the rows keep only what is theirs, declining an
associative receiver for `squish`/`eager`/`produce` (the pure cascades read it as pairs). The arms are gone: the by-name
cascade asks `any_list_method_by_mro`, which walks the receiver's MRO with `invoke_mro` before its other arms (after the
`Supply` type-object guard for `classify`/`categorize`). The rows that Rakudo's `Any:U` candidates cover take a type
object (`TYPE_OBJECT_OK`: `Int.map`, `Int.first`, `Int.eager`); `rotor` does not. A call no row binds is
`X::Multi::NoMatch` (`.reduce()`, `.squish(1)`, `.iterator(1)`), as in Rakudo, where the arms answered some with an
ad-hoc error or a wrong result. Remaining divergence: `.map()` with no callable is `X::Multi::NoMatch` here and
`X::Cannot::Map` in Rakudo (it was a wrong success before), and `Int.rotor(2)` is `X::Method::NotFound` rather than
`X::Multi::NoMatch` (`invoke_mro` reports no match only when no owner has a row of the arity). Pinned in
`t/oo/method/any-list-methods-by-mro.t`. 14 arms leave the runtime cascades.

What the walk needed from the rows, since the arms took every call the routine takes: `grep` and `first` validate
their adverbs and arity themselves (`X::Adverb`, the detailed `grep(List:D: )` no-match), so their rows are one
`SLURPY`, `ANY_NAMED` row each; their matcher is `Mu`, so a new flag, `JUNCTION_ARGS`, lets `invoke_mro` bind a
`Junction` argument as itself (`@log.grep('a' | 'b')`) instead of refusing it for autothreading; `map` declares the
eight adverbs `accepted_nameds` keeps (a `:label` must still rank the built-in before an augmented capture-only
candidate); `classify-list`/`categorize-list` are `SLURPY` from arity 1 and gain the rows Rakudo declares on `Bag`,
`BagHash`, `Mix` and `MixHash` (`MixHash.new.classify-list(@mapper, 1, 2, 3)`).

Follow-up (`refactor/11276-any-maps-rows`). `flatmap`, `deepmap`, `duckmap` and `nodemap` had no row at all, only
arms. They are now `Any` rows on the same routines (`flatmap`'s inline arm body became
`Interpreter::dispatch_flatmap_method`), reached through the same MRO walk. `flatmap` takes one mapper, so
`.flatmap(&a, &b)` is `X::Multi::NoMatch`, as in Rakudo; before, the second mapper was silently dropped.

Follow-up (`refactor/11276-any-junction-rows`). `any`, `all`, `one` and `none` had only an arm, which built a junction
of the receiver's list. They are now pure `Any` rows (`collections/any_misc.rs`, `TYPE_OBJECT_OK`) found by the guard
step and the MRO walk. A scalar-held list is deitemized first, as the cascade's scalar-transparent entry did.

### 9.135 A pure row's interpreter twin: an owner's rows at one arity are candidates in turn (2026-10-16)

`refactor/11276-any-aggregate-twins`. `Any.min`, `max`, `minmax`, `minpairs`, `maxpairs`, `sort`, `unique` and
`repeated` have a pure `Handler::Narrow` row that answers a plain list without a callback. The by-name cascade kept an
arm per name for everything else: a callback or adverb (`.max(&by)`, `.sort(&cmp)`, `.unique(:as(...))`), and a
receiver the pure row declines (an instance, a user iterator, a type object). The arms called one routine each
(`dispatch_min_max_method`, `dispatch_sort_method`, `dispatch_unique`, ...). A row could not replace them: one owner,
name and arity had one row, the pure one, and the guard step must keep finding it.

Decision. An owner may register a second row at the same name and arity, its interpreter twin
(`collections/any_interp_twins.rs`: `OWNER_ONLY`, `SLURPY`, `ANY_ARGS`, `ANY_NAMED`, `TYPE_OBJECT_OK`), registered
after the pure row. `owner_row`, the guard step's shape tables and the const index keep finding the first row, the
pure one, so every pure path is unchanged. The MRO walk (`invoke_mro`) now tries each of an owner's rows at the call's
arity in turn (`owner_rows_at`), as Rakudo tries a proto's candidates. When the pure row declines or does not bind
the call's adverbs, the twin takes it through the routine the arm called. The eight names join
`any_list_method_by_mro`, and their arms are gone. A call no candidate takes is `X::Multi::NoMatch`
(`.minpairs(1)`), as in Rakudo. Pinned in `t/oo/method/any-list-methods-by-mro.t`.

### 9.136 Late rows: `OWNER_ONLY` interpreter rows only the MRO walk reaches (2026-10-17)

`refactor/11276-any-coercion-rows`. `Any`'s coercions `Seq`, `List`, `Set`, `SetHash`, `Bag`, `BagHash`, `Mix`,
`MixHash`, `Map`, `Hash` and `hash`, `Any.slice`, `Mu.take` and `Bag`/`BagHash.new-from-pairs` were arms in
`dispatch_method_by_name_2`. A plain receiver of most of them is answered natively before the cascade: by the pure
cascades, or by a pure row (`Any.List`, `Any.hash`). A row placed in the guard step's shape tables would have run ahead
of that native answer and changed it.

Decision. They are *late rows* (`collections/any_coerce_late.rs`): `Handler::Interp`, `OWNER_ONLY`, registered after
any pure row of the same owner, name and arity (a §9.135 twin when there is one). The shape tables never list an
`OWNER_ONLY` row, so every native answer is unchanged. Only the by-name cascade's MRO walk (`any_list_method_by_mro`)
reaches these rows, for the receivers and calls the native paths decline. Their handlers are the deleted arms' bodies.

`invoke_mro` also reports `X::Multi::NoMatch` when every row that takes the call's arity turned it down only because
the receiver is a type object. That is Rakudo's `Any:D` candidate not binding `Any:U` (`Int.slice(0, 2)`,
`Int.rotor(2)`), where the walk used to fall through to an unrelated answer. A call with arguments no candidate takes
(`.Set(1)`) is `X::Multi::NoMatch` too. 12 arms leave `dispatch_method_by_name_2`. Pinned in
`t/oo/method/any-list-methods-by-mro.t`.

Follow-up (`refactor/11276-match3-late-rows`). The twins and late rows are a group of their own,
`collections::LATE_FAMILIES`, listed last in `table::GROUPS`. A twin must stand behind a pure row that may live in
another group (`Mu.is-lazy`'s is in `instances`), and only the last group can guarantee that. The late rows gain
`toggle`, `tree` with a depth spec, `is-lazy`, `List`/`Seq.rotate`, `Thread.join` and `Any.join`. `Any.join` is an
only-method, so its row takes every arity and reports a surplus separator as Rakudo's arity error (`Too many positionals
passed`). `Any.slice` keeps a single implementation, the late row. Its duplicate in `dispatch_method_by_name_3` was
unreachable behind the deleted `dispatch_method_by_name_2` arm and is gone. The late row now flattens its indices and
ends at the first index past the end, as Rakudo does, and an index that does not ascend dies with Rakudo's
`Provided index N, which is lower than M`.

Follow-up (`refactor/11276-match2-rest`). The rest of `dispatch_method_by_name_2` follows: `elems` (an `Any` twin),
the quant hashes' `Setty`/`Baggy`/`Mixy` (late rows of the six owners Rakudo's `QuantHash` composes them into), and a
`Supply`'s `list`/`Array`. `Supply.list`'s row becomes an interpreter row, `Interpreter::supply_materialize`. It covers
the pure `listify`, the drain of a live channel-backed supply, and the stateful collection of an on-demand one. `Array`
is `Any.Array`'s twin for a `Supply`. The MRO walk takes `list`/`Array` only for a `Supply`, so every other receiver's
list coercion keeps its native answer. Left in that function: `pop` (a `Mut` row's concern) and `snip`.

Follow-up, same branch. `dispatch_method_by_name_1`'s answers for any receiver become late rows too
(`collections/mu_late.rs`): `Mu.say`/`print`/`put`/`note` without an argument, `Mu.return-rw`, `Mu.can`, `Mu.does`,
`Any.are` and `Array.shape`. `Cool.printf`/`sprintf` and the `polymod` rows already existed; their arms only covered
receivers without a shape, and the MRO walk now reaches those rows instead. The walk lets a receiver whose MRO lacks
the owner fall through, so `Plain.new.sprintf` and `(1, 2).shape` are `X::Method::NotFound`, as in Rakudo, where the
receiver-blind arms answered. `Cool.printf`/`sprintf` take a type object (`Int.sprintf('%s')` is Rakudo's
directive-count error). A `say`/`put` with arguments is a class acting as `$*OUT` and stays the cascade's.

### 9.137 `HyperSeq`'s and `RaceSeq`'s methods are rows; three copies become one (2026-10-17)

`refactor/11276-instance-call-rows`. A hyper or race sequence's own methods (`hyper`, `race`, `configuration`,
`is-lazy`, `iterator`, `map`, `grep`) had three implementations: a by-name block in `call_method_with_values` and one
in each VM call path (`CallMethod`, `CallMethodMut`). The copies had drifted. `CallMethod`'s `map`/`grep` ran small
sequences on the worker pool, `CallMethodMut`'s ran them sequentially, and only `CallMethod` answered `configuration`,
so `my $h = (1..6).hyper(:batch(2)); $h.configuration` read the `List` and died. `.hyper(:batch, :degree)` was a
fourth copy in each of the three places.

Decision. The seven methods are `OWNER_ONLY` rows of `HyperSeq` and `RaceSeq` (`instances/hyper_seq.rs`, owners via
`hyper_seq_owners_of`), with one implementation each in `runtime/methods_hyper_seq_rows.rs`. `map`/`grep` take the
worker pool below 1000 elements and the sequential list path above, reifying each element. All three entry points
ask the rows first. Each keeps only the pseudo-methods (`^name`, `WHAT`, `defined`, `isa`/`does`) and the `List`
read of every other method. `.hyper`/`.race` with adverbs is one routine, `hyperize_with_config`, and its error is
Rakudo's (`Invalid value '0' for :batch on method hyper`). The recognition table and the oracle snapshot gain the two
owners. The snapshot lines were generated by `scripts/gen-rakudo-method-tables.raku` and merged in, not regenerated
whole.

### 9.138 A cascade's last resort walks the MRO for late rows only (2026-10-17)

`refactor/11276-instance-ops-rows`. Two blocks of `call_method_with_values` still answered by name after every other
answer: the trailing `Mu` arms of `dispatch_instance_and_fallback` (`DUMP`, `gist`, `raku`/`perl`, `isa` of a type
object, `REPR`, `Str`/`Stringy`, `Int`/`Numeric`/`Real`, `EVAL`), and an `Array` block for the positional protocol
with several indices, a shaped array's bounds and a native array's refusals. The §9.129 walk (`invoke_mro`) did not
fit either place: it retries an owner's pure rows, which every earlier step already declined, and reports
`X::Multi::NoMatch` where the cascade must go on.

Decision. `invoke_mro_late` walks the receiver's MRO like `invoke_mro` but takes only `OWNER_ONLY` interpreter rows,
and a call none binds is `None`. The two blocks become late rows (`collections/mu_render_late.rs`,
`collections/any_pos_late.rs`) that this walk reaches from the same place. The routines are
`runtime/methods_mu_late_rows.rs` and `runtime/methods_array_pos_rows.rs`. The positional rows are `Any`'s (Rakudo's
`**@indices` candidates). `EVAL` is `Cool`'s, so `Blob.new(49).EVAL` is now `X::Method::NotFound`, as in Rakudo,
where the receiver-blind arm evaluated it. `no_row_is_registered_twice` allows one late row behind a first row of any
handler, as long as the first is not itself a late row (`Mu.Stringy`'s first row is an interpreter row). The
recognition table gains `Mu.DUMP`, `Mu.perl`, `Mu.REPR`, `Cool.EVAL` and `Any.ASSIGN-POS`/`BIND-POS`/`DELETE-POS`.
Left in place: the `name` arm, whose answers (`Nil` for an `Array` or `Hash`, an error for a `Code` type object) model
container descriptors mutsu does not have yet.

### 9.139 The tail of `dispatch_method_by_name_3` is late rows of the receiver's MRO (2026-10-17)

`refactor/11276-match3-tail-rows`. The by-name match at the end of `dispatch_method_by_name_3` answered, per receiver:
a `Seq`'s `Numeric`; the `Supply` type object's `signal` and `grab`; the temporal type objects' `Date`/`DateTime`;
`Thread.run`; the `Str`/`Stringy` of a list whose elements need the interpreter, and of a regex; and the `keys`,
`values`, `AT-KEY`, `EXISTS-KEY` and `EXISTS-POS` of a `Stash`, an object hash, a `Match`, a `Capture` and a `Pair`.

Decision. The match becomes the §9.138 walk (`invoke_mro_late`) over the receiver's MRO, without `Mu`, for those names.
`Mu`'s late rows are the cascade's last resort and must not answer ahead of the user methods this step precedes. The
rows are in `collections/match3_late.rs`, with owners Rakudo declares: `Seq.Numeric`; `Date`'s and `DateTime`'s `Date`
and `DateTime`; `Thread.run`; `List.Str`, `Seq.Str` and `Seq.Stringy`; and `Any`'s `keys`, `values`, `AT-KEY` and
`EXISTS-KEY`. `Any.EXISTS-POS`'s late row (§9.138) also answers a `Match`'s positional capture. The type-object `grab`
joins `Supply`'s existing `grab` row, which now admits the type object. `Thread.run` is no longer a native method of
`Thread` (`hardcoded_native_method`), because the native path's receiver is a copy rebuilt from the attributes, and
`run` answers the invocant itself.

Removed rather than moved: `Supply.signal`, which Rakudo does not have (`signal(...)` is a routine). The arm built an
empty supply that never fired. The regex `Str`/`Stringy` arm was already unreachable, since `Code`'s rows answer a
regex first.

### 9.140 `Metamodel::EnumHOW`'s metamethods and `WalkList`'s methods are rows (2026-10-17)

`refactor/11276-enumhow-walk-rows`. Two by-name matches answered a metaobject's and an instance's own methods:

- `dispatch_enumhow_method`, asked by `dispatch_classhow_method` ahead of the shared `Metamodel::*HOW` rows, covered
  `enum_values`, `elems`, `enum_from_value`, `enum_value_list`, `add_enum_value`, `set_export_callback`,
  `compose_values`, `is_composed` and `compose`.
- `try_walk_list_method` covered the `WalkList` of `.WALK(...)`: `invoke`, `reverse` and `quiet`.

Decision. Both become rows of their owners, following slice 3G and §9.137:

- The EnumHOW rows (`ctors_mop/enum_how.rs`) are `Metamodel::EnumHOW`'s and are reached by `invoke_owner_raw` over
  `ENUM_HOW_OWNERS`, in the same place and ahead of `MOP_OWNERS`. The type object is the first argument, so each row's
  arity counts it.
- mutsu's HOW is one dispatcher over every kind, so the rows still see a type that is no enum. The queries report the
  EnumHOW-only error, `elems` answers `Any.elems`'s 1, and `is_composed`/`compose` decline to the ClassHOW rows, as
  the arms did.
- The `WalkList` rows (`instances/walk_list.rs`) are reached by `invoke_owner` for a `WalkList` instance.
- Each routine is one `Interpreter` method. The recognition table gains the twelve names, `DECLARED` but not
  `ONLY_METHOD`, since the Rakudo snapshot lists neither owner.

### 9.141 The constructors and `Failure.handled` are late rows of the receiver's MRO (2026-10-17)

`refactor/11276-dispatch-new-rows`. `dispatch_new_and_constructors` was a by-name match, reached after the by-name
cascades. It covered:

- `Range.new` and `IO::ArgFiles.new` on their own type objects;
- `Failure.handled` (read and set);
- `new-from-pairs`;
- `new` and `bless` for every other receiver;
- the qualified `Mu::new`.

Decision. The match becomes the §9.138 walk over the receiver's MRO, with `Mu` appended when the MRO lacks it. The rows
are in `ctors_mop/ctor_late.rs`: `Range.new`, `IO::ArgFiles.new`, `Failure.handled`, the `Set`/`SetHash`/`Mix`/`MixHash`
`new-from-pairs` (`Bag`'s already were rows, §9.136), and `Mu`'s `new` and `bless`. `Range.new` and `IO::ArgFiles.new`
answer their own type object only, so a subclass stays `Mu.new`'s, as the arms' exact-name guards had it. A call of no
QuantHash's `new-from-pairs` (`Int.new-from-pairs`) is now `X::Method::NotFound`, as in Rakudo, where the match handed it
to a routine that reported "not supported". `Mu::new` is a qualified call, not a method name, and stays one test.

Follow-up. A native path that reaches its type's rows by `invoke_owner(&[T, "Mu"], ...)` answers on a receiver
rebuilt from the attributes. Such a path must not take a `Mu` late row whose answer depends on the receiver itself.
`native_io_special` keeps its own constructor (`IO::Special.new("<STDOUT>")`) out of that lookup, as `Thread.run` was
kept out of `Thread`'s native methods (§9.139).

### 9.142 `Mu.say`/`put` with arguments, `Junction.THREAD` and `Cool.encode` are late rows (2026-10-17)

`refactor/11276-match1-late-rows`. Three arms of `dispatch_method_by_name_1` became late rows
(`collections/match1_late.rs`), reached by the §9.138 walk right after the `Any` list walk (§9.134):

- `say`/`put` with arguments, for a class that declares its own `print` and so acts as `$*OUT`;
- `THREAD` on a `Junction`;
- `encode` for every receiver the `Str` rows did not take.

The owners are Rakudo's: `Mu`, `Junction` (an `only` method) and `Cool`. `Mu.say`'s and `Mu.put`'s one-argument late
rows stand behind their deferral bases (`mu_base`'s `DEFERRAL_BASE` rows), which only `invoke_base` reaches. So
`no_row_is_registered_twice` lets a late row follow a deferral base too.

Not moved, because mutsu's MRO for them is not Rakudo's yet:

- `Buf[uint8]`'s MRO lacks `Blob`, so `decode` and `unpack` would lose `Blob`'s owner;
- a QuantHash's MRO lacks `Setty`/`Baggy`/`Mixy`, which `default` belongs to;
- an enum type's MRO lacks `Enumeration`, which `enums` belongs to.

Walking those MROs for the Rakudo owner would drop answers. Those arms wait for the MRO to carry the roles.

### 9.143 A late walk reads the roles a built-in class composes (2026-10-17)

`refactor/11276-role-owner-walk`. §9.142 left `decode`, `unpack`, `default` and `enums` as arms, because mutsu's MRO for
their receivers is the class chain only. Rakudo answers each from a role: `Buf[uint8]`'s `unpack` from `Blob[uint8]`, a
`Set`'s `default` from `Setty`, an enum's `enums` from `Enumeration`. The catalog already records the roles each
built-in class composes (`builtin_type_info(..).roles`, `composes_role_at`). The method table's compile-time index
flattens them in the same way.

Decision.

- `Interpreter::late_walk_owners` (`runtime/methods_late_walk_owners.rs`) is the owner list for a late walk. It holds
  the receiver's MRO, with each class followed by the built-in roles it composes (their parameters stripped, `Blob[uint8]`
  → `Blob`) and ahead of its parent, as Rakudo's resolution flattens a role's methods.
- An enum (a declared enum's type object or value, and `Bool`, the core's own) has `Enumeration` right after its type.
- `dispatch_method_by_name_1`'s late walk reads it. `Blob.unpack` and `Enumeration.enums` become late rows there
  (`collections/match1_late.rs`).
- The `decode` and `default` arms are deleted rather than moved. Their receivers are already answered by `Blob`'s and
  `Buf`'s `decode` rows and by the `Array`/`Hash`/QuantHash `default` rows, which the shape lookup reaches, so the arms
  were unreachable.
- The other late walks keep the class MRO. None of their rows is a role's.

### 9.144 A grammar's `parse` family is `Grammar`'s rows from every call site (2026-10-17)

`fix/13731-grammar-parse-single-new`. Two routines sat behind `dispatch_method_by_name_1`'s by-name
`parse`/`subparse`/`parsefile` arm and `Grammar`'s rows (§9.46):

- `dispatch_package_parse` answered a type object;
- `dispatch_instance_parse` answered an instance and the rows. It called a grammar's own `new` and then
  `dispatch_package_parse`.

The start rule already runs on `self.new` (the `grammar-start-rule-built-invocant` test pins it), so the explicit call
was a second `new`. An instance parse and a `nextsame` from a user `parse` ran the user `new` twice where Rakudo runs it
once (#13731).

Decision. `dispatch_instance_parse` is deleted, and `Grammar`'s rows call `dispatch_package_parse`. The by-name arm
becomes one `invoke_owner_raw(&["Grammar"], ...)` for a type object or an instance whose grammar declares no method of
that name. A package that is no grammar is the rows' to decline, as an instance that is no grammar was the arm's.

### 9.145 An enumeration's key/value views and `Mu.set_why` are rows (2026-10-17)

`refactor/11276-enum-format-late-rows`. Three arms named enum receivers by method name:

- a block of `call_method_with_values` answered `pairs`/`keys`/`values`/`kv`/`antipairs`/`invert` of
  any type object (an enum type's values, `()` for the rest);
- the `enum_value_list` arm of `dispatch_method_by_name_1` answered a method Rakudo does not have
  (only `.^enum_value_list`, the `Metamodel::EnumHOW` row of §9.140);
- its `invert` arm inverted a `Str` that named an enum type, which in Rakudo is a `Str` and dies.

An enum's *value* had no answer of its own: `red.pairs` was `(0 => red)` and `red.invert` died, where
Rakudo lists the whole enumeration. Rakudo declares the views on `Any`, with an `Any(Enumeration)`
candidate (`self.enums.METHOD`), an `Any(Bool)` one and an `Any:U` one (`()`); a defined value's `kv`
is `Enumeration.kv`, its own key and value.

Decision. The candidates are `Any`'s rows in `collections/any_enum_views.rs`: `Handler::Interp`
(the enumeration comes from the registry), `OWNER_ONLY`, `TYPE_OBJECT_OK`. One routine, `view`, builds
every answer from the enumeration's `(key, value)` list as a `Seq`; the pure `Any` rows' `Bool` cases
call it with `Bool`'s list, so `True.invert` now answers and the order of the `Bool` views is the
declaration order. The pure `Any` rows decline an enum value as they decline a type object, and the
rows are reached from those two places: the type-object block becomes one call, and
`dispatch_enum_method` asks them after the enumeration roles' and the base type's rows. An `Array`-
valued value's `keys`/`values`/`pairs` no longer go to the base type (`small.keys` is the
enumeration's keys, as in Rakudo); its `elems` still does. The two non-Rakudo arms are deleted.
`Interpreter::enumeration_variants` is the one reader of an enumeration's values; `dispatch_enums`
(`Enumeration.enums`) reads it too.

`Mu.set_why($why)` is a late row of `Mu` in `match1_late.rs`, reached by the same MRO walk as
`Enumeration.enums`; it declines a metaobject receiver, whose `set_why` is the meta-level route. The
recognition table gains `Mu.set_why` (Rakudo declares it), so `Mu.^can('set_why')` is true.

### 9.146 Rows of language revision 6.e (2026-10-17)

`refactor/11276-enum-format-late-rows`. Rakudo declares some methods only in its `core.e` setting:
`Any.snip`/`snitch`, `Supply.snip`, `Cool.nomark`/`Str.nomark`, `IO::Path.stem`, and others. mutsu
had no notion of a revision in the method table. `snip` was a cascade arm that answered under 6.d
too, `snitch` was an arm that checked the revision itself, `nomark` and `stem` were missing, and
`.^can` denied all of them under 6.e because the recognition catalog, generated under 6.d, had no row
for them.

Decision.

- **A row flag, `RowFlags::REVISION_E`.** The method exists only when the program's language revision
  is 6.e. `RowFlags::in_force` reads the revision (the thread-local the parser sets, which
  `raw_invocant` and `snitch` already read at run time). Every owner lookup (`owner_row`,
  `owner_rows_at`, `owner_declares`) skips a row that is not in force, so under 6.d the call finds no
  method (`X::Method::NotFound`, as in Rakudo) and no "declared but no candidate" error. The shape
  tables are built at compile time and cannot be skipped per revision, so a `REVISION_E` row is
  `OWNER_ONLY`; the test `revision_e_rows_are_owner_only` enforces it.
- **A catalog flag, `NativeRowFlags::REVISION_E` (bit 6).** Every reader of the recognition catalog
  goes through one `entry` lookup (or filters on `in_force`), so `.^can`, `.^methods`, `.^lookup` and
  the `augment` conflict check see the name only under 6.e.
- **An oracle for the 6.e additions.** `scripts/gen-rakudo-method-tables.raku` runs
  `scripts/rakudo-method-tables-e.raku` as a child process (a compilation unit raises its revision with
  its first statement, and an `EVAL` cannot) and appends `declared.e`/`methods.e`/`only.e` lines:
  the names 6.e adds, for the owners whose 6.e lists only add names (under 6.e `::('Exception')`
  does not resolve to the class, so such owners are left out). A `REVISION_E` catalog row is checked
  against those lines, and `revision_e_rows_are_6e_additions` (always run) requires it to be one.
  The committed 6.d lines are unchanged; only the `.e` lines were appended.
- **The rows** are late rows in `collections/revision_e_late.rs`, reached by the MRO walk of
  `dispatch_method_by_name_1`: `Any.snip` (slurpy, the routine `eval_snip`), `Any.snitch`
  (`dispatch_snitch`), `Supply.snip`, `Cool.nomark`/`Str.nomark` (the new `str_prim::nomark`, the
  `:m` fold of `str_prim::Fold`, so the adverb and the method share one implementation) and
  `IO::Path.stem` (the basename minus its last `$parts` extensions, `$parts` taken as a number: a
  `Range` counts its elements, as Rakudo's does). The `snip` and `snitch` arms are deleted.

Not done: 6.e methods that change a method 6.d already has on a type with a shape (`Int.pick`,
`Int.roll`, `Complex.sign`) would have to shadow a shape-table row per revision; `Mu.Callable` and
`Grammar`'s 6.e additions; `Format` and `Formatter`, whose owners the catalog does not list. They are
filed as #13738.

### 9.147 The last by-value copies of the array mutators (2026-10-17)

`refactor/11276-pure-cascade-residuals`. §9.23 left the `Array` mutators with copies outside their `Mut`
rows. Two of them needed no new mechanism, only the detached place §9.23 already built:

- **The `pop` arm of `dispatch_method_by_name_2`** (`dispatch_pop_method`) popped any receiver
  that reached it. For an array it repeated the row's rules: the arity error, the lazy refusal and
  the empty-array `Failure`. For anything else it answered the empty-array `Failure` too, so
  `Foo.new.pop` "popped" an object that has no `pop`. The arm is deleted. An array held by value
  (`f().pop`, `$x.pop`, `%h<a>.pop`) already reaches the row through `invoke_mut`. Any other receiver
  is `X::Method::NotFound`, as in Rakudo.
- **`proxy_subclass_array_mutate`** (an array attribute of a `Proxy` subclass, `$p.VAR.log.push`) had a
  `match` over the six mutator names with its own `append`/`prepend` flattening. It now runs
  `invoke_mut` on the stored array as a `Detached` place and writes the array back. The storage lock is
  not held across the row, which may run user code.

Three call sites each listed the mutator names they forward to `invoke_mut`: the Proxy-subclass
interception in the interpreter and in the VM, and the mixin over a native array. They now ask the
table (`names_a_mut_row`, no longer test-only), so `splice` and `grab` are forwarded there too.

An enum value's base-type rows (`dispatch_enum_method`) are its base type's and `Cool`'s, as Rakudo's
MRO has them (`Int`/`Str`, then `Cool`). The `sign` arm of the pure cascade, which answered an enum value
from its integer, is deleted. `Cool.sign` numifies the value, so a `Str`-valued value that is not a
number dies as it does in Rakudo; the arm answered 0 for it.

### 9.148 `Any`'s list methods read the receiver's own `iterator` or `list` (2026-10-17)

`refactor/11276-own-iteration-view`. Rakudo writes `Any`'s list methods on two primitives:

- `self.iterator`: `head`, `tail`, `first`, `skip`, `grep`, `map`, `sort`, `min`, `max`, `minmax`, `sum`,
  `unique`, `squish`, `rotor`, `batch`, `Seq`, `repeated`, `toggle`;
- `self.list`: `join`, `elems`, `reverse`, `kv`, `pairs`, `keys`, `Array`, `List`, `list`, `cache`, `eager`,
  `flat`, `Slip`, `classify`, `categorize`, `combinations`, `permutations`, `end`, `antipairs`,
  `maxpairs`, `minpairs`.

`Any.iterator` is `self.list.iterator`. So an object that declares `iterator` answers the first set from its
elements, and one that declares `list` answers both sets. Neither has to compose `Iterable`. Both sets were
measured on Rakudo 2026.09 with a plain class declaring only one of the two methods. A method counts as "through
the primitive" when the object's answer equals the elements' answer and differs from the one-item list's.

mutsu modelled this as four name lists at four places in `call_method_with_values`:

- `Backtrace`'s frames (12 names);
- an `IterationBuffer`'s items (9 names);
- a buffer-backed `CArray`'s `.list` (7 names);
- a user `iterator`, or a `does Iterable` class's `list` (6 names, plus a mixin's `iterator` for 3).

The method table's shapeless walk (`shapeless_row`) also answered the instance as one item for the names in
`ANY_LIST_FOR_CLOSED_SHAPES`. Measured on the two classes, mutsu agreed with Rakudo on 6 of 18 names for
`iterator` and 12 of 39 for `list`.

Decision. `runtime/methods_own_iteration.rs` holds the two sets and one step, `own_iteration_view`, which
replaces the four places. The step decides where the receiver's elements come from:

- a list: a user `list`, `Backtrace`'s frames, a buffer's `.list`;
- an iterator: a user `iterator`, an `IterationBuffer`'s items, a mixin's `iterator`.

It redispatches the method on those elements, unless the receiver declares the method itself. `shapeless_row`
declines a user instance whose class declares `iterator` or `list`, so the step sees it. Now mutsu agrees with
Rakudo on every name of both sets.

Not changed: `values` of a `list`-only class is a `Seq` of the list, where Rakudo answers the `List` itself
(`Any.values` is `self.list`). List assignment keeps its own rule (`try_iterable_instance_items`), which requires
`Iterable` for a `list`-only class, as Rakudo's does. `for` read the same rule; since #13748 it iterates a
`list`-only class, as Rakudo's `.map` does.

### 9.149 6.e methods over a shaped type's 6.d answer (2026-10-17)

`refactor/13738-revision-e-shape-rows`, part 1 of #13738. §9.146's `REVISION_E` rows are `OWNER_ONLY`: an
owner walk skips them under 6.d, but no shape lookup sees them. That covers a name 6.e adds. It does not
cover a method 6.e adds to a type that has a shape, over a method the shape already answers from an
ancestor under 6.d:

- `Complex.sign`: under 6.e it is the unit complex number; under 6.d it is `Cool.sign`, which refuses a
  non-real `Complex`.
- `Int.pick` and `Int.roll`: under 6.e they sample `^self`; under 6.d they are `Any.pick`/`roll`, which sample
  the one-item list `(self,)`.

Decision. `resolve` (the guard step's lookup) asks one more thing before the shape's entry. If the program's
revision is 6.e and the receiver is an instance, it looks for a `REVISION_E` row that the shape's own type
declares for the name and arity, and that row answers instead of the entry. A memoized bit per row name says
whether any `REVISION_E` row has the name, so every other lookup pays one bit test. The resolved row is cached
per call site like any other: the revision is fixed for the program. The rows are `scalars/revision_e.rs`
(`Handler::Narrow`; `Int.pick`/`roll` call the one `sampling::pick`/`roll` on `^self`). `REVISION_E` joins the
flags the debug cross-check skips, because the cascades know no revision. The recognition catalog is unchanged:
6.d already recognizes these names on these owners. So the declared-by-Rakudo test checks a `REVISION_E`
method row against the snapshot's `declared.e` lines instead of the catalog.

Rakudo 2026.09's own `Int.pick` crashes the process on `0.pick` and answers a garbage integer for `(-3).pick`.
mutsu's row reads a non-positive receiver as the empty range.

Left on #13738: `Format`/`Formatter`/`Formatter::Syntax` (their owners need catalog rows and a representation
decision), `Mu.Callable` and `Grammar`'s 6.e additions.

### 9.150 A qualified `Mu::` call is `Mu`'s base row (2026-10-17)

`refactor/11276-qualified-mu-base`. `$o.Mu::gist`, `.Any::Str` and the like were answered by two `match`es in
`methods_qualified.rs`, one for a type object and one for an instance. Their answers disagreed with Rakudo:
for an instance, `Mu::gist` printed the generic `C.new(...)` repr where Rakudo's `Mu.gist` is
`self.raku` (the receiver's own `raku`), and `Mu::Str` answered `Mu<id>` where Rakudo's is `C<id>`.
§9.56 had already made `Mu`'s base answers rows: `defined`, `Bool`, `so`, `not`, `WHICH`, `WHERE`,
`gist`, `Str`, `raku` and the output methods. Those rows are the last candidate of a deferral chain, which is
what a qualified call to `Mu` means: `Mu`'s own method, whatever the class overrides.

Decision. After the existing checks (no arguments, a `Mu`/`Any`/`Cool` qualifier, `Cool` in the MRO), the
qualified call is `invoke_base(Mu, method)` on the receiver. `Any` and `Cool` declare none of these names, so
their qualified calls reach `Mu`'s too. `perl` maps to `raku`, and `Stringy` to `Str`. Both `match`es are
deleted. The answers now match Rakudo's for `gist` and `Str` of an instance (pinned in
`t/types/coercion/qualified-mu-coercion-base-rows.t`). The qualified call also gains `so`, `not`, `WHERE`
and the output methods.

### 9.151 A junction's own methods are rows; one rule for what autothreads (2026-10-17)

`refactor/11276-junction-rows`. A junction has no shape (§9.66). A method called on one either autothreads over
its values or is answered by the junction itself. mutsu kept three lists of the names the junction answers
itself, one per call path (`call_method_with_values`, the VM's `CallMethod`, the VM's `CallMethodMut`), and they
disagreed: the `CallMethod` list lacked `Str`. The answers were a `match` in `call_method_with_values` that
rendered `gist`/`raku`/`perl`/`Str` as a string. Rakudo is different in several places: `Junction.Str` is
`self.THREAD(*.Str)` (a junction of `Str`s); `.isa`, `.does`, `.can` and `.not` are `Mu`'s and ask the junction
itself; `.sink` is `Nil`.

Decision.

- `Junction`'s own methods are rows, in `collections/junction.rs`:
  - `gist` and `raku`, rendering each value by its own `gist` (`Nil` for a `Nil`) or `raku`;
  - `Str`, which threads;
  - `sink`, which is `Nil`.
  The catalog gains `Junction.raku`, `Str` and `sink`. One helper, `Interpreter::junction_own_method`, reaches
  them by owner from all three call paths, ahead of autothreading; `perl` is `raku`.
- `Interpreter::junction_invocant_autothreads` is the one list, measured against Rakudo 2026.09:
  - the pseudo-methods;
  - `Junction`'s non-threading methods: `Bool`, `defined`, `WHICH`, `gist`, `raku`, `perl`, `Str` (threads
    in its row), `sink`, `THREAD`, `map`;
  - `Mu`'s methods about the object: `so`, `not`, `isa`, `does`, `can`, `DEFINITE`, `WHERE`, `say`, `note`.

  Every other method autothreads. That includes `put` and `print`, which write each value, and `Junction`'s
  numeric coercions, whose Rakudo bodies thread.
- `any_list_method_by_mro` lets a junction reach `Mu`'s late rows (`can`, `does`) for the names the junction
  answers itself.
- The argument-position rule (`should_autothread_method`, a junction passed as an argument) is not changed.

`t/types/junction.t` asserted the old `$j.Str eq "any(1, 2, 3)"`, which fails under Rakudo too. It now checks the
threaded `Str`'s gist.

### 9.152 `Mu.perl` is `self.raku`; the scalar shapes leave the rendering cascade (2026-10-17)

`refactor/11276-render-cascade` (the rendering names, plan item 2.1).

**`perl`.** Rakudo declares `perl` once, on `Mu`, as `method perl(|c) { self.raku(|c) }`. mutsu treated the two
names as aliases wherever either was answered: about a hundred `"raku" | "perl"` tests across the cascades, the
VM's call paths, the native bypass gate and the rows. The copies had drifted. `default_instance_repr` needed its
own branch so that a class with only a user `raku` rendered through it under `.perl`, and two rows mapped `perl`
to `raku` by hand.

Decision. The call entries resolve the name once: `CallMethod`, `CallMethodMut` (and their dynamic forms, which
delegate to the same bodies) and `call_method_with_values` call `Interpreter::perl_as_raku`. A call to `perl`
dispatches as `raku` unless a user-declared `perl` is in reach of the receiver. That covers a class's own, one
from a role it composes or one mixed into it, an `augment` of a built-in type, and a `.wrap` (any wrap keeps the
name, since a wrap chain can sit on `Mu.perl`). A modified call (`.^perl`, `.+perl`) keeps its name. Nothing below
the entries sees `perl` otherwise, so every `"perl"` alias in the dispatch code is deleted. The lists that answer
whether a method *exists* keep `perl`: `.^can`, role requirements, the native-method names of a built-in class,
the EVAL name table and the recognition table. `Mu.perl` keeps two rows, both calling `self.raku` as Rakudo's
does: the late row (`.^find_method('perl')`) and a new deferral-base row (§9.56), which is the last candidate of a
user `perl`'s `callsame` (`method perl { "P:" ~ callsame }` was `P:` with a Nil warning). Pinned in
`t/types/mu-perl-calls-raku.t`.

**The scalar shapes' renderings.** `methods_0arg/dispatch_core_repr.rs` answered `gist`/`raku` for `Bool`, `Int`,
`Num`, `Rat`, `FatRat`, `Complex`, `Str` and `Version`, every one a shape with rows. Its arms were reached only
when a row declined: a zero-denominator rational, which the rows declined, and `FatRat`, which had no rendering
rows. They were also reached when a caller ran the cascade without the table (the lever-A refusal, the debug
cross-check). The zero-denominator forms (`<1/0>`, `FatRat.new(1, 0)`, `X::Numeric::DivideByZero` for `gist`) move
into `scalars::render`. `FatRat` gains `gist`/`raku` rows on the same handlers, since Rakudo composes them into
`Rat` and `FatRat` from the one `Rational` role. The cascade's arms for these shapes are deleted, and its
`to_string_value` catch-all now declines any receiver with a shape, so a shape without a rendering row is never
answered by stringification (`Capture.raku` read as its `Str`). Pinned in `t/types/fatrat-render-rows.t`.

### 9.153 `Format` is a `Str` subclass with rows; `Formatter`'s are rows (2026-10-17)

`refactor/13738-format-rows` (#13738, item 2).

6.e's `Format` is `class Format is Str does Callable`. mutsu modelled it as an instance with a `format` attribute and
answered its methods in one by-name `match` (`dispatch_format_method`: `Str`/`gist`/`Stringy`, `arity`/`count`,
`Callable`, `raku`, `CALL-ME`, plus `Formatter.new`/`CODE`/`AST`, `Formatter::Syntax.parse` and `Format.new`). It was
not a `Str`: `$f ~~ Str` was `False`, and `$f.chars` and `$f.uc` were `X::Method::NotFound`.

Decision.

- A `Format` is a native `Str` subclass value. Its string is the `Str` payload (`__mutsu_str_value`) that a user
  `is Str` subclass carries, so every method it inherits answers on the string through `builtins::native_base`.
  The type catalog gains `Format` (`Format, Str, Cool, Any, Mu`, roles `Stringy` and `Callable`) and `Formatter`.
  Rakudo also lists `Callable` in `Format.^mro`; here it is a role, as everywhere in the catalog.
- `Format` and `Formatter` are closed dispatch shapes; `Formatter` has type objects only. `Format`'s own methods are
  rows: `raku`, `arity`, `count`, `directives`, `Callable`, `CALL-ME` and `AST`. `Formatter`'s `CODE` and `AST`
  are type-object rows. `native_base` declines a method that the receiver's own shape has a row for, so the payload
  answers only what `Format` does not declare. The debug cross-check caught `.AST` answered by `Str.AST` on the
  string. `call_method_with_values` reaches the same rows by owner.
- Both `new`s are `native_ctor` entries.
- The recognition catalog gains the rows as `REVISION_E`. The oracle snapshot gains `Format`'s and `Formatter`'s
  `.e` lines. `scripts/gen-rakudo-method-tables.raku` emits the lists of an owner that only 6.e has.
- `dispatch_format_method` is gone. What stays is `Formatter::Syntax.parse`/`subparse` for the type object, with a
  `TODO`. In Rakudo it is `Grammar.parse` running the format grammar's rules; mutsu answers a `Match` of the whole
  string. The fix is a real grammar, not a row: Rakudo declares no `parse` on `Formatter::Syntax`.
- `Format.WHICH` stays `Mu`'s object identity, where Rakudo's is the `Str` value's (`Format|x`).
- `Format` still exists under 6.d in mutsu, as before. Rakudo has it only under 6.e.

A declared class that shadows a built-in name (`class Format {}`, `my class Instant {}`) was given the built-in's
ancestry. `compute_class_mro` supplies a catalog type's real parent for an `augment` with no `is` clause, and it
did the same for a new declaration: `class Instant {}` was `Instant, Cool, Any, Mu`. The smartmatch also matched
the class's own name against the built-in's static rules (`Instant.new ~~ Cool`). Now `register_class_decl` gives
such a class `Any` as its parent, and `type_matches_value` matches its own name only against itself. Pinned in
`t/types/string/format-str-subclass-rows.t`.

### 9.154 The shapeless subscript protocol is rows (2026-10-17)

`refactor/11276-shapeless-subscript`. The last quoted-name arms of the pure one-argument cascade
(`methods_narg/dispatch_1arg.rs`) were the subscript protocol of receivers with no shape:

- `Nil.AT-POS`/`AT-KEY` were `Nil`, and its `EXISTS-POS`/`EXISTS-KEY` were `False`.
- The `Any` and `Mu` type objects' `AT-KEY` autovivified to `Any`, and their `EXISTS-KEY` was `False`.
- A hash or quant hash with no settled shape (a `Map` subclass's raw storage) read its keys through the
  associative rows' implementation.
- An instance with no positional protocol answered `AT-POS` as the one-element list holding itself, through `Any`'s
  row called by name.
- A native-backed `CArray` answered `AT-POS` from its buffer elements.

§9.66 kept `Nil` without a shape, so these could not be shape rows.

Decision. `answer_shapeless` reaches them as rows; it is the step every call site with an interpreter takes after
the cascades.

- **`Nil`** declares `AT-POS` and `AT-KEY` in Rakudo (`only` methods answering `Nil`), so they are rows of `Nil`
  (`any_subscript::NIL_ROWS`), and the recognition table gains them. `shapeless_row` admits a `Nil` receiver and
  picks its own rows first. Its `EXISTS-POS`/`EXISTS-KEY` are `Any`'s rows, whose `EXISTS-POS` now counts `Nil` as
  undefined.
- **A built-in type object** (one the catalog lists, and `Any`) is admitted for `AT-KEY`/`EXISTS-KEY`. `Any`'s rows
  already take type objects. A package stash stays the interpreter's (`CORE::<&say>`). `Mu` is not an `Any`:
  `Mu.AT-KEY` is now `X::Method::NotFound`, as in Rakudo.
- **A hash or quant hash with no settled shape** answers `AT-KEY`/`EXISTS-KEY` from its type's rows (`Hash`, `Set`,
  `Bag`, `Mix` and the mutable forms).
- **An instance** with no subscript protocol of its own reaches `Any`'s row through `shapeless_row`'s MRO walk.
  It takes `Any`'s row wherever the walk finds it, and falls back to it when nothing in the MRO declares the method.
  The fallback is needed because the registered MRO of `class P does Positional` is `P, Positional`, without `Any`
  (#13769). A class declared `is Mu` has none.
- **A `CArray`** over a buffer is a `Blob` shape receiver (§9.107).
- **`Any:U.EXISTS-POS(|)`** binds any index without coercing it. `index_arg` answers `False` for a type object or
  `Nil` before it calls the index's `.Int`, so `Nil.EXISTS-POS("x")` is `False`.

`dispatch_1arg`'s `Nil`, type object, unshaped-hash, instance and `CArray` arms are deleted. Its `AT-POS` and
`EXISTS-POS` arms keep only the list with no settled shape: a lazy or unreified `Seq` and a `LazyList`, read
through `method_table::positional`. `shapeless_row` leaves those receivers to the interpreter (§9.106), so this
arm stays until a `Seq` with consumption state is a receiver the rows can take. Pinned in
`t/collections/subscript/shapeless-subscript-rows.t`.

### 9.155 The `Systemic` classes' methods are rows (2026-10-17)

`refactor/11276-systemic-rows` (3E remainder). `Distro`, `Kernel`, `VM`, `Raku`/`Perl` and `Compiler` are the objects
behind `$*DISTRO`, `$*KERNEL`, `$*VM`, `$*RAKU` and `$*RAKU.compiler`. Their methods were by-name `match`es in
`runtime/native_methods/system.rs` and `compiler_config.rs` (`native_distro`, `native_kernel`, `native_vm`,
`native_perl`, `native_compiler`). Two other things went with them. `runtime_init` held a hand-written
`native_methods` name list per class, which gates the native entry. The `Raku` handler's fallback answered any
attribute by name.

Decision.

- Every method is an `OWNER_ONLY` row of its class (`io_concurrency/systemic.rs`), reached by `invoke_owner` from
  the native-instance entry. Most rows read the attribute of their name. The others call one `Interpreter` method
  each: `platform_library_name`, `request_garbage_collection`, `compiler_instance`, `compiler_verbose_config`, the
  `sys_resources` probes, and the signal table. `Kernel`'s CPU and memory rows and `Compiler.id` take the type
  object. The VM's type-object lane and the `Compiler.new` constructor reach the same code.
- `runtime_init`'s `native_methods` list for these classes is derived from the rows (`systemic::names`), so it
  cannot drift from them.
- A method the class does not declare is `Mu`'s. The custom `raku` arms of `Distro`, `VM`, `Raku` and `Compiler`
  are gone, along with the any-attribute fallback. `VM.properties` stays a row although Rakudo does not declare it
  (RT #124624): roast pins it (`S02-magicals/VM.t`), and `rows_are_declared_by_rakudo` exempts it as it exempts
  `NFC.chars`. The classes now register their attributes in
  Rakudo's order, as `Proc` does, so `Mu.raku` renders them as Rakudo does. `$*VM.raku` was `mutsu.new(...)` and
  `$*RAKU.compiler.raku` was `mutsu.new(...)`; they are now `VM.new(config => ..., ...)` and
  `Compiler.new(id => ..., ...)`. `Kernel` declares its own `raku`, which stays a row.
- The recognition table gains the 63 rows and the oracle snapshot the five owners' lists. Every row name is one
  Rakudo declares on its owner.

Not done: the names Rakudo declares that mutsu does not implement (`Distro.cur-sep`, `Kernel.archname`,
`cpu-cores-but-one`, `cpu-usage`, `VM.own-up`, `remote-debugging`, `Compiler.flavor`, `supports-op`). They were
never answered and are not answered now. Pinned in `t/io/kernel-distro-vm-rows.t`.

### 9.156 The built-in encodings' methods are rows (2026-10-17)

`refactor/11276-encoding-rows` (3E remainder). Three classes answered their methods through by-name `match`es and a
by-name gate:

- `Encoding::Builtin`, what `Encoding::Registry.find` answers, used `native_encoding_builtin`.
- `Encoding::Encoder::Builtin` used `native_encoding_encoder`.
- `Encoding::Decoder::Builtin` used `try_stream_decoder_method`, `native_stream_decoder` and `is_decoder_method`,
  gated in two dispatch entries.

`runtime_init` also held a name list per class.

Decision.

- Every method is an `OWNER_ONLY` row of its class (`io_concurrency/encoding.rs`):
  - `Encoding::Builtin`: `name`, `alternative-names`, `encoder(:replacement)` and `decoder(:translate-nl)`.
  - `Encoding::Encoder::Builtin`: `encode-chars`.
  - `Encoding::Decoder::Builtin`: `add-bytes`, the `consume-*` methods, `set-line-separators`, `bytes-available` and
    `is-empty`.

  A decoder row edits the decoder's state in its own attribute cell (`InstanceSlots`), through the routines the
  `nqp::decoder*` ops run.
- `Interpreter::encoding_owners_of` gives the owners: the receiver's class, or the decoder for a user subclass of it.
  It joins the owner lookup that the interpreter's and the mutating entries already do for `Proc::Async`, `Supply`
  and the rest. The native-instance entry reaches the same rows.
- `runtime_init`'s name lists for the three classes are derived from the rows. The by-name dispatch and its gate are
  deleted.
- `Encoding::Builtin` registers its `name` attribute, so `Mu.raku` renders `Encoding::Builtin.new(name => ...)` as
  Rakudo does. It used to render `Encoding::Builtin.new`.
- A UTF-8 encoder's `encode-chars` answers a `utf8` buffer, as Rakudo's does. It used to answer `Blob[uint8]`.
- The recognition table gains the 14 rows and the oracle snapshot the three owners.

Not done:

- The registry names `utf8` and the UTF-16 forms with a dash (`name` is `utf-8` where Rakudo's is `utf8`), and the
  encoders and decoders key on that spelling. Swapping it would change every consumer.
- A UTF-16 encoder still encodes UTF-8.

Pinned in `t/types/string/encoding-decoder-rows.t`.

### 9.157 The handle classes' methods are rows: `Lock`, `Cancellation`, `Tap`, `Proc` (2026-10-17)

`refactor/11276-lock-tap-proc-rows` (3E remainder). `Lock`, `Lock::Async`, `Lock::ConditionVariable`,
`Cancellation`, `Tap` and `Proc` stand for live OS state by an id into the runtime's tables. Their methods were
by-name `match`es in `runtime/native_methods/` (`native_lock`, `native_condition_variable`, `native_cancellation`,
`native_tap`, `native_proc`, `native_proc_mut`), each behind a hand-written `native_methods` list in
`runtime_init`.

Decision.

- Every method is an `OWNER_ONLY` row of its class (`io_concurrency/lock_rows.rs`, `tap_proc_rows.rs`), reached
  by `invoke_owner` from the owner blocks of the two instance entries (for the classes themselves) and from the
  native-instance entry (for a user subclass). Each `match` arm became one `Interpreter` method (`lock_protect`,
  `lock_lock`, `lock_unlock`, `lock_condition`, `condition_wait`, `condition_signal`, `cancellation_cancel`,
  `cancellation_cancelled`, `tap_close`, `proc_respawn`, `proc_live_stream`). A handler reads the ids off a
  snapshot of the attributes, so a method that blocks or runs user code holds no lock on them.
- `Proc.spawn` and `Proc.shell` left the mutating native entry: the row writes what the re-run changed into the
  instance's shared attribute cell (`write_keys`), as the decoder rows do (§9.156).
- `runtime_init`'s `native_methods` lists are derived from the rows (`handle_names`).
- The names follow Rakudo's declarations. `Lock::Async` is its own owner, with no `condition`.
  `Tap` declares only `close`; mutsu's `Tap.cancel` alias is gone. `Proc.Int` and `Proc.run` are gone too:
  Rakudo has neither, and `Proc.new.Int` dies there. The tap a listening socket answers is now an
  `IO::Socket::Async::ListenSocket`, a `Tap` subclass that declares `socket-port` and `socket-host`, as in Rakudo.
- The recognition table gains the 26 rows and the oracle snapshot the new owners' lists.

Not done. `Lock.protect`'s VM fast path, which writes back to the calling frame, is untouched. The names Rakudo
declares that mutsu does not implement (`Lock::Async.protect-or-queue-on-recursion`,
`with-lock-hidden-from-recursion-check`, `Proc.os-error` as a method, `Proc.sink`) stay unanswered by rows.
`Proc.new.exitcode` is `Nil` here and 1 in Rakudo, and `Proc.command` keeps `IO::Path` words unstringified. Both
are pre-existing. Pinned in `t/concurrency/thread-lock/lock-tap-proc-rows.t`.

### 9.158 The socket classes' instance methods are rows (2026-10-17)

`refactor/11276-socket-inet-rows` (3E remainder). `IO::Socket::INET` (a blocking TCP socket) and `IO::Socket::Async`
(a TCP connection or a bound UDP socket) answered their instance methods through by-name `match`es
(`native_socket_inet`, `native_socket_async`, `native_socket_async_udp`), gated by hand-written `native_methods`
lists.

Decision.

- Every method is an `OWNER_ONLY` row of its class (`io_concurrency/socket_rows.rs`), reached like the handle
  classes' (§9.157): `handle_owners_of` covers both classes. Each arm became one `Interpreter` method:
  - `IO::Socket::INET`: `inet_getpeername`, `inet_close`, `inet_accept`, `inet_print`, `inet_write`, `inet_recv`,
    `inet_read`, `inet_get`, `inet_lines`, `inet_nl_in`. `localport` reads its attribute.
  - `IO::Socket::Async`: `async_socket_close` and `async_socket_supply`, which branch on whether the socket is a UDP
    one; `async_socket_send` (`print`/`write`, TCP only); `async_socket_send_to` (`print-to`/`write-to`, UDP
    only); `async_socket_native_descriptor`. The address methods read their attributes.
- `runtime_init`'s name lists are derived from the rows.
- The names follow Rakudo's declarations. `IO::Socket::INET` has no `say` of its own, so the native `say` alias is
  gone. `getpeername` stays a row although Rakudo does not declare it: roast pins it (`S16-unfiled/getpeername.t`),
  and `rows_are_declared_by_rakudo` exempts it as it exempts `VM.properties`.

Not done:

- `IO::Socket::INET.nl-in` answers a `List` in mutsu's separator order, where Rakudo answers an `Array` in its own.
- The names Rakudo declares that mutsu answers only as attributes or not at all: `backlog`, `encoding`, `family`,
  `host`, `nl-out`, `port`, `proto`, `type`, `native-descriptor`, `listening`, and `IO::Socket::Async.enc`.
- The mutsu-only `IO::Socket::Async::Listener` class, which stands in for the `Supply` Rakudo's `listen` answers.

Pinned in `t/io/socket-inet-rows.t` and `t/concurrency/socket-async-rows.t`.

### 9.159 Three runtime classes' class methods are rows (2026-10-17)

`refactor/11276-loader-class-rows` (3G remainder). `call_method_with_values_inner` matched four class methods by
name and type-object name before any other dispatch: `Rakudo::Internals.REGISTER-DYNAMIC`, `Deprecation.report`,
and `CompUnit::Loader`'s `load-source` and `load-source-file`.

Decision.

- Each method is a class-method row of its owner (`io_concurrency/class_methods.rs`), reached through
  `class_method_owners_of` from the by-name dispatch as the other class methods are (§9.122). The bodies moved
  unchanged into `Interpreter` methods in `runtime/class_methods_loader.rs`: `register_dynamic`,
  `deprecation_report`, `compunit_load_source` and `compunit_load_source_file`.
- The four `if method == ...` checks are deleted from the method-call entry.
- The recognition table gains the four rows, and the oracle snapshot gains the three owners. Every name is one
  Rakudo declares.

Not done: `load-source` still runs through the interpreter's EVAL (its `TODO: compile to bytecode` stays), and
`CompUnit::Loader.load-precompilation` is not answered. Pinned in `t/modules/compunit/compunit-loader-class-rows.t`.

### 9.160 The runtime's result objects answer through one owner lookup (2026-10-17)

`refactor/11276-internal-instance-rows` (3G remainder). `call_method_with_values_inner` handled three result
objects the runtime mints, each with its own check:

- `HyperConfiguration`'s `batch` and `degree` were a by-name attribute read.
- `Routine::WrapHandle.restore` was a by-name arm.
- `WalkList` was an owner block of its own (§9.140).

Decision.

- `Routine::WrapHandle.restore`, `HyperConfiguration.batch` and `HyperConfiguration.degree` are `OWNER_ONLY` rows
  (`instances/runtime_objects.rs`).
- One lookup, `runtime_object_owners_of`, finds the owner of all three classes, `WalkList` included. It sits where
  the earliest of the old checks was.
- The `restore` body moved unchanged into `Interpreter::wrap_handle_restore` (`runtime/wrap_handle_restore.rs`).
- The recognition table gains the three rows. `Routine::WrapHandle` is lexical to `Routine` in Rakudo, so, like
  `WalkList`, it has no oracle line; `HyperConfiguration` gains one.

Pinned in `t/routines/closure/wrap-restore-rows.t`.

### 9.161 A row states a `:D` invocant; the pure cascades lose their named stragglers (2026-10-17)

`refactor/11276-pure-cascade-names`.

**`RowFlags::DEFINITE` and `RowFlags::DEFINITE_MULTI`.** A type object calling a method whose invocant
Rakudo declares `:D` was refused by four by-name lists: the numeric methods in the zero-argument cascade
(`check_numeric_type_object_method`, every name refused alike with `X::Parameter::InvalidConcreteness`),
`IO::Path`'s `IO_PATH_CONCRETE_METHODS` in the VM's entry gate, and two `Supply` lists in
`call_method_with_values_inner` (one of them answering an `X::AdHoc` of mutsu's own wording). A row
without `TYPE_OBJECT_OK` simply skipped a type object, so the call went on to whichever later path
happened to answer it.

Decision.

- `DEFINITE` says the method's invocant is `:D`: a type object that reaches the row is refused with
  `X::Parameter::InvalidConcreteness` naming the row's owner. `DEFINITE_MULTI` says every candidate of a
  `multi` is `:D`: the refusal is `X::Multi::NoMatch` (`Int.sqrt`, `Int.round`, `Supply.lines`).
- The refusal is asked where a type object misses: the guard step (the row an instance of the type
  object's shape reaches; a type object with no shape of its own whose instances have one, `UInt` and
  `IO::Path::Unix`, uses theirs), `invoke_owner` and the MRO walk. The invocant binds before the
  arguments, so a row of any arity refuses (`IO::Path.split`, `Supply.start`).
- The flags are set from Rakudo's answers (probed per type object and method): the numeric rows
  (`abs`, `sign`, `floor`, `ceiling`, `truncate`, `narrow`, `lsb`, `msb`, `polymod`, `Rat`, `FatRat`
  `DEFINITE`; `sqrt`, `exp`, `log`, `log2`, `log10`, `round`, `chr`, `Complex.Rat` `DEFINITE_MULTI`),
  `IO::Path`'s (the `stat` rows and every name of the old list; `absolute`, `extension`, `parent`,
  `slurp`, `dir` `DEFINITE_MULTI`) and `Supply`'s; `Str.split`, `Int.base` and `FatRat.base` are
  `DEFINITE_MULTI`, and `Int.is-prime`, `roots` and `Int.expmod` (which Rakudo refuses inside the method
  body) `DEFINITE`. A row whose type object Rakudo answers some other way (`Rat.abs` numifies, `Cool.abs`
  warns) has neither flag. Recognition (`native_method_arities`, what `.^can` and the catalog tests
  read) counts a refusing row: Rakudo's `Int.^can('abs')` is true.
- The three lists are deleted. What stays: `IO::Path.Int` is numeric context (`Mu.Int` is
  `self.Numeric.Int`) and keeps the VM's numeric-context refusal; the `Exception` type objects'
  `throw`/`fail`/`die`/`rethrow`/`resume` keep the VM gate, since only `throw` and `resume` are rows; the
  `Dateish` `.IO` refusal stays in the two `.IO` fast paths.

Behaviour changes toward Rakudo: `Int.sqrt`, `Int.exp`, `Int.log`, `Int.round`, `Int.chr` (and the other
numeric type objects') are `X::Multi::NoMatch`, not `X::Parameter::InvalidConcreteness`; `IO::Path::Unix.e`
refuses; `IO::Path.extension` and its kin are `X::Multi::NoMatch`; `Supply.map({...})` and the other
`Supply:D:` methods with arguments are `X::Parameter::InvalidConcreteness`, not mutsu's
"Cannot call .map on a Supply type object". Pinned in
`t/types/numeric/numeric-type-object-definite-rows.t`, `t/io/io-path-type-object-definite-rows.t` and
`t/concurrency/supply/supply-type-object-definite-rows.t`.

**The cascades' named stragglers.** The pure arity cascades still answered methods by name in front of,
or instead of, their rows:

- `Attribute.DEPRECATED` (the trait's message) and `Thread.yield` are rows of their owners
  (`ctors_mop/attribute.rs`, `io_concurrency/class_methods.rs`; `Thread:U:` refuses an instance).
- `Kernel.hostname` and `Kernel.endian` on the type object are `Kernel`'s rows, which now answer the type
  object (`TYPE_OBJECT_OK`) from the running process; the class-method owner lookup reaches `Kernel`.
- `Any.nl-out` is a row of `Any`, and so is S04's `pending` (Rakudo has none; mutsu answers it on any
  receiver, `roast/S04-exceptions/pending.t`).
- `Cool.AST` takes `:compunit` and a `$slang` argument. Its four copies (two cascade arms, the VM's
  native entry and two blocks of `call_method_with_values`) are gone.
- `Exception.resume` signals the handler for every exception, which decides whether it resumes; the
  cascade's twin is gone.
- The `RakuAST` builders `add-statement`, `unshift-statement`, `push`, `replace-statement-list` and
  `set-expression` are rows of `RakuAST::Node`, and `Pod::Block::Declarator`'s `_add_leading` and
  `_add_trailing` are rows found by `runtime_object_owners_of`.
- The two-argument cascade's `naive-word-wrapper`, `in-range`, `substr-eq`, `split` and `comb` arms
  duplicated rows that already existed; they are deleted, with `native_split_method`, which only they
  called. The one-argument cascade's `__baggy_data__` delegation duplicated the native-base redirect.
- `Backtrace::Frame.name`, a mutsu alias of `subname` that Rakudo does not have, is gone.

Pinned in `t/oo/method/builtin-method-name-rows.t`.

### 9.162 `Mu.gist` and `Mu.raku` are rows; the rendering cascade goes (2026-10-17)

`refactor/11276-mu-render-rows` (plan item 2.1, the rendering names). After §9.152 moved the scalar shapes
off it, `methods_0arg/dispatch_core_repr.rs` still answered `gist`/`raku` by name for every receiver the
rows left. Those were type objects, `Nil`, a parametric role, `Signature`, `Parameter`, `Failure`,
`CallFrame`, the quant hashes, a package stash, an Array/Seq/Slip/Pair/Hash `raku`, a `WhateverCode`, a
`Range`, a lazy list's placeholder, `Promise`, `Channel`, and the string form of the rest. Disabling the
cascade broke 223 `t/` files, so it carried real answers that no row gave.

Decision.

- `Mu.gist` and `Mu.raku` are pure `TYPE_OBJECT_OK` rows (`ctors_mop/mu_render.rs`) whose handlers are the
  cascade's receiver arms. They are reached the way `Mu.Str` is (§9.93): `gist` and `raku` join
  `MU_FOR_CLOSED_SHAPES`, so a closed shape whose own row declines reaches them, and a receiver with no
  shape reaches them through `answer_shapeless`.
- Two kinds of receiver are declined. A mixin renders through what it wraps, which the cascade's mixin
  step and the interpreter resolve. An instance is declined unless its class is a built-in one the
  handler knows.
- Without an interpreter, an instance's renderings go to `Mu`'s rows directly. A subclass of a built-in
  class whose nearest owner is not `Mu` (an `Exception` subclass with its own `message`) is the
  interpreter's, as `Str` already was.
- Two rows that were registered under owner `Mu` but meant for one class move to that class.
  `IO::Handle.raku` becomes a row of `IO::Handle`: Rakudo has none, and its `Mu.raku` dumps attributes
  mutsu keeps elsewhere, so `rows_are_declared_by_rakudo` names it as an exception. With that,
  `DispatchShape::IoHandle` no longer reaches every `Mu` row. `IO::Special`'s `gist` folds into
  `Mu.gist`.
  Left under `Mu`, `IO::Handle`'s interpreter row would be the first `Mu.raku` `owner_row` finds, and
  every shapeless receiver would render through it (`Nil.WHAT.raku` was `""` in a first draft).
- The quant hashes' `gist`/`raku` rows keep the cascade's answer for an element that needs no dispatch:
  `gist` declines only when `quanthash_needs_element_dispatch` says so, and `raku` declines only when
  `container_needs_raku_dispatch` does. `Int.Bag.raku` is `(Int=>1).Bag`.
- The cascade's mixin step renders a mixed-in collection's `raku` through the collections' renderer,
  since the collections' `raku` rows need the interpreter (a typed container names its type).
- Four instance shapes with no rendering of their own do not reach these rows: `Backtrace`,
  `Backtrace::Frame`, `CompUnit::DependencySpecification` and `IO::Path::Parts`
  (`DispatchShape::reaches`, next to `IO::Path::Parts`'s `Str`). Their attribute dump is the
  interpreter's default representation, which a pure row cannot give. `Mu.iterator`, which only
  `IoHandle` reached through that blanket, becomes `OWNER_ONLY` (the MRO walk reaches it).
- `dispatch_core_repr.rs` is deleted.

Not done: `WhateverCode.gist` is still `WhateverCode.new`, where Rakudo prints the source (`* + 1`).
Pinned in `t/types/mu-render-rows.t`.

### 9.163 Exception's control methods, `Any.elems`, `Blob`'s class methods and `Mu.emit` are rows (2026-10-18)

`refactor/11276-exception-control-rows`. Four by-name clusters of `call_method_with_values_inner`, the
VM's CallMethod entry and the zero-argument cascade, each replaced by rows.

**Exception's control methods.** `throw`, `rethrow`, `die`, `fail`, `resume` and `Failure` were two
by-name blocks of `call_method_with_values_inner` (one for an `Exception` or `X::Control` MRO, one for an
`X::`/`CX::` MRO), a VM gate refusing them on a type object, the VM's `exception_failure_method`, a VM
step stamping a throw-site backtrace on a built-in exception, and two pure rows (`throw`, `resume`) that
answered only the core classes.

- They are `OWNER_ONLY | DEFINITE` `Handler::Interp` rows of `Exception`
  (`instances/exception_control.rs`); the handlers are `Interpreter::exception_control` and
  `failure_control` (`runtime/exception_control.rs`), which own the backtrace stamping, the throw-site
  dynamics, the deferred user `message` (#12625), `X::Control` and `CX::Warn`.
- One entry, `exception_control_by_mro`, sits where the first block was. It asks the table whether
  `Exception` or `Failure` has an interpreter row for the call (a bit test and one scan), then whether the
  receiver's MRO reaches that owner, and walks the late rows (`invoke_mro_late`). A class doing
  `X::Control` reaches `Exception`: Rakudo's role is `X::Control is Exception`, a parent mutsu's MRO does
  not list.
- `DEFINITE` refuses a type object (`X::NYI.throw`), so the VM gate keeps only `IO::Path`'s numeric
  context.
- `Failure`'s `rethrow`, `die` and `fail` are rows of `Failure`. Rakudo answers them through `FALLBACK`,
  so `rows_are_declared_by_rakudo` names them. Its `throw` stays the pure row.

Behaviour change toward Rakudo: `.fail` of an *unhandled* `Failure` throws its exception (it answered a
`Failure` of the `Failure`); `Exception.^can('Failure')` is true. Pinned in
`t/exceptions/exception-control-rows.t`.

**`Any.elems` and the containers' `default`.** `dispatch_core_numeric.rs`, the last family of the
zero-argument cascade, answered `elems` and `default` for every receiver no row reached.

- `Any.elems` is a `Narrow` row carrying the arm's body (a junction's eigenstates, a native buffer, a
  genuinely lazy list's `X::Cannot::Lazy`, a stash's symbols), `TYPE_OBJECT_OK`. Like `end` (§9.109),
  it joins `ANY_LIST_FOR_CLOSED_SHAPES`; a shape whose type declares `elems` answers its own row first.
- `Array.default` and `Hash.default` answer a typed, shaped or lazy container through `answer_shapeless`
  (`CONTAINER_FOR_SHAPELESS`).
- `dispatch_core_numeric.rs` and `dispatch_core_families` are deleted: the zero-argument cascade ends in
  `answer_shapeless`.

Behaviour change toward Rakudo: a typed container's default is its element type (`my Int @a; @a.default`
is `Int`). Pinned in `t/oo/method/elems-default-method-rows.t`.

**`Blob`'s class methods.** `allocate`, `encoding` and `chars` (`X::Buf::AsStr`) are rows of `Blob` and
`Buf` (`scalars/blob_class.rs`); `buf_allocate` needs no interpreter. `allocate` binds any fill
(`OBJECT_ARGS`), and an instance is `X::Multi::NoMatch`, since every candidate is `Blob:U:`. The three
by-name blocks go; their `chop`/`chomp` arm was unreachable, because a buffer is not `Cool` (#13313).
`CArray[T].allocate` keeps its block. Pinned in `t/oo/method/blob-class-method-rows.t`.

**`Mu.emit`.** Rakudo's `method emit() { emit self }` is an interpreter row reached by every receiver
(`MU_FOR_CLOSED_SHAPES`). Its handler is the `emit` routine's own, `Interpreter::emit_to_supply`, so the
method form also delivers to the dynamically enclosing supply's emitter. The two by-name `emit` blocks
(interpreter and VM) go. Pinned in `t/oo/method/mu-emit-method-row.t`.

### 9.164 The by-name gates of `call_method_with_values` are rows (2026-10-18)

`refactor/11276-call-method-name-gates`. The interpreter entry kept a run of `method == "..."` gates
between the receiver guards and the table lookup, which `--arms` does not count (they are no `match`
arms). Each became a row of the owner Rakudo declares it on, or turned out to be answered by a row
already and was deleted.

**`ACCEPTS`.** `Regex.ACCEPTS` (the `Match` or `Nil`), `Code.ACCEPTS` (the call's own result,
`self.count ?? self($topic) !! self()`), `WhateverCode.ACCEPTS` (the predicate's truth),
`Signature.ACCEPTS` (the bind test) and `Mu.ACCEPTS` of a type object (the type check, `Mu:U:`) are
`Handler::Interp` rows with `OBJECT_ARGS` (`instances/code_accepts.rs`). None of these receivers has a
shape: the code objects reach their rows through `code::answer`, which picks the owners by kind
(`Regex`, `WhateverCode`, `Code`); a `Signature` instance answers its built-in class's own row in
`answer_shapeless`; and a type object with no closer candidate answers `Mu`'s there. `Mu.ACCEPTS`
declines a class that declares its own `ACCEPTS` and tries an enum's composed role candidates first.
The three gates and the two `ACCEPTS` arms of `dispatch_sub_method` go.

**`IO::Handle`'s `nl-out` and `chomp`.** A subclass with no handle of its own (`class
Text::IO::String is IO::Handle`) answers its attribute or the default from the row, as `nl-out`
already did; `chomp` joins the by-MRO list, and both gates go.

**The mutator refusals and `Any:U` autovivification.** `Any.push`/`append`/`unshift`/`prepend` are
`Handler::Mut` rows (`mutating/autoviv.rs`): an undefined receiver with no binding (`Nil`, `Any`, `Mu`)
answers a fresh `Array` grown through `Array`'s own rows (so `append`'s single-argument rule is
`Array`'s), a defined scalar matches no candidate (`X::Multi::NoMatch`). `Range` declares the six
mutators to refuse them (`X::Immutable`). `mutating::owners_of` gains the kinds (`Range`, the scalars,
`Nil`, `Any`/`Mu` type objects) and loses its `has_name` argument: a `Str` always reaches `Str`'s rows,
whose `subst-mutate` refuses a receiver with no binding. The interpreter's four gates go, and so do the
VM's two autovivification copies (`nil-autoviv`, `any-autoviv`), which the mutating entry after them
now answers. A named binding is still autovivified by the entry that holds it.

**`Mu`'s `rakuseen`, `WALK` and `return`.** Rows every receiver reaches (`MU_FOR_CLOSED_SHAPES`); the
bodies are `Interpreter::mu_rakuseen` and `try_walk_method`, and `.return` raises the method return.

**`List.from-iterator`, `Array.from-iterator`, `Match.make`, `Seq.Int`.** Type-object rows found by
owner (`from_iterator::owners_of`, a `Slip` reaching `List`'s); `Match.make` is an interpreter row
reached by any match (`regex_match::make_answer`); `Seq.Int` (Rakudo's `self.cache.Int`) is a row, so
the gate re-dispatching `sum`/`min`/`max`/`Int` of a `Seq` as a `List` goes.

**Deleted as already answered by rows.** `.self(args)`, `.sum(:named)`, `.hash`/`.Hash` of a list,
`.arity`/`.count` of a `Signature`, `.hyper`/`.race` with named arguments, and the second `STORE` block
of a quant hash. A zero-denominator `Rat.Str` throws from its row, as `gist` already did, so that gate
goes too.

**Left by name in `call_method_with_values_inner`:** `Complex` coercions of a `Str` that numifies to a
`Complex` (the `Str` rows are pure and decline it), the `pick`/`roll` family with a callable count,
`split`, `of`/`keyof` of a class composing `Positional`/`Associative`, a user `Iterable`'s
`hyper`/`race`/`lazy`, the lazy-list blocks, `STORE` of a `Hash`/`Array` (and their subclass
instances and mixins), the `log`/`exp`/`atan2` argument coercion, the `Numeric`/`Real`/`Dateish`
defaults of a class composing those roles, and the constructors.

Behaviour changes toward Rakudo: `5.self(1, 2)` dies (a positional argument), and `Seq.from-iterator` is
no method. Pinned in `t/oo/method/by-name-gate-method-rows.t`; `t/collections/lazy-seq/from-iterator.t`
is updated.

### 9.165 The composed built-in roles' rows and the sampling `Callable` count (2026-10-18)

`refactor/11276-role-default-rows`, after §9.164. Two more by-name gates of
`call_method_with_values_inner` go.

**`Positional`, `Associative`, `Iterable`.** A user class that composes `Positional[V]` or
`Associative[V, K]` keeps the type arguments in its class metadata, and a user `Iterable` lists through
its own `iterator`. `Positional.of`, `Associative.of`/`keyof` and `Iterable`'s `hyper`/`race`/`lazy` are
interpreter rows of the roles (`collections/composed_roles.rs`, `OWNER_ONLY | TYPE_OBJECT_OK`). A role has
no shape: `composed_roles::answer` tries the roles that have a row for the call (a table lookup, not a
name list) on an instance or an unparameterized type object, and a handler declines a class whose
composition does not answer (`Array[Int].of` keeps its own parameter).

**The sampling methods' `Callable` count.** The count-taking rows of `pick`, `roll` and `pickpairs` are
interpreter rows: a `Callable` count is called with the receiver's size (`total` for a quant hash's
`pick`/`roll`, `elems` otherwise and for `pickpairs`; `Bool` counts the two values it samples) and lowered
by the quant hashes' own `callable_count_to_int`. `Array.grab` takes one the same way. The gate that
resolved the count and re-dispatched goes. The zero-argument rows stay `Narrow`.

**Tried and left:** `split` (a deferred `Seq` argument and a grammar cursor reach no row);
`log`/`exp`/`atan2` with an object argument (their arity-1 rows are `Narrow`, which the JIT and constant
folding call, and `invoke_unadmitted` reaches only the first row of a key, so an interpreter twin would
not be found); the `Numeric`/`Real` type-object default of a class composing the role and the `Dateish`
formatter (Rakudo answers both from `Mu`, whose `Numeric`/`Int` stay deferred with #12551).

Pinned in `t/oo/role/role-default-method-rows.t`.

### 9.166 `Routine`'s and `Code`'s own methods are rows, one body per kind (2026-10-18)

`refactor/11276-routine-code-rows`. `runtime/methods_sub.rs` answered `wrap`/`unwrap`, `assuming`,
`add_dispatchee`, `candidates`/`dispatchees`, `cando`, `rw`, `prec`/`precedence`/`associative` and
`is-implementation-detail` twice by name: once in `dispatch_routine_method` for a `&name` handle and once
in `dispatch_sub_method` for a `Sub`, each from the state that kind carries (the registry entry under the
name, or the `SubData`).

- They are rows of the owner Rakudo declares them on: `Routine` for `wrap`, `unwrap`, `add_dispatchee`,
  `candidates`, `dispatchees` and `rw`; `Code` for `assuming`, `cando`, `prec`, `precedence`,
  `associative` and `is-implementation-detail` (`instances/code_methods.rs`, `OWNER_ONLY` interpreter
  rows). The recognition table gains the `Routine` rows and `Code.cando`.
- A row's handler picks the body by kind (`by_kind`): `Interpreter::routine_handle_*` for a handle and
  `sub_*` for a `Sub`, each the arm it replaces moved verbatim (`runtime/methods_code_rows.rs`,
  `methods_code_candidate_rows.rs`, `methods_code_wrap_rows.rs`). A handle's `wrap` of a non-regex routine
  still wraps through a forwarding `Sub`, now by calling `sub_wrap` directly.
- `routine::owners_of` answers `["Routine", "Code"]` for a `Sub` and a handle, so the existing owner lookup
  in `dispatch_callable_method` reaches the rows ahead of the kind dispatchers. An instance of a code class
  (a `Method` object) keeps `["Routine"]` and declines in `by_kind`.
- The dead `Str` arm of the handle dispatcher (the `Code.Str` row answers first) goes. What is left by name
  in the two dispatchers is not Rakudo's: `readonly`, `nextwith`, `callwith` and the `can` lists.

`methods_sub.rs` shrinks from 1,475 to about 750 lines. No behaviour change; pinned in
`t/oo/method/routine-code-method-rows.t`.

### 9.167 A method object is the third kind of the code rows; `Routine.yada` (2026-10-18)

`refactor/11276-instance-ops-gates`, after §9.166. A method object (an instance of `Method`, `Submethod`,
`Regex` or a subclass, from `.^lookup`, `.^find_method` or `.^methods`) answered `wrap`, `unwrap`,
`assuming`, `cando` and `set_name` from by-name gates in the method-call fallback
(`methods_instance_ops.rs`).

- `by_kind` (§9.166) gains the third kind: an instance answers through
  `Interpreter::method_object_code_row` (`runtime/methods_method_object_rows.rs`, the gates' bodies
  moved), and `Code.set_name`'s handler does the same for an instance.
- The fallback reaches them through `method_table::method_object_answer`, which tries only the rows a
  method object answers (`wrap`, `unwrap`, `assuming`, `cando`, `set_name`): its other `Code` methods
  (`of`, `name`, `signature`, `clone`) read the object's own attributes, which the `Code` rows of a code
  value would misanswer, so `routine::owners_of` keeps `["Routine"]` for an instance.
- `Routine.yada` is a row (`Interpreter::code_yada`). A `&name` handle answers from the registered def's
  `is_stub` (`&say.yada` was no method; Rakudo answers `False`).

Pinned in `t/oo/method/routine-code-method-rows.t`.

### 9.168 The native fast path's name lists shrink: `Supply` by owner, adverbs and `AT-POS` by rows (2026-10-18)

`refactor/11276-native-gate-names`. Slice 5 deletes `try_native_method_raw`'s name checks and
`should_bypass_native_fastpath`; this removes the ones the rows already make redundant.

- **`Supply`.** The VM gate declined twenty `Supply` method names twice (an instance and the type
  object) and `Supplier.Supply`. A `Supply` answers its methods through its own rows and dispatcher, which
  need the interpreter, so the VM now declines any call whose receiver `supply_owners_of` names: a
  property of the receiver's class, not of the name. The interpreter twin keeps its list, because
  `Supply.list` and its kin answer from `answer_shapeless` inside the native step it guards.
- **`squish` and `tail`** were sent to the interpreter by name (`tail` by the VM, `squish` by both
  gates): their rows answer every receiver the cascades used to misread, so the names go.
- **The adverb interceptors.** `contains`, `starts-with`, `ends-with` and `substr-eq` with `:i`/`:m`,
  and `base(..., :no-trailing-zeroes)`, were answered by interceptors in front of the VM's arity cascade,
  and `base` also by an interpreter twin. The search rows already bind every adverb (`str_search_interp`,
  `ANY_NAMED`); `Rat`'s and `FatRat`'s `base` rows now declare `:no-trailing-zeroes` (`Handler::Named`),
  the adverb Rakudo's `Rational.base` takes. The interceptors and the dead `native_*_with_options`
  helpers (`methods_narg/str_match.rs`) go.
- **`AT-POS`.** Both entries answered an explicit `.AT-POS` through `builtin_at_pos` ahead of the table;
  the subscript rows (§9.106, §9.138) answer it.

No behaviour change.
### 9.169 The thread-shared lane is the first step of `Array`'s mutator rows (2026-10-18)

`refactor/11276-shared-array-place`. §9.23 left the VM's thread-shared lanes with their own copies of
`push`, `unshift`, `append`, `prepend`, `pop`, `shift` and `splice` ("a `Shared` place is slice 5's"):
`CallMethodMut` answered a plain lexical `@name` that several threads reach from a block of its own, writing
to the binding's one cross-thread home (its decl-site `ContainerRef` cell, or the atomic store keyed by
the name).

- The block is one `Interpreter` method, `shared_array_lane` (`vm/vm_shared_array_lane.rs`), and the first
  step of `Array`'s rows (`mutating::array::run`): when the place names a binding the lane applies to
  (`shared_array_lane_applies`, the two conditions the VM tested), the row answers from it; otherwise the
  row runs as before. The lane's conditions are properties of the binding (shared variables active, a
  plain lexical `@name`, held in a cell or not re-declared), read from the place's name, so no new place
  kind is needed: the named place already carries what the lane reads.
- `CallMethodMut` asks `invoke_mut` at the old position when the lane applies, and goes on to the full
  dispatch otherwise, as before. The copies of the seven mutators in the VM go.
- Every other entry that reaches a row with a named place (the interpreter's by-name mutating entry, a
  run-time method name) now takes the lane too, where it used to write the thread-local copy.

The `ArrayPush` opcode keeps its own lane; it is not a method call. Pinned in
`t/vm/writeback/shared-array-mutator-rows.t`.

### 9.170 `Buf`'s `write-*` family is rows; `Blob`'s mutator protos refuse (2026-10-18)

`refactor/11276-buf-write-rows`. The 3B remainder named "the `write-*` family (no recognition rows)":
`write-int8` .. `write-int128`, `write-uint8` .. `write-uint128`, `write-num32`, `write-num64`,
`write-bits` and `write-ubits` had six copies: the VM native fast path (`try_native_buf_write`), the
VM `CallMethodMut` lane (`try_native_buf_mut`), the interpreter's mutating entry (an instance and a
type-object branch for each sub-family, and a `write-bits` instance fallback) and its by-value entry.

- They are `Handler::Mut` rows of `Buf` (`mutating/buf_write.rs`), reached through `invoke_mut` from
  every entry. A `Buf` type object now has the owner `Buf` (`mutating::owners_of`), so `buf8.write-*`
  answers a fresh buffer from the same rows; an instance is written into its shared attribute cell.
- One transform, `buf_write_int::apply_buf_write`, checks the argument count against Rakudo's
  signatures (`Too few positionals passed; expected 3 or 4 arguments but got 2`) and computes the
  bytes. The six copies go.
- `Blob` declares only the protos of `push`, `append`, `unshift` and `prepend`. Its rows
  (`mutating/buf.rs`) raise `X::Multi::NoMatch` for a `Blob:D` or an encoding buffer, and `pop`,
  `shift` and `reallocate` have no `Blob` row, so they raise `X::Method::NotFound`. The two name-gated
  interceptors that refused a `Blob` with an `X::AdHoc` go, and so does the by-value copy of the `Buf`
  mutators that the rows already answered.
- The recognition table gains the 14 names under its folded `Blob` owner, like `push`.

Behaviour changes toward Rakudo: `buf8.write-bits`/`write-ubits` on a type object work; a `Blob`,
`blob8` or `utf8` receiver of `write-*` raises `X::Method::NotFound` (an `X::AdHoc`, a write into the
`utf8`, or a `Blob[uint8]` answer before); `Blob`'s mutators raise Rakudo's exception types; the
argument-count error is Rakudo's. Pinned in `t/types/buf-write-rows.t`.

- **Found, not fixed** (#13849): the folded `Blob` owner makes `Blob.can("reallocate")` and
  `Blob.can("write-uint8")` true (Rakudo: false), as it already did for `reallocate`.

### 9.171 `Any`'s growing rows autovivify a named binding (2026-10-18)

`refactor/11276-any-autoviv-binding`. §9.164 left the autovivification of a *named* undefined
receiver (`my $x; $x.push(1)`) to "the mutating entry that holds it": the VM's `CallMethodMut` kept a
`push`/`unshift`/`append`/`prepend` name gate that wrote a fresh `Array` into the binding before
dispatch.

- The `Any` rows (`mutating/autoviv.rs`) do it now. `Interpreter::vivify_array_binding`, moved
  verbatim from the VM block, stores the array (in its `Scalar` for a `$` variable or a
  package-qualified slot, and through a `:=` alias or a `<->` loop parameter). The row then grows it
  through `Array`'s row with the same binding, re-read (`ReceiverPlace::rebound`). An `Array` type
  object in a named binding (`my Array $a`) vivifies too, as the VM block did.
- The VM block goes. The modifier forms no longer see a pre-vivified receiver: `$q.+push(1)` now
  vivifies once and answers `([1],)` as Rakudo does, where the VM answered `([1], [1], [1])`.
- `Array`'s growing row answers the array decontainerized (`my $y = [1]; $y.push(2)` is `[1, 2]`,
  not `$[1, 2]`), as Rakudo does; the binding keeps its item.

Pinned in `t/types/any-autoviv-binding-rows.t`. The typed-scalar and `is rw` attribute gaps stay with #13823.

### 9.172 `Any`'s keyed mutators are rows; the VM's key-method arms go (2026-10-18)

`refactor/11276-vm-key-arms`. `CallMethodMut` kept a `match` on the method name ahead of dispatch:
an `AT-KEY` fast path on a hash or quant hash, and `ASSIGN-KEY`, `DELETE-KEY` and `BIND-KEY` on an
undefined receiver, which vivified a hash (with a copy of `Hash.BIND-KEY`'s variable binding) or
answered `Nil`.

- `Any` has `ASSIGN-KEY`, `BIND-KEY` and `DELETE-KEY` rows (`mutating/autoviv.rs`), as Rakudo declares
  them (`Any:U \SELF` candidates). For an undefined binding (`Nil`, or the `Any`, `Mu` or `Hash` type
  object), `ASSIGN-KEY` and `BIND-KEY` vivify it through `vivify_binding`, generalised from §9.171,
  and store through `Hash`'s rows on the re-read binding. `DELETE-KEY` answers `Nil`. A receiver
  with no binding, or a typed container's type object, takes the cascade (#13823).
- `AT-KEY` needs no arm: the ordinary row dispatch answers every case the fast path did, including
  a missing key's typed default, `is default`, the element container, an object hash, the quant
  hashes and `Map`.
- The recognition table gains `Any`'s three rows.

Behaviour changes toward Rakudo: `my $x; $x.ASSIGN-KEY('a', 1)` leaves `${:a(1)}` (the hash in the
`Scalar`), not `{:a(1)}`; `Any.BIND-KEY` binds the argument's value (`$BIND` is not raw), not the
caller's variable. Pinned in `t/types/any-key-autoviv-rows.t`.

- **Found, not fixed**: the HyperSeq/RaceSeq pseudo-method arms (`WHAT`, `^name`, `defined`) stay.
  Without them, the `List` conversion that follows answers `List`; the conversion, not the arms,
  is the debt. An element receiver (`@a[1].ASSIGN-KEY('e', 9)`) still raises `X::Method::NotFound`
  where Rakudo vivifies the element (#13823).

### 9.173 A hyper or race sequence answers `Mu`'s introspection as itself (2026-10-18)

`refactor/11276-hyperseq-mut-receiver`. Three entries read a `HyperSeq`/`RaceSeq` as its `List` for
every method its own rows do not answer: the VM's `CallMethod`, its `CallMethodMut` and the
interpreter's `call_method_with_values`. The two VM entries kept arms ahead of the conversion so that
`WHAT`, `^name` and `defined` (plus `isa`/`does` in `CallMethod` only) still saw the sequence. The
mutating entry's arms lacked `isa`, so `my $h = (1..3).hyper; $h.isa(List)` was `True`.

- One list, `hyper_seq::answers_as_itself` (`WHAT`, `^name`, `isa`, `does`, `defined`, `DEFINITE`,
  `WHICH`, `WHERE`: `Mu`'s type and identity introspection), declares which methods see the sequence.
  All three conversions skip those names, and the ordinary dispatch answers them on the sequence. The
  two VM arm blocks go.
- Rakudo's `HyperSeq`/`RaceSeq` reach every other method through `self.List`, which the conversion
  still models.

Behaviour change toward Rakudo: `$h.isa(HyperSeq)` is `True` and `$h.isa(List)` is `False` through a
variable, as they already were on a temporary. Pinned in `t/oo/mop/hyper-seq-mu-introspection.t`.

### 9.174 `STORE` of `Array`, `List`, `Hash` and `Map` is rows (2026-10-18)

`refactor/11276-container-store-rows`. `call_method_with_values` answered `STORE` from a name arm,
`native_container_store`. It re-initialised any native hash or array and declined an immutable list.
So `%h.STORE(keys, values)` stored the keys as alternating pairs, a built `Map` was overwritten, a
typed array took any element, and `(1, 2).STORE(...)` was `X::Method::NotFound`.

- `Handler::Mut` rows `Array.STORE`, `List.STORE`, `Hash.STORE` (one and two arguments) and
  `Map.STORE` (`mutating/store.rs`), reached through `invoke_mut` from every entry.
  - `Array` checks each element against the element type and stores into the shared node, keeping
    its metadata (`store_into_array`).
  - `List` without `:INITIALIZE` is Rakudo's list assignment into its elements, so a list of plain
    values refuses with the first element's `X::Assignment::RO`. A list of containers takes the
    existing path.
  - `Hash` zips the two-argument form.
  - `Map` without `:INITIALIZE` refuses with `X::Assignment::RO`.
  - The answer is the container, not the item a `$` variable holds it in.
- The name arm stays for what has no row: a user `is Hash`/`is Map` instance (its backing storage)
  and a mixin over a hash or array.
- The recognition table gains `Array.STORE`, `List.STORE` and `Hash.STORE`. `Array.^can('STORE')` is
  then true, as in Rakudo. The parser's `.=` desugaring on a method-call lvalue had used `.^can('STORE')`
  to mean "a custom container" (a `Proxy`, a user `STORE`), so `self.x .= Hash` on a `$` attribute
  holding an `Array` began storing into the `Array`. It asks `__mutsu_custom_store` now
  (`runtime/builtins_dot_assign_store.rs`): a `Proxy`, or a user class, role or mixin that declares
  `STORE`.

Pinned in `t/vm/writeback/container-store-rows.t`.

- **Found, not fixed**: `Array.STORE(...)` on the type object raises `X::Method::NotFound` where
  Rakudo raises `X::Multi::NoMatch`: a `Mut` row has no type-object refusal step yet.

### 9.175 `Any`'s autovivifying rows refuse a receiver with no container (2026-10-18)

`refactor/11276-mut-type-object`. §9.171 and §9.172 vivified any undefined binding the place named.
Three kinds of name had no container to vivify, and each was overwritten:
- a read-only parameter or `for` alias (`sub f($p) { $p.push(1) }`);
- a name bound straight to a type object (`my $x := Any`, a sigilless `my \s = Any`);
- the bare type word of `Hash.ASSIGN-KEY(...)`, which reaches `CallMethodMut` with the spelling `Hash`
  and was vivified into a stray env entry.

- `vivify_binding` checks the binding as an assignment does (`check_readonly_for_modify`), so a
  parameter or alias raises Rakudo's "Cannot assign to a readonly variable or a value".
- `containerless_type_object` (`mutating/autoviv.rs`) names the other cases: a name stored under a
  sigilless term key, a bare type word (the type's own spelling with no env binding), or a sigiled
  name bound to the type object.
  - The growing rows raise `X::Assignment::RO` ("Cannot modify an immutable 'Any' type object").
  - `ASSIGN-KEY` raises "assign requires a concrete object (got a Hash type object instead)".
  - `BIND-KEY` raises `X::Multi::NoMatch`.
  - A `$` scalar arrives without its sigil, so these tests cannot mistake one; `my $Any` still
    vivifies.
- `Array.push(1)` now refuses with `X::Assignment::RO`. Rakudo's own refusal is `X::Multi::NoMatch` from
  the `STORE` its `Any:U` candidate calls, an implementation detail not reproduced.

Pinned in `t/types/any-autoviv-readonly-receivers.t`.

- **Filed**: #13881. A type-object receiver of `Array`/`Hash`/the quant hashes reaches no `Mut` row
  (`Array.pop`, `SetHash.set`, `Array.STORE`), so it raises `X::Method::NotFound` instead of Rakudo's
  concreteness or no-match error.

### 9.176 `STORE` of `is Hash`/`is Map` instances and mixins runs the rows (2026-10-18)

`refactor/11276-store-subclass`. §9.174 left `native_container_store` as a name arm with its own
copy of the store for a user `is Hash`/`is Map` instance and a mixin over a hash or array.

- `native_container_store` now only finds the storage (the instance's `__mutsu_hash_storage`, a mixin's
  inner value) and runs `Hash.STORE`/`Array.STORE` on it as a detached place through
  `native_array_storage_mut`, answering the invocant. The plain-container case is no longer
  there: the rows answer it, and a row that declined (`Map` without `:INITIALIZE`) is not overridden.
- A mixin whose role composes `STORE_AT_KEY` still stores pair by pair through that method.
- The call is still reached by name from `call_method_with_values` and from the `callsame` bridges
  (`native_hash_storage_base`, `mixin_base_native_entry`), because `mutating::owners_of` cannot name
  an instance or mixin receiver from its value; it is a bridge to the rows, not a second store.

Pinned in `t/vm/writeback/container-store-subclass.t`.

### 9.177 The `List` type object refuses the mutators with `X::Immutable` (2026-10-18)

`refactor/11276-list-type-object`. `List.push(1)` was `X::Method::NotFound`; Rakudo raises
`X::Immutable` for `push`, `append`, `unshift`, `prepend`, `pop` and `shift`, as for an instance.
`RowFlags::IMMUTABLE` marks a row whose owner is immutable: `Row::type_object_refusal` answers
`X::Immutable` for it, so the refusal step of `invoke_mut` (#13881) covers it with no new code path.
`List.splice` and `List.STORE` already answered Rakudo's no-match errors.

Pinned in `t/collections/list-type-object-mutators.t`.

### 9.178 A lazy receiver goes through the lazy step (2026-10-19)

`refactor/11276-lazylist-rows`; the design is [ADR-13891](13891-lazy-receivers-pull-before-rows.md)
(`Accepted` 2026-10-19). The five `LazyList` blocks of `call_method_with_values_inner` (the `gist`/`Str`/`raku`
placeholders, `cache`, the `AT-POS`/`EXISTS-POS` partial force, `join`'s `...` and
`should_force_lazy_list`'s force-and-redispatch) are one step, `Interpreter::lazy_receiver_step`:
a *pull* registered by the owner Rakudo declares the method on (`method_table::lazy_pull`, 16
pulls on `Seq`, `List`, `Array` and `Any`), found through the MRO of the type the receiver
reports, answers from what it pulls or declines; a method in the reify table then reifies the
receiver and re-dispatches on a settled `Seq`, whose rows answer it. No behaviour change.

Left: the reify table is still names (it becomes "the rows without a pull" after each name is
audited on an infinite sequence); the streaming adaptors and index pipes stay ahead of the step
because they also serve an infinite `Range` (ADR-13891 §4).

Pinned in `t/collections/lazy-seq/lazy-receiver-pulls.t`.

### 9.179 The `Real` and `Numeric` roles' methods are composed-role rows (2026-10-19)

`refactor/11276-real-role-rows`. A user class that composes `Real` got the role's methods from a
heuristic in `dispatch_instance_and_fallback`: any method the class did not answer was re-sent
to `.Numeric` (or `.Bridge`) of the instance, with `Bool`, `Bridge` and `log` gated ahead of it,
and it applied to every class with a `Bridge` or `Numeric` method, composing a role or not.
Rakudo bridges only through the roles' declared methods (`Real.^candidates[0].^method_table`
and the `Numeric` role's), so the heuristic is gone and the methods are rows of `Real` and
`Numeric` (`collections/composed_real.rs`, about 60 rows), reached through
`composed_roles::answer` like `Positional.of`:

- `self.Bridge.METHOD(...)` for the conversions, rounding, transcendental, `base`,
  `polymod`, `unpolar`, `roots` and `Str`; an argument that composes `Real` is bridged too.
- `Real.Bridge` is `self.Num` (a class with neither dies instead of recursing, where Rakudo
  loops), `Real`, `Numeric` and `conj` are the invocant, `isNaN` is `False`, `abs` is
  `self < 0 ?? -self !! self`, `succ`/`pred`/`Bool` are `self ± 1` / `self != 0` through the
  operators, `Numeric.gist` is `self.Str`.

Placement: the class's own methods win (`composed_roles::answer` declines a name the class
declares); `Mu`'s rows found by the class MRO walk (`shapeless_row`) decline a zero-argument
name a composed role declares, so `Numeric.Bool` comes before `Mu.Bool`; infix `~`/`eq` and the
truth test ask `composed_roles::role_declares` alongside the user-method check. The `Bridge`
arms of `should_bypass_native_fastpath` and of the numeric-bridge probe are gone with the
heuristic. A subclass of a built-in number (`class F is Num`) is not one of these classes: its
built-in parent answers from the payload. What the heuristic also covered and Rakudo has too,
`Cool`'s numeric methods (`self.Numeric.METHOD`) on an instance of a `Cool` subclass with its
own `Numeric` (`roast/S32-num/cool-num.t`), stays a narrow step in
`dispatch_instance_and_fallback` marked `TODO: compile to bytecode` (a `Cool` row reached by
the instance's MRO walk with an interpreter twin is its replacement), and `log`/`exp`/`atan2`
coerce a `Real` argument through its `Bridge`, as the operators do.

Behaviour changes toward Rakudo: `.Real`, `.Numeric`, `.conj` and a non-negative `.abs` answer the
invocant; `.isNaN` no longer calls `Bridge`; `.is-prime` (not a `Real` method) is
`X::Method::NotFound`; a class with only a `Bridge` or `Numeric` method no longer answers
`log`/`sqrt`/`Int`/`succ`/... (Rakudo: `X::Method::NotFound`), and keeps `Mu.Bool`.
`t/types/numeric/numeric-bridge-probe-memo.t`'s last test augmented a `Bridge` onto a plain
class and expected `.Int`; it now augments a `Numeric` and checks `+`.

Left: `ACCEPTS` (#13894: `Any.ACCEPTS` answers first, and `Int.ACCEPTS` does not bridge), the
type-object answers (`R.sqrt` is `X::Multi::NoMatch` in Rakudo), the `Numeric`/`Real`
type-object default in `call_method_with_values_inner`, and the operators' own `Bridge`
coercion (`builtins_operators_coerce.rs`), which is the operators' and stays.

Pinned in `t/oo/role/real-role-composed-rows.t`.

### 9.180 A built-in subclass answers its base's methods on its payload (2026-10-19)

`refactor/11276-builtin-subclass-payload`. `dispatch_instance_and_fallback` opened with a list of
nineteen names (`key`, `value`, `kv`, ..., `Str`, `Hash`, `Map`, `cmp`) that an `is Pair`
subclass sent to the `Pair` it carries. The table's guard step already answers such an instance
from its payload's rows (`native_base::redirect`, §9.79); what the list covered beyond it were
the methods the `Pair` shape has no row for (`Hash`/`Map`, which are `Any`'s) and `Str`. The list
is gone; instead:

- the late instance step of `dispatch_instance_and_fallback` re-dispatches a method the class
  does not declare (no user method or accessor) on `native_base::redirect`'s payload, for every
  built-in subclass;
- `Any`'s rows reached through the instance's own MRO walk (`shapeless_row`,
  `any_list_method_by_mro`) decline for an `is Pair` or quant-hash subclass
  (`native_base::has_collection_payload`), so `Any.Hash` runs on the `Pair`, not on the instance;
- `redirect` answers `Str`/`Stringy` of an `is Pair` subclass with the `Pair`'s (`a\t3`, Rakudo's
  `Pair.Str`); a scalar subclass keeps rendering itself.

No behaviour change for the names the list had, bar `cmp` (Rakudo: `X::Method::NotFound` on the
instance; mutsu now names `Pair`). Pinned in
`t/collections/range-pair/pair-subclass-payload-methods.t`.

### 9.181 The roles' type-object coercions and `Dateish`'s renderings are rows (2026-10-19)

`refactor/11276-role-type-object-rows`. Two by-name gates of `call_method_with_values_inner` were
role methods spelled out by hand:

- `.Numeric`/`.Real` of the type object of a class composing the role warned "Use of
  uninitialized value of type C in numeric context" and answered `C.new`. That is Rakudo's
  `Numeric(Numeric:U:)` and `Real(Real:U:)`: the §9.179 rows `Numeric.Numeric` and `Real.Real`
  now take a type object (`TYPE_OBJECT_OK`) and answer it so, through
  `raise_resumable_warning`.
- `.Str`/`.gist`/`.Stringy` of an instance of a user class composing `Dateish` called its
  private `!formatter` as from a method of the class. They are rows of `Dateish`
  (`collections/composed_dateish.rs`; Rakudo's `Str(Dateish:D:) { self!formatter }` and
  `gist(Dateish:D:) { self.Str }`), reached through `composed_roles::answer` (`Dateish` joins its
  owners). `Stringy` is `Mu`'s `self.Str`, and infix `~` and interpolation ask
  `composed_roles::role_declares`, so no row of its own is needed. `Date`, `DateTime` and their
  subclasses render natively and decline.

No behaviour change. Pinned in `t/oo/role/real-role-composed-rows.t` and
`t/oo/role/dateish-role-composition.t`.

### 9.182 `split` is found through the receiver's MRO (2026-10-19)

`refactor/11276-split-gate`. `call_method_with_values_inner` sent every `.split` call that was not
on an `IO::Handle`, `IO::Pipe`, `IO::CatHandle` or `IO::Spec` to the `Str`/`Cool` rows' routine
(`dispatch_split_row`), or to `IO::Path`'s content split. The rows already exist on each of those
owners (§9.76, §9.20-§9.22); what the arm still covered were the receivers the guard step does not
take: a grammar cursor, a `Cool` subclass, and a call whose argument is a deferred `Seq` (a
`gather` of needles). `split` joins the text methods `text_method_by_mro` answers, which walks the
receiver's MRO to the first owner declaring the row (a cursor reaches `Cool`'s, an `IO::Path`
its own) and reads a `Cool` instance with its own `Str` through it. The arm is gone. No behaviour
change. Pinned in `t/oo/class/split-receivers-by-mro.t`.

### 9.183 A `Str` subclass's `.Str` is the instance, through the native base (2026-10-19)

`refactor/11276-str-subclass-identity`. A by-name gate of `call_method_with_values_inner` answered
`.Str` of an instance whose MRO reaches `Str` with the instance itself, unless a class below `Str`
declares its own `Str`. That is the native-base resolution's "coercer that answers the instance"
(§9.79: `MyInt.new(5).Int` is the `MyInt`): `native_base::returns_the_instance` now lists `Str`
and `Stringy` for a `Str` payload, and `native_base::dispatch` asks it before the
"answered by the instance" names, which include the renderings. The gate is gone; `.Stringy`,
which no gate covered, gives the instance too, as Rakudo's `Str.Stringy` does. A class's own `Str`
is still found first. Pinned in `t/types/string/str-subclass-str-identity.t`.

### 9.184 A numeric parameter coerces a user object: `RowFlags::NUMERIC_ARGS` (2026-10-19)

`refactor/11276-numeric-arg-coercion`. Before any dispatch, `call_method_with_values_inner`
rewrote the arguments of every `log`, `exp` and `atan2` call: an instance argument was replaced by
its `Bridge` (a `Real` class) or its `Numeric`, unless the receiver's class declared the method
itself. That is a parameter coercion of the built-in rows (Rakudo's `Num(Real) $base`,
`Numeric $base`), so it moved to them: `RowFlags::NUMERIC_ARGS` (`RowFlags` widens to `u32`; the
sixteen bits were spent) marks the arity-1 `log`/`exp`/`atan2` rows of `Int`, `Num`, `Rat`,
`Cool` and `Complex`, and `invoke_unadmitted`, which already takes the calls whose arguments the
guard step declines, coerces each user-object argument (the operators' rule: `Bridge` for a
`Real` class, `Numeric` otherwise) and hands the row the number. The gate is gone; a user method
of the name never reaches the rows, so it still sees its argument unchanged. The rows stay pure:
the coercion runs in the interpreter's entry, not in the handler, so constant folding and the JIT
see the same rows. No behaviour change. Pinned in `t/types/numeric/numeric-math-object-args.t`.

### 9.185 `raku` of a typed container is the `raku` row's (2026-10-19)

`refactor/11276-raku-row-typed-containers`. Three by-name gates of `call_method_with_values_inner`
rendered a container whose element or key type the interpreter's registry records:
`Hash[Int,Str].new(...)` (or `(my Int % = ...)`), `Array[Int].new(...)` and
`array[int].new(:shape(...), ...)`. They ran after the pure prologue had computed its answer and
overrode it, while the `raku` rows of `Array` and `Hash` (§9.95, `render_names`) declined such a
container to them. The three renderings are now one routine, `Interpreter::typed_container_raku`
(`runtime/methods_raku_typed.rs`), which the `raku` row's handler calls first, and the gates are
gone. The receivers that reach no `Array`/`Hash` shape: a shaped or itemized array or hash reaches
the type's row through `answer_shapeless` (when it has an interpreter: a pure entry keeps `Mu`'s
rendering, which the "odd number of elements" message reads), and the pure prologue leaves a
shaped array's `raku` to it, since only the interpreter knows its element type. The
dispatch-leaf rendering (`raku_repr_with_dispatch`: an element whose own `raku` is user code)
stays a gate. No behaviour change; the existing typed-container tests
(`t/collections/subscript/shaped-array-*.t`, the constrained-hash and typed-array `raku` tests)
pin it.

### 9.186 `tail` and `head(&callable)` are `Any`'s interpreter twins (2026-10-19)

`refactor/11276-head-tail-twins`. Two by-name gates of `call_method_with_values_inner` sent every
`.tail` call (over the pure answer the prologue had already computed) and every
`.head(&callable)`/`.head(*)` call to the interpreter's routines (`dispatch_tail`,
`dispatch_head`). The pure rows (`terminals`, `list`) answer a plain list; what the gates covered
is what they decline: a lazy list (`X::Cannot::Lazy`), a `Seq.new($iterator)` body that has not
been pulled (a predictive iterator's `count-only`), an instance, a `Callable` or `Whatever`
count. Those are now `Any`'s interpreter twins (`any_interp_twins`, §9.135), reached through the
late MRO walk of `dispatch_method_by_name_1` once the pure rows declined; the gates are gone.
`tail` ignores a named argument as before (`(1, 2, 3).tail(:zzz)` is 3). The pure `tail` rows now
decline an unpulled `Seq` (`terminals::unpulled_seq`), which they used to read as empty. No
behaviour change: the 242 `t/` files calling `.head`/`.tail` pass, among them
`t/types/any-head-tail-rows.t`, `t/routines/call/tail-function.t` and
`t/collections/lazy-seq/predictive-seq-tail-scope.t`.

### 9.187 `Capture` and `fmt` of the declined receivers are interpreter twins (2026-10-19)

`refactor/11276-capture-fmt-twins`. Three by-name gates of `call_method_with_values_inner` handed
`.Capture` of a user object, a `Channel`, a `Supply`, a cat handle's lines or a list with a non-`Str`
`Pair` key to `try_interpreter_capture`, and `.fmt` with a `Format` object or over elements whose
`.Str`/`.Numeric` is user code to `dispatch_fmt_with_format`/`dispatch_fmt_with_user_coercion`. The
pure rows (`Mu.Capture`, the collections' `Capture`, `fmt`) already decline exactly those
receivers, so the routines are now the late interpreter twins `Mu.Capture` and `Any.fmt`
(`any_interp_twins`; the twin macro takes an owner), reached through the late MRO walk of
`dispatch_method_by_name_1` like `tail` (§9.186). The gates are gone. No behaviour change. Pinned
in `t/routines/call/capture-fmt-interpreter-twins.t`.

### 9.188 `gist` of a collection with user-rendered elements is the collections' twin (2026-10-19)

`refactor/11276-collection-gist-twins`. A by-name `gist` gate of `call_method_with_values_inner`
rendered an `Array`, `Seq`, `Slip`, `Hash` or `Pair` holding a value whose rendering is user code
(an instance, a type object, a code object, a mixin) element by element through method dispatch,
over the answer the pure prologue had computed. That renderer is now
`Interpreter::collection_gist_with_dispatch` (`runtime/methods_gist_dispatch.rs`), the late
interpreter twin of `List.gist`, `Map.gist`, `Seq.gist` and `Pair.gist`, reached through the late
MRO walk for a collection receiver only (every other receiver's `gist` stays `Mu`'s). The pure
`gist` rows already declined such a collection, except for a mixin element (`[5 but R]`), which the
pure probe (`collection_gist::gist_route`) now routes to dispatch too. The gate is gone. No
behaviour change; the 1,029 `t/` files that call `.gist` or `say` pass. Removing the gate exposed a
wrong element count it had hidden: `my @b = (1, 2, 3) but R` held one element, rendered without
its parentheses by the gate (`roast/S02-types/list.t`); `coerce_to_array` now flattens a mixin over
a list as Rakudo does, pinned in `t/oo/mixin-list-array-assign.t`.

### 9.189 The lazy step's reify table is audited and shared with the VM (2026-10-19)

`refactor/11276-lazy-reify-audit`. The reify-table audit of ADR-13891 §3 (its §6 amendment): the
runtime step's table and the VM's `lazy_list_needs_forcing` were two copies of one list and are
now `method_table::lazy_pull::reifies`; `Mu.item` is a pull (an infinite sequence itemizes as it
is) and `keys` is an index pipe like `kv`/`pairs`, both answering Rakudo where mutsu threw
`X::Cannot::Lazy`. The itemized sequence `.item` now returns (`Scalar(LazyList)`) reaches the
lazy step on both VM lanes (`lazy_pull::itemized_lazy_receiver`), and its `raku` is the sequence's
own inside `$( )`. `sink` stays off the VM's forcing lane, whose force does not mark a sunk gather
consumed. Pinned in `t/collections/lazy-seq/lazy-receiver-reify-audit.t`.

### 9.190 A string that spells a `Complex` converts through late twins (2026-10-19)

`refactor/11276-complex-real-gate`. A by-name gate of `call_method_with_values_inner` answered
`Int`/`UInt`/`Num`/`Rat`/`FatRat`/`Real` of a `Complex` and of a string that numifies to one
through `dispatch_complex_to_real`. A `Complex` receiver's conversions were rows already (§9.70,
`Cool.UInt` §9.71), so the gate only still answered the string (`"1+2i".Int`), which the pure
`Str.Int`/`Str.Num`/`Cool.Rat`/`FatRat`/`Real` rows decline because it needs `$*TOLERANCE`. Those
five are now late interpreter twins (`scalars/str_complex_real.rs`) reached through the late MRO
walk for a `Str` receiver, and `Cool.UInt` answers the string itself. The gate is gone. No
behaviour change; pinned in `t/types/string/str-complex-real-coercion.t`.

### 9.191 `Allomorph.ACCEPTS` of an object is a late interpreter twin (2026-10-19)

`refactor/11276-allomorph-accepts`. A by-name gate of `call_method_with_values_inner` matched an
allomorph receiver against an instance by calling the instance's `.Numeric` and `.Str`. It is now
the late interpreter twin `Allomorph.ACCEPTS` (`scalars/allomorph_accepts.rs`) with Rakudo's body:
a `Numeric` argument is matched by the number, a `Str` by the string, anything else by both, each
through the part's own `ACCEPTS`. `Allomorph` joins the row catalogue and the Rakudo method-table
snapshot. The pure arm (`methods_narg::allomorph`) still answers every other argument; the gate and
its `allomorph_numeric_equal` copy are gone. Smartmatch with an allomorph matcher and an object
topic (`N.new ~~ <42>`, `False` before) now reaches the twin, and an instance of a `Str` subclass
smartmatches its string payload (`S.new(value => "42") ~~ "42"`, `False` before). Pinned in
`t/types/coercion/allomorph-accepts-object.t`.

### 9.192 A type object has no `coerce` (2026-10-19)

`fix/11276-no-type-object-coerce`. A by-name gate of `call_method_with_values_inner` answered
`Type.coerce($value)` on any type object through `try_coerce_value_for_constraint`, a method
Rakudo does not have (`Int.coerce("3")` is `X::Method::NotFound`); coercion is the coercion
type's HOW's (`Metamodel::CoercionHOW.coerce`, an interpreter row). Nothing in mutsu called it,
and a class's own `coerce` is found before it. The gate is gone, so the call now fails as in
Rakudo; pinned in `t/types/coercion/type-object-has-no-coerce.t`.

### 9.193 `CArray[T].allocate` is upstream NativeCall's (2026-10-19)

`refactor/11276-carray-allocate-upstream`. A by-name `allocate` gate of
`call_method_with_values_inner` pre-sized a `CArray[T]` natively (zero-filled storage for a native
element type, the type's gap value otherwise). Since ADR-11203 `use NativeCall` loads the vendored
upstream `NativeCall::Types`, whose typed `CArray` roles declare `allocate` (`nqp::create` plus an
`nqp::bindpos_*` of the last slot), which the VM runs; the gate is gone. Pinned in
`t/nativecall/carray-allocate-upstream.t`; the 281 `t/nativecall/` and CArray files pass.

### 9.194 `raku` of a collection with user-rendered elements is the collections' twin (2026-10-19)

`refactor/11276-call-me-gate`. The `raku` counterpart of §9.188: a by-name `raku` gate after the
native cascade of `call_method_with_values_inner` rendered a container holding a value whose
`raku` is user code (an instance, a built-in object type) through
`Interpreter::raku_repr_with_dispatch`, over whatever the rows had answered. The pure `raku` row
(`render_names::raku_row`) already declines such a container (`container_needs_raku_dispatch`);
the renderer is now the late interpreter twin of `raku` on every collection owner Rakudo declares it
on (`List`, `Map`, `Seq`, `Pair`, `Capture`, `Set`/`SetHash`, `Bag`/`BagHash`, `Mix`/`MixHash`),
reached through the late MRO walk for such a container only. Unlike the `gist` twins they keep the
receiver's itemization, which is part of the text (`$(F!, 1)`). The gate is gone; the itemized
instance gates (`raku_scalar_itemized`) stay. Two pure answers had been hidden behind the gate and
now decline too: `Mu.raku` (`mu_render`) for such a container, and the `raku` row for a `Seq` whose
source is unpulled (`[$f].Seq`), which the interpreter pulls first. No behaviour change; pinned in
`t/collections/container-raku-user-elements.t`.

### 9.195 `Variable.block` and `Variable.var` are rows (2026-10-19)

`refactor/11276-variable-rows`. Two by-name gates of `call_method_with_values_inner` answered `block`
and `var` of the reflective `Variable` a variable trait handler receives. They are now `OWNER_ONLY`
interpreter rows on a `Variable` owner (`ctors_mop/variable.rs`), reached from the instance dispatch
by owner as `Attribute`'s are (§9.121); the `var` body is `Interpreter::variable_var`. `Variable`
joins the type catalogue (`Variable Any Mu`), the row catalogue and the Rakudo method-table
snapshot. `.var` no longer answers a `.VAR` descriptor, which Rakudo's `Scalar` does not declare
(`X::Method::NotFound`). Pinned in `t/oo/trait/variable-trait-rows.t`.

### 9.196 `Supply.interval` is a class-method row; the `Systemic` type-object rows are reached by owner (2026-10-19)

`refactor/11276-instance-ops-gates`. Three by-name gates of the `Package` class-method lane in
`methods_instance_ops.rs`:

- `Supply.interval` is a class-method row (`class_methods.rs`, next to `from-list` and
  `on-demand`); its body is `Interpreter::dispatch_supply_interval`
  (`runtime/supply_interval.rs`), and it joins the row catalogue.
- `Compiler.id`'s gate becomes a hook that offers a `Systemic` class's type object the rows flagged
  `TYPE_OBJECT_OK` (`Compiler.id` was one already). The `attr!` rows lose that flag: an attribute
  needs an instance (`Compiler.name` is Rakudo's "Cannot look up attributes in a Compiler type
  object"), which the hook would otherwise have answered with `Nil`.
- `X::AdHoc.payload`'s gate is gone: its row (`instances/exception.rs`) already answers it.

Pinned in `t/concurrency/supply/supply-interval-class-row.t`. `Raku.version` on the type object and
`Kernel.name`'s error still differ from Rakudo, as they did (#13952).

### 9.197 `IO::Special.new` is a class-method row (2026-10-19)

`refactor/11276-leave-io-special`. A by-name gate of `call_method_with_values_inner` built the
`IO::Special` stand-in for `IO::Special.new($what)`. It is now a class-method row
(`class_methods.rs`, `TYPE_OBJECT_OK`), found by owner for the `IO::Special` type object as
`Supply`'s class methods are (§9.122); the gate is gone. `leave` keeps its gate: Rakudo 2026.09
implements no `leave` (`Routine.leave` dies "not yet implemented"), and mutsu's answers receivers
Rakudo declares none on (`leave Sub: 42`), so it is no row of a Rakudo owner. Pinned in
`t/io/io-special-new-class-row.t`.

### 9.198 `Stash.merge-symbols` and `Raku.legacy` are rows (2026-10-19)

`refactor/11276-more-gates`. Two by-name gates of `methods_instance_ops.rs`:

- `Stash.merge-symbols` is a row of `Stash` (`instances/stash.rs`), found by its owner from the
  instance dispatch like `Variable`'s (§9.195). The gate answered any `PseudoStash` too; Rakudo's
  `PseudoStash` (`MY::`) is not a `Stash` and has no such method, and neither does mutsu's now.
- `Raku.legacy` (`method legacy(Raku:U:)`, ADR-0098) is a `Systemic` row of `Raku` flagged
  `TYPE_OBJECT_OK`, which §9.196's hook offers the type object. On an instance it raises
  `X::Parameter::InvalidConcreteness`, as Rakudo does; the gate's arm let the instance fall through
  to "No such method".

The class-method rows' instance refusal (`type_object_only`) now raises that typed exception
instead of an `X::AdHoc`, and `RuntimeError::parameter_invalid_concreteness` wraps its invocant
message at 72 columns as Rakudo's `naive-word-wrapper` does, rather than at a fixed word. Pinned in
`t/vm/scope/stash-merge-symbols-row.t` and `t/routines/invocant-concreteness-message-wrap.t`.

`IO::Spec::CurUpDir.ACCEPTS` keeps its gate: Rakudo has no such class. `$*SPEC.curupdir` is a
`Block` (`-> str $dir { $dir ne '.' && $dir ne '..' }`), so the fix is to answer one, not to give a
class mutsu invented a row (#13959).

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

### 9.24 Slice 3G: the metaobject protocol (2026-10-07)

Branch `refactor/11276-3g-ctors-mop`. Inventory (`scripts/method-rows-report.py --arms`): the 58 quoted-name
arms of `dispatch_classhow_method` (`runtime/methods_classhow_dispatch.rs`, 2,184 lines) plus the
hand-written name list `is_classhow_method` that gated it, and the `new` of every built-in type, spread over
about forty sites that each also ask "does the user class define `new`".

**MOP family.** A mutsu `Perl6::Metamodel::*HOW` instance is one dispatcher over many HOW kinds, each call
carrying the type object first (`how_dispatch_args`). Rakudo declares the metamethods on its HOW classes, so
each arm is now a row of the owner that declares it: `Metamodel::ClassHOW` (46), `Metamodel::SubsetHOW`
(`refinement`, `refinee`), `Metamodel::CoercionHOW` (`nominalize`, `coerce`), `Metamodel::ParametricRoleHOW`
(`pretending_to_be`, `parameterize`, `set_body_block`, `pun`), `Metamodel::ParametricRoleGroupHOW`
(`candidates`), `Metamodel::CurriedRoleHOW` (`curried_role`) and `Metamodel::NativeHOW` (`nativesize`,
`unsigned`). The recognition table and the oracle snapshot gain the seven owners (the snapshot generator
takes `Metamodel::*` owners as they are: their own HOW is an `NQPClassHOW`, which has no `.defined`).

Mechanism: none of these has a shape, so every row is `OWNER_ONLY` and reached by `invoke_owner_raw`, which
differs from `invoke_owner` in one respect: no argument is split off as named, because a metamethod reads its
own flags (`.^methods(:all)`) and the row is found by the full count. A handler is one `Interpreter` method
(`runtime/methods_classhow_arms_*.rs`; the arm's body, moved verbatim); the three arms with a residual guard
(`rw` on a role, `candidates`, `set_body_block`) keep it as a `*_applies` predicate the row asks first. The
gate `is_classhow_method` is the rows plus the `EnumHOW`/`DefiniteHOW` names, whose own dispatchers still
answer before the rows. A call no row answers is `X::Method::NotFound`, as the `_` arm was.

### 9.25 Slice 4, first step: one enumeration of the native base candidates (2026-10-07)

Branch `refactor/11276-4-resolver-cutover`. Inventory: `dispatch_next_candidate`
(`runtime/builtins_dispatch_next.rs`) ends a deferral chain in one of ten
`native_*_next_candidate` bridges (grammar `parse`, built-in grammar rule, `Mu` base for
`BUILDALL`/`POPULATE`/`clone`/`new`, `is Array` / `is Hash` / baggy backing storage, `gist`/`Str`/`raku`,
a mixin's inner value, an `augment`ed core type, the metamodel HOW), enumerated by hand at three sites
with three different orders (a user MRO exhausted, a multi method exhausted, a method with no dispatch
frame). None is a `MethodDef`, so the user-MRO walk never reaches them.

This step makes the enumeration one thing: `NativeBase` names each bridge, `NATIVE_BASE_EXHAUSTED`,
`NATIVE_BASE_MULTI` and `NATIVE_BASE_NO_FRAME` are the three orders (unchanged), and
`native_base_next_candidate` is the single place that walks one. No behaviour change;
`t/nativecall/native-base-candidates.t` pins one case per bridge family.

What is left of slice 4: a `DeferralEntry::Native { owner }` produced from `resolve_sequence`'s `Native`
candidates and invoked through `invoke_owner`, so the bridges that only call a builtin on the receiver
(`CoreType`, `AnyBase`, `MixinBase`, the storage three) become entries of the sequence instead of probes at
exhaustion. `augment` and the single-method fast path (no frame is pushed) are the part to design first.

### 9.26 Slice 4, second step: `DeferralEntry::Native` for an augmented core type (2026-10-07)

Branch `refactor/11276-4-native-deferral-entry`. The `CoreType` bridge (the builtin behind a method
`augment`ed onto a core type) is no longer a probe at exhaustion: `build_method_dispatch_frame` appends a
`DeferralEntry::Native { name }` to the frame when the receiver is a native value on which the user declared the
method (`core_type_receiver_has_user_override`), and `dispatch_next_candidate` runs it through
`run_core_type_builtin` (the builtin with the augmentation hidden from the override gate). Such a call now always
has a frame, so the bridge is gone from all three `NATIVE_BASE_*` orders. `callwith` replacement arguments are
honoured as for the other entries. Left: `AnyBase`, `MixinBase` and the three storage bridges.

### 9.27 Slice 4, third step: the default rendering behind a user `gist`/`Str`/`raku` (2026-10-07)

Branch `refactor/11276-4-any-mixin-entries`. The `AnyBase` bridge is a `DeferralEntry::Native` too:
`build_method_dispatch_frame` appends it when a user `gist`/`Str`/`raku` is declared on an `Instance`, and the
entry's arm runs `any_base_native_entry` (the shared default rendering) for an `Instance` receiver and the core-type
builtin otherwise. `native_any_base_next_candidate` keeps one job, resolving the receiver for the frame-less lanes
(and is dropped from the three `NATIVE_BASE_*` orders). `MixinBase` is not moved: its frames are built by
`dispatch_mixin_method_call` (`methods_mixin_dispatch.rs`), a second builder, and the storage bridges depend on
`is_container_protocol_method` frames; both need one shared frame builder first.

### 9.28 Slice 4, fourth step: the builtin behind a role mixed into a native value (2026-10-07)

Branch `refactor/11276-4-frame-builder`. `dispatch_mixin_method_call` builds its own frame for a role method
called on a `Mixin`; when the inner value is native (no `MethodDef`s) the frame now carries one
`DeferralEntry::Native { name }`, and the entry's arm answers a `Mixin` invocant through
`mixin_base_native_entry` (the former bridge body: `STORE`, the in-place hash `ASSIGN-KEY`/`DELETE-KEY`, the array
mutators, then the builtin on the inner value). `MixinBase` is gone from the `NATIVE_BASE_*` orders and
`native_mixin_base_next_candidate` is deleted. The `Native` arm now serves three receiver kinds (a core-type value,
an `Instance`, a `Mixin`). Left: the three storage bridges (`is Array`/`is Hash`/baggy subclasses), `MuBase`,
`GrammarParse`, `GrammarBuiltinRule` and `Metamodel`, whose receivers are user instances whose frames come from
`build_method_dispatch_frame`'s override flags.

### 9.29 Slice 4: what was tried for the remaining bridges, and why it stops here (2026-10-07)

Steps 1-4 (§9.25-§9.28) moved the bridges whose existence the frame builder can decide from the call alone:
`CoreType` (a user method declared on a core type), `AnyBase` (a user `gist`/`Str`/`raku` on an instance) and
`MixinBase` (a role method on a `Mixin` whose inner value is native). Those three are `DeferralEntry::Native`.

Tried and dropped, on the branch `refactor/11276-4-instance-bridges` (never pushed): give the remaining bridges
(`GrammarParse`, `MuBase`, `ArrayStorage`, `HashStorage`, `BaggyStorage`, `Metamodel`) the same treatment by
pushing a `Native` entry from `build_method_dispatch_frame`'s override flags and emptying
`NATIVE_BASE_EXHAUSTED`. With the `Native` arm routing `Instance`/`Package` receivers to those bridges,
1,021 `t/` files pass except four, and the four show the real obstacle:

- The flags decide *whether a frame is forced* (a cost guard), not *whether a bridge applies*. A bridge applies
  when the receiver **value** carries the backing storage (`__mutsu_array_storage`, `__mutsu_hash_storage`,
  `__baggy_data__`) or is a punned role's type object, and no static question about the class name answers that:
  `role MK is Hash { method keys { callsame } }` applied with `my %h is MK` has a role as its receiver class, and a
  `push` override on `class C is Array` is not a "container protocol" name. Widening the flags to every user method of
  a class with a container base fixes the class case and not the role case (`t/control/proxy-topic-for-alias.t`,
  `t/oo/method/nextsame-in-qualified-method-call.t`).
- Pushing the entry whenever the receiver is an `Instance` would force the non-empty frame build (the MRO walk)
  on every user-method call of a program that names a deferral builtin anywhere, which is the cost
  `empty_method_dispatch_frame` exists to avoid.

So these six bridges are the same single question asked of the receiver at exhaustion ("which native
implementation backs this value?"), and the sound replacement is to answer it with rows, not with more frame flags:
`Array`/`Hash`/`BagHash` rows reached by `invoke_owner` on the storage value (the 3C remainder: `is Array`/`is Hash`
subclass receivers have no shape), `Mu`'s `new`/`BUILDALL`/`POPULATE`/`clone` rows (3G remainder: constructors), the
`GrammarHOW`/`ClassHOW` rows (done in 3G part 1) and the grammar `parse` rows. Slice 4 is therefore finished
when those owners have rows; the resolver then asks "is there a row for this owner on the storage value?" in
`resolve_sequence`, and `NATIVE_BASE_EXHAUSTED` and `NATIVE_BASE_NO_FRAME` are deleted by slice 5.

### 9.30 The rendering and identity names, scalar owners (2026-10-07)

Remainder item 1 of the plan (`refactor/11276-rendering-identity-rows`). The shared `match` that §9.16 and §9.17
deferred is split by owner group, not all at once: this step does the scalar owners, whose shapes are open and
whose answers need no interpreter.

- `WHICH` is one function now (`methods_0arg/which.rs::which_of`, with `has_value_identity`), moved out of
  `dispatch_core_coerce`; the cascade arm is a call to it and the rows call the same function.
- `gist`, `raku` and `Bool.Str` are `scalars/render.rs` handlers (`Handler::Narrow`) with 18 rows:
  `Int`/`Num`/`Rat`/`Complex`/`Str` (`gist`, `raku`, `WHICH`) and `Bool` (`gist`, `raku`, `Str`). The cascade's
  `Bool`, `Rat` and `Str` arms in `dispatch_core_repr` call the handlers; a rational with a zero denominator
  stays in the cascade (its error needs the interpreter's context), so the handlers decline it.
- Not registered, because Rakudo does not declare them on these owners: `Int.perl`, `Str.perl` (the table keeps
  them as recognition rows) and `Bool.WHICH`; and `fmt`, which takes a format and is the text-methods-with-arguments
  family.
- Left in the shared `match` for the next steps: the collections (`Array`, `List`, `Hash`, `Map`, `Pair`, `Range`,
  `Seq`, `Set`/`Bag`/`Mix` and their hash forms), `Version`, `Blob`, `Capture`, `Nil` and the objects group
  (`Exception`, `Failure`, `Code`, `Signature`, `Backtrace`), `Cool`/`Any`/`Mu`, and `clone`/`fmt` everywhere.
  Finding: `(5 but "x").gist` answers `5` in mutsu and `x` in Rakudo; the mixin path does not take the rows.

### 9.31 The rendering and identity names, collections (2026-10-07)

Second step of the rendering and identity names (`refactor/11276-rendering-collections`), after §9.30.

- `WHICH` rows (7) on `Array`, `Hash`, `Pair`, `Range`, `Set`, `Bag` and `Mix`, the owners Rakudo declares it on,
  calling the same `which_of` as the cascade. `List`, `SetHash`, `BagHash` and `MixHash` reach them through the MRO.
- `gist` and `raku` rows (12) on the six quant hashes, calling `setbagmix_gist` / `setbagmix_raku`, and `Range.raku`
  (Rakudo declares no `Range.gist`; the cascade answers it with the same text). The cascade's `Set`/`Bag`/`Mix`
  and `Range` arms in `dispatch_core_repr` call the handlers, so the three hand-copied arms are gone.
- Left: `Array`/`List`/`Hash`/`Map`/`Pair`/`Seq` `gist`/`raku`/`Str`/`fmt` (the cycle- and user-`gist`-aware
  `GistRoute` walk), `Version`, `Blob`, `Capture`, `Nil`, the objects group, `Cool`/`Any`/`Mu`, `clone`.

### 9.32 The `Blob` and `Buf` shapes (2026-10-08)

Slice 3B remainder, item 2.5 of *Plan from here* (`refactor/11276-blob-buf-shape`); it answers §8.3.

**Decision (§8.3).** Rakudo has no inheritance between `Blob`, `Buf` and `utf8`: each is a class of its own
(`Blob.^mro` is `Blob, Any, Mu`; `Buf`'s is `Buf, Any, Mu`) with its own copy of every method, because the role
`Blob` is composed into them. mutsu keeps the recognition table's folded owner `Blob` (it is what `.^can` reads), and
the method table gets two shapes:

- `DispatchShape::Blob`: `Blob`, `Blob[uintN]`/`Blob[intN]`, `utf8`, `utf16`, `utf32`;
- `DispatchShape::Buf`: `Buf`, `Buf[uintN]`/`Buf[intN]`.

Both are closed, decoded from the instance's class name (`DispatchShape::from_instance_class`); `buf8`, `blob16` and
the like are normalised to the parameterised spelling before an instance exists, so they need no entry. A user
subclass, `Blob[num32]` and a class composed over a buffer have no shape and take the cascades.

**Rows.** Each read-only row is registered once per owner, `Blob` and `Buf`, with one shared handler: `elems`, `bytes`,
`of`, `list`, `contents`, `reverse`, `Bool`, `gist`, `raku`, `Str`, `Buf` (and `Blob`, declared on `Buf` only), the
twelve `read-*` accessors at one and two arguments and `subbuf` at one and two (`blob_read.rs`). The seven
mutators and `reallocate` are `Handler::Mut` rows of `Buf` alone, reached through `invoke_mut` (`mutating::owners_of`
answers `Buf` for a `Buf`-shaped instance). The cascade's arms (`dispatch_core_repr`, `coercion`, `dispatch_core_unicode`,
`dispatch_1arg`, `dispatch_2arg`, the by-value mutator arms of `methods_call_dispatch`) call the same functions, so
a receiver with no shape is answered by the code the rows run.

**Declined arguments.** `subbuf` and the `read-*` offset decline a `WhateverCode` (the compiled call evaluates
it against the length) and any non-number, so `subbuf(*-2)` and a `Str` offset keep their cascade answers.

**Mutators.** A named binding goes through the existing by-name routines (`buf_mutate_method`,
`buf_pop_shift_splice`, `buf_reallocate`), which re-seat the binding through its shared cell. A receiver with no name
is mutated on a copy that is the answer (`Buf.new(...).append(...)`), as the by-value arms always did; `pop` and
`shift` of a temporary answer the element and leave the buffer alone. `Blob.push` still answers
"Cannot modify immutable Blob" (Rakudo: "Cannot resolve caller push(Blob:D: ...)").

**Left.** `decode` (the encoding registry needs the interpreter: a `Handler::Interp` row), `subbuf-rw` (a write-through
`Proxy`), `new`/`allocate` (3G remainder), the `write-*` family (no recognition rows), `Capture`, and the shape-less
receivers above. `rakudo_method_tables.txt` has no `Blob`/`Buf` lines because its generator skips roles; the rows are
checked against the folded owner `Blob` of the recognition table, whose `DECLARED` bits this slice completes (the
mutators, `reallocate`, `Blob.Blob`, seven `read-*` names).

### 9.33 `Blob.decode` and `Buf.decode` (2026-10-08)

Follow-up to §9.32 (`refactor/11276-blob-decode`).

- `decode` at zero and one argument is a `Handler::Interp` row of both `Blob` and `Buf` (`blob_decode.rs`), with
  `replacement` (and the ignored `strict`) as its named arguments: the encoding registry and the newline mode are
  the interpreter's, so the pure entries skip it and the entries that have an interpreter answer it.
- There were four decoders: the pure `decode_buf_method` behind the 0- and 1-argument cascade arms, the
  interpreter's `dispatch_decode`, the callers' newline translation of the pure answer (`vm_native_dispatch`,
  `methods_call_dispatch`) and `try_native_encode_decode`. The row and `dispatch_decode` now end in one
  `Interpreter::decode_buf`; the two pure arms and the two post-hoc translations are deleted. `decode_buf_method`
  stays for its other callers (`utf8.Str`, the comparison and concatenation coercions), which never needed a
  registry.
- Not changed, filed as #12341: an unknown encoding name does not throw, and a 16-bit buffer defaults to UTF-16
  where Rakudo applies UTF-8 to its bytes.

### 9.34 `Date` and `DateTime`: `later`, `earlier`, `truncated-to`, `in-timezone`, `local` (2026-10-08)

3D remainder (`refactor/11276-3d-temporal-interp`), the interpreter-looking rows of `Date` and `DateTime`.

- **Rows.** `later` and `earlier` on both owners are `Handler::Named` rows at zero arguments whose named list is the
  fourteen units of Rakudo's `*%unit` (`second(s)` ... `year(s)`); `truncated-to` (one argument) on both owners and
  `DateTime.in-timezone` (one argument, Rakudo's signature has no default) are `Handler::Narrow` rows; `DateTime.local`
  is the `Handler::Interp` row, because it reads `$*TZ`. Nothing else here needs the interpreter: the first four are
  pure, the plan's "interpreter rows" label for them came from `runtime/methods_temporal.rs` living under `runtime/`.
- **One implementation.** The 680-line `runtime/methods_temporal.rs` moved into the method table's instance group
  (`instances/temporal_shift.rs`: `later`/`earlier` and the shared re-bless helpers; `instances/temporal_edit.rs`:
  `truncated-to`, `in-timezone`, `clone`). The rows and the cascade's `dispatch_temporal_method` call the same
  entry functions (`later_earlier`, `truncated_to`, `in_timezone`), so a subclass instance (no shape, kept through
  `rebless_*_result`) and a spelling no row binds answer from the same code.
- **What stays on the cascade path.** The units as a positional list of pairs (`.later((:2hours, :30minutes))`:
  Rakudo's only way to order several units) are not named arguments, so the guard step declines them, as it does a
  non-scalar positional. `.in-timezone` with no argument, `Date.in-timezone`, `clone`, `utc` for subclasses and the
  separator forms of `yyyy-mm-dd` stay in `dispatch_temporal_method` for the same reason (no row, or no shape).
  `Date.in-timezone` and the zero-argument `in-timezone` are mutsu extensions Rakudo does not have.
- **Not done.** `Date.IO`/`DateTime.IO` (the path form), `Instant`/`Duration`'s `base` and `polymod` (the `Real`
  machinery), `clone` (a rendering-and-identity name, plan item 1).
- **Found, not changed.** Rakudo refuses several named units in one call ("More than one time unit supplied"), mutsu
  applies them in a fixed order; the roast and ecosystem code that exists spells them as a list of pairs.

### 9.35 `Backtrace` and `Backtrace::Frame` (2026-10-08)

3D remainder, the objects group, `Backtrace` first (`refactor/11276-3d-backtrace`).

- **Two closed shapes**, `DispatchShape::Backtrace` and `BacktraceFrame`, decoded from the exact class name (a user
  subclass has another name and no shape), with catalog rows (`(Backtrace Any Mu)`, `(Frame Any Mu)`, as Rakudo). Both
  are closed: `elems`, `List`, `Seq` and `Stringy` are ancestor answers (`Any`, `Mu`) read from the `frames` list, and
  stay in the zero-argument cascade until a slice audits `Any`/`Mu` for the shapes.
- **23 rows** (`instances/backtrace.rs`), all pure. `Backtrace`: `Str`, `gist`, `list`, `flat`, `full`, `concise`,
  `summary`, `is-runtime` (a recognition row the table lacked), `AT-POS`, `outer-caller-idx`, and the introspection
  helpers `nice` (`:oneline`) and `next-interesting-index` (`:named`, `:noproto`, `:setting`) as `Handler::Named` rows,
  registered at every arity the cascades accepted (the recognition table claims `nice` at one argument and
  `next-interesting-index` at two). `Backtrace::Frame`: `subname`, `file`, `line`, `Str`, `code`, `is-routine`,
  `is-hidden`, `is-setting`.
- **One implementation.** The 0-, 1- and 2-argument cascades lost their `Backtrace` arms (the `.nice`/`.outer-caller-idx`
  introspection block, the `AT-POS` arm, the 140-line `Str`/`gist`/`full`/`concise`/`summary`/... block, the frame
  accessors); each now makes one call, `backtrace::answer`, which finds the row by owner, name and positional count and
  runs its handler. The call is needed because the debug cross-check re-runs every pure row through the cascades, and
  a generic instance fallback answers a `Backtrace` call differently (`list` rendered the text) — the same reason the
  `Seq` and `Blob` slices kept their arms as calls into the shared handlers.
- **Left.** `Backtrace::Frame.name` (a mutsu alias of `subname`; Rakudo has none) and `.raku`/`.gist` of a frame
  (`default_instance_repr`, which renders the synthesized `code` object); the rest of the objects group (`Mu`, `Code`,
  `Exception`, `Failure`, `Signature`, `X::AdHoc`, ...).
- **Found, not changed.** A user subclass of `Backtrace` answers `X::Method::NotFound` for `is-runtime` where Rakudo
  answers (the class-name decode is exact, as the arms it replaces were); `Backtrace.new` inside a sub has one frame
  fewer than Rakudo's (mutsu does not record the `new` frame).

### 9.36 `gist` of the collections (2026-10-08)

Third step of the rendering and identity names (`refactor/11276-collection-render-rows`), after §9.30 and §9.31.

- `gist` rows (4) on `List` (`Array` and `Slip`-free list receivers reach it through the MRO), `Seq`, `Hash` and `Pair`, the
  owners Rakudo declares it on. `Map.gist` is not registered: `Hash` declares its own, so a `Map` row would be reached by
  no shape.
- One renderer, `methods_0arg::collection_gist::collection_gist`, shared by the rows and by `dispatch_core_repr`. It owns the
  zero-denominator check, the `gist_route` walk (user-`gist` element: decline to the interpreter; cycle: `gist_value`),
  the 100-element cap and the per-element renderer. The two hand-copied `gist_item` functions (Array and Seq/Slip) and the
  Array/Seq/Slip/Pair/Hash gist arms of the cascade are gone; those arms now answer `raku`/`perl` only.
- The row declines (`None`) exactly when the route says the interpreter must dispatch an element's own `gist`, so a row
  never preempts a user `gist`.
- Left: `raku`/`Str`/`fmt` of the collections, `Version`, `Capture`, `Nil`, the objects group, `Cool`/`Any`/`Mu`, `clone`.

### 9.37 Slice 4: the subscript protocol's mutators are rows (2026-10-08)

`refactor/11276-4-resolver-cutover`. The storage bridges of §9.29 (`ArrayStorage`, `HashStorage`, `BaggyStorage`) and the
`Mixin` bridge each carried an inline copy of the subscript protocol's mutators next to the `Handler::Mut` rows of §9.23.
This step makes those mutators rows, so the bridges become one question ("which native value backs this receiver?") followed
by `invoke_mut` on a detached place.

- `mutating::subscript`: `ASSIGN-KEY` (2) and `DELETE-KEY` (1) on `Hash`, `SetHash`, `BagHash`, `MixHash` (in place through
  the shared node, answers as the VM arms did: the value, the old value, `True`/`False`, the old count/weight) and on the
  immutable `Set`, `Bag`, `Mix` (`X::Assignment::RO`). `Map.DELETE-KEY` keeps `refuse_map_removal`.
- `mutating::subscript_pos`: `ASSIGN-POS` (2) and `DELETE-POS` (1) on `Array`/`List`, with the element type check, the shaped
  array dimension check, the bound-element guard and the native-array refusal. The multi-dimension forms and `BIND-POS`
  (which needs the caller's argument sources) stay with the cascade.
- Deleted: the VM's `ASSIGN-KEY`/`DELETE-KEY` arms of `CallMethodMut` (about 340 lines; only the undefined-receiver case is
  left), `assign_key_in_place`, `dispatch_assign_key_method`, the cascade's single-index `ASSIGN-POS`/`DELETE-POS` arms and
  the `DELETE-KEY` shortcut, and the inline `ASSIGN-KEY`/`DELETE-KEY`/`push`/`append` bodies of the hash, baggy and mixin
  bridges. `Hash.push`/`append` on an `is Hash` instance's storage now stack a repeated key as `Hash.push` does everywhere
  else, instead of overwriting it.
- Left: `STORE` of the baggy bridge, the `BIND-*` family, `Mu`/grammar/metamodel bridges (they need the constructor rows of
  the 3G remainder).

### 9.38 `raku`, `Str` and `fmt` of the collections (2026-10-08)

Fourth step of the rendering and identity names (`refactor/11276-collection-render-identity-rows`), after §9.30, §9.31 and
§9.36.

- `raku` rows on `Array`, `List`, `Hash`, `Pair`, `Seq` and `Capture`, `Str` rows on `List`, `Hash`, `Pair`, `Seq`,
  `Capture` and `Range`, `Capture.gist` and `Seq.Stringy` (`collections/render_names.rs`). `Map` and `Array.Str` have no
  row of their own: an immutable `Map` is a `Hash` shape and `Array` inherits `List.Str` (the table's reach and declared-by
  tests say so). The `raku` rows are `Handler::Interp`: a typed container's `.raku` names its declared type
  (`Array[Int].new(...)`), which only the interpreter's container registry holds, and an element whose own `raku` is
  reachable through method dispatch (a user instance) must be rendered by the interpreter. The row declines in exactly the
  cases the two gates of `try_native_method_raw` did (`container_needs_raku_dispatch`, a typed `Hash` / `Array`), because
  the table runs in front of those gates. **A row that replaces a name the family cascade answers has to carry the
  cascade's pre-gates**: the first version of the `raku` row, a pure handler, printed `Pt<18>` for `[Pt.new(x => 1)].raku`.
- `fmt` (`collections/fmt.rs`): rows at arities 0, 1 and 2 on `Array`, `List`, `Map`, `Pair`, `Range`, `Seq` and the six
  quant hashes, all calling `fmt_native` (`methods_narg/fmt_contains.rs`). It replaces the three copies in the 0-, 1- and
  2-argument cascades (about 330 lines); the interpreter's fourth copy, `dispatch_fmt_with_user_coercion`, stays as the path
  a declined call takes (a `Format` object, a directive over an item that may carry a user `Str`/`Int`/`Numeric`).
- The cascades' `raku` arms for `Array`, `Seq`, `Pair`, `Hash` and `Capture`, and the `Capture.Str` arm, call the rows'
  renderers (`render_names::raku`, `str_row`).
- Left: `clone` (its typed-container metadata lives in the interpreter's registry, like `raku`), `Junction.Bool`/`gist`,
  `Nil`'s rows (no `Nil` shape), `Map.gist`, the list terminals, and `Cool.fmt` of a scalar (answered by the same
  `fmt_native`, reached through the cascade).
- **Found, not fixed.** `[Nm.new].fmt('%s')` (an item with its own `Str`) answers `nm` where Rakudo dies, because it calls
  `.fmt` on each item; `<a b>.fmt('%s%s')` answers `a b` where Rakudo dies on the argument count.

### 9.39 `Exception` and its subclasses, `Signature.gist`/`raku` (2026-10-08)

Third step of the objects group (3D remainder; `refactor/11276-exception-rows`), after `Backtrace` (§9.35).

- **Nine rows** (`instances/exception.rs`), all pure and `OWNER_ONLY`: `Exception` `message`, `gist`, `Str`, `backtrace`,
  `resume`; `X::AdHoc` `message`, `payload`; `CX::Warn` `message`; `X::TypeCheck::Assignment` `message`. The recognition
  table already declared all of them. Two rows (`Signature` `gist`, `raku`, `instances/signature.rs`) read an attribute the
  compiler rendered.
- **No shape.** An exception is an instance of a class named `Exception`, `X::...` or `CX::...`, and every `X::Foo` is
  one, so a shape decoded from a class-name list cannot name them. The rows are reached through their owner: `answer`
  walks the instance's owner chain (`X::AdHoc`, `CX::Warn` or `X::TypeCheck::Assignment` first, then `Exception`), exactly as
  the family cascade's ~190-line exception block did, and that block is now one call. The same holds for `Signature`.
  `.^can` and the resolver now find these as native candidates; the dispatch of a plain exception call is unchanged.
- The class-specific texts that were scattered over the `gist`, `Str` and `message` arms (`Unthrown Exception with no
  message`, `Something went wrong in (Exception)`, `Unexplained error`, the typed message, `<Class> with no message`) are
  one function, `text`, with the two callers' differences as arguments.
- Left: `line`, `file` and `filename` (mutsu's own accessors; Rakudo declares them on `X::Comp`, which the oracle snapshot
  lacks), `Exception.throw` (an interpreter row), `Failure` (the recognition table declares nothing on it), `Code`,
  `Mu`'s rows.

### 9.40 Slice 4: `STORE` of the quant hashes is a row (2026-10-08)

`refactor/12387-store-row-quanthash` (item 1 of #12387). `STORE` was folded by `quanthash_store` in three places: the baggy
`nextsame` bridge, the `is BagHash` VM delegate and the by-value interpreter entry. It is now one `Handler::Mut` row on each of
`SetHash`, `BagHash`, `MixHash` (re-initialize) and `Set`, `Bag`, `Mix` (`X::Assignment::RO`), in
`mutating::quanthash`, and the three callers reach it through `invoke_mut` (a detached place for the storage and by-value
cases). The bridges still answer the invocant, as the tied-variable declaration expects.

- Left: `BIND-KEY` / `BIND-POS` (they need the call's argument sources; an ADR amendment on the binding descriptor comes first),
  the `Mu`, grammar and metamodel bridges, and asking the storage question in `resolve_sequence`.

### 9.41 Slice 4: `BIND-KEY` and `BIND-POS` are rows; the place carries the argument sources (2026-10-08)

`refactor/12387-bind-key-pos-rows` (item 2 of #12387). Binding an element to the caller's *variable* needs the call's
argument sources, which a `Handler::Mut` row did not receive. Decision (the amendment §9.37 asked for):

- `ReceiverPlace::Var` carries `arg_sources` (the variable behind each positional, from the call site), set with
  `with_arg_sources`; a detached place and the interpreter's by-name entries carry none. A row reads one with `arg_source(i)`
  and asks `came_through_vm()` whether the call came through the VM. This is the interim form of ADR-0097's binding descriptor: when
  that lands the accessors change and the handlers do not.
- `install_source_cell` promotes the source variable into the element's shared cell in both halves of the dual store.
- Rows (`mutating::subscript_bind`): `Hash.BIND-KEY` (object hashes key by `.WHICH` and keep the key object),
  `Array.BIND-POS` (one index, scalar-variable source, in place through the node; a natively typed array, a negative index and
  a non-variable source decline to the cascade), and the `X::Bind` refusal of `BIND-KEY` on the six quant hashes. A call that
  did not come through the VM declines, exactly as before.
- Deleted: the VM's `BIND-KEY` arms for `Hash`/`Set`/`Bag`/`Mix` and the `BIND-POS` arm (about 290 lines). The rebuild-and-
  reinsert fallback of the old `BIND-KEY` arm is gone with them: a bind now always writes the shared node, so every alias of
  the hash sees it. What stays is `BIND-KEY` on an undefined receiver (it vivifies a hash) and the stash intercept.
- Left: the `Mu`, grammar and metamodel bridges (item 3) and asking the storage question in `resolve_sequence` (item 4).

### 9.42 Slice 4: the storage bridges are a frame entry, not an exhaustion probe (2026-10-09)

`refactor/12387-storage-bridges-in-resolver` (item 4 of #12387, first half). §9.29 found that the frame builder's override
flags decide *whether a frame is forced*, not *whether a bridge applies*, and that the question is about the receiver
**value**. This step asks it there:

- `invocant_carries_native_storage`: the receiver is an `Instance` with `__mutsu_array_storage`, `__mutsu_hash_storage` or
  `__baggy_data__` (a role punned onto a container has the storage too, whatever its class name). With a user method of that
  name on the class or a composed role, the frame builder forces the frame and ends it with a `DeferralEntry::Native`, as it
  does for an augmented core type. It replaces the protocol-name list (`is_container_protocol_method`), so a `keys`, `elems`
  or `push` override reaches the storage too. `Mu`'s names (`new`, `BUILDALL`, `POPULATE`, `clone`) are excluded: their base
  candidate is the `Mu` one, not the storage's `Array.new`.
- The `Native` arm of `dispatch_next_candidate` asks `native_storage_base_entry` first for an instance and falls back to the
  default rendering (`any_base_native_entry`). The three storage bridges are gone from `NATIVE_BASE_EXHAUSTED`.
- Left: `NATIVE_BASE_MULTI` and `NATIVE_BASE_NO_FRAME` still list the three bridges (a `multi method` override has its own
  frame kind; a single compiled method with no frame reads the samewith context), and the `Mu`, grammar and metamodel
  bridges, so `native_*_next_candidate` and the lists are not deleted yet.
- Found, not fixed: `class S is SetHash { method set(|c) { callsame } }` does not set (the storage reached through
  `try_native_method` has no mutating rows).

### 9.43 Slice 4: the storage bridges leave every probe list (2026-10-09)

`refactor/12387-storage-bridges-multi-noframe` (item 4 of #12387, second half), after §9.42. With the frame builder pushing a
`Native` entry for a storage-carrying receiver, emptying `NATIVE_BASE_MULTI` and `NATIVE_BASE_NO_FRAME` of the three storage
bridges changed no result in the `t/` and roast suites that exercise them (a `multi method` override in a class is a method
frame, not a sub-style multi dispatcher; a frame-less single compiled method has no deferral to resume). So they were dead
entries, not a case for a second frame kind:

- `NativeBase::ArrayStorage`, `HashStorage` and `BaggyStorage`, `NATIVE_BASE_NO_FRAME` and its call site are deleted.
  `NATIVE_BASE_MULTI` keeps `GrammarBuiltinRule`.
- `native_array_storage_next_candidate`, `native_hash_storage_next_candidate` and `native_baggy_storage_next_candidate` stay
  as the body of `native_storage_base_entry` (they read the innermost frame); folding them into the storage owners' rows is
  left with the `Mu` / grammar / metamodel bridges.

### 9.44 Slice 4: the metamodel and grammar built-in rule bridges are a frame entry (2026-10-09)

`refactor/12423-native-base-metamodel-grammar-bridges` (first item of #12423), after §9.43. Same move as §9.42, decided by the
receiver value, no new rows:

- The frame builder ends the frame with a `DeferralEntry::Native` for a user method on a metamodel-HOW class (or a native
  HOW mixin), and for a grammar's own `ws`/`alpha`/`ident`/... (`is_builtin_rule_name`, a cheap name test so ordinary grammar
  tokens do not pay for a frame).
- The `Native` arm asks `native_metamodel_next_candidate` and `native_grammar_builtin_rule_next_candidate` before the storage
  and default-rendering fallbacks. `NativeBase::Metamodel`, `NativeBase::GrammarBuiltinRule` and `NATIVE_BASE_MULTI` are
  deleted; `NATIVE_BASE_EXHAUSTED` keeps `GrammarParse` and `MuBase`.
- Left for #12423: `Mu.BUILDALL`/`POPULATE`/`clone`/`new`, the `Grammar` oracle-snapshot decision and the
  `parse`/`subparse`/`parsefile` rows.

### 9.45 Slice 4: the grammar parse and `Mu` bridges are a frame entry; the probe lists are gone (2026-10-09)

`refactor/12423-mu-base-and-grammar-parse-rows` (rest of #12423), after §9.44. The last two bridges move the same way:

- The frame builder pushes `DeferralEntry::Native` for a user `parse`/`subparse`/`parsefile` on a grammar and for a user
  (or role-provided, so a punned role's `new` counts) `new`/`BUILDALL`/`POPULATE`/`clone`. The `Native` arm asks
  `native_grammar_parse_next_candidate`, then `native_mu_base_next_candidate`, then the metamodel, grammar-rule and storage
  bridges, then the default rendering.
- An exhausted frame now simply answers `Nil`: `NativeBase`, `NATIVE_BASE_EXHAUSTED` and `native_base_next_candidate` are
  deleted.
- Deliberately not done: registering `Grammar.parse` and `Mu.*` rows. That needs the `Grammar` owner in the vendored oracle
  snapshot (`rakudo_method_tables.txt`), a decision for the maintainer; the frame entry does not depend on it. The
  `native_*_next_candidate` bodies stay as the implementation the `Native` arm calls, and `Mu.clone` still needs the
  typed-container registry before it can be a row.

### 9.46 Slice 4: `Grammar.parse`, `.subparse` and `.parsefile` are rows; the oracle snapshot gains `Grammar` (2026-10-09)

`refactor/12423-grammar-oracle-snapshot-and-rows` (the `Grammar` item of #12423), after §9.45. The maintainer approved
regenerating the vendored Rakudo method-table snapshot:

- `raku scripts/gen-rakudo-method-tables.raku > src/builtins/rakudo_method_tables.txt` after `Grammar` joined the owners in
  `native_method_row_table.rs`. The diff is exactly the three new `Grammar` lines (`declared`, `methods`, `only`); the header
  and every other owner are unchanged, so nothing else in the catalog moved with the regeneration.
- `ctors_mop/grammar.rs` holds three `OWNER_ONLY` `Handler::Interp` rows reached through `invoke_owner_raw(&["Grammar"], ..)`
  with the receiver first, like the `Metamodel::*HOW` rows. The handler is `dispatch_instance_parse`, the function the direct
  `G.parse(...)` call already uses, so the deferral and the direct call share one implementation.
- `native_grammar_parse_next_candidate` shrinks to building the argument list and asking the row.
- Still open on #12423: `Mu.BUILDALL`/`POPULATE`/`clone`/`new` as rows (`clone` needs the typed-container registry; `new`
  needs the "user class defines `new`" guard first).

### 9.47 Slice 4: `Mu.BUILDALL` and `Mu.POPULATE` are rows (2026-10-09)

`refactor/12423-mu-buildall-populate-rows` (the `Mu` item of #12423), after §9.46. Rakudo declares both on `Mu` (already in
the oracle snapshot, so nothing is regenerated). `ctors_mop/mu_base.rs` holds two `OWNER_ONLY` `Handler::Interp` rows that
answer the receiver, the already-built instance (mutsu builds natively before the user hook, `run_user_buildall_hook`);
`native_mu_base_next_candidate` asks them through `invoke_owner_raw(&["Mu"], ..)`.

Left for #12423: `Mu.clone` (needs the typed-container registry reachable from a row) and `Mu.new` (about 40 per-type `new`
sites, each interleaved with a "does the user class define `new`" check, so the constructors need that check as one guard
first). Both stay in `native_mu_base_next_candidate`.

### 9.48 Slice 4: `Mu.clone` and `Mu.new` are rows; the `Mu` base candidate is complete (2026-10-09)

`refactor/12423-mu-clone-row` (the `Mu` item of #12423), after §9.47.

- `Mu.clone`: the §9.38 obstacle (the typed-container registry) applies to cloning a *container* (`Array`, `Hash`), not to
  the instance clone this bridge answers. That clone needs only the interpreter's class-attribute registry, which a
  `Handler::Interp` row has, so `ctors_mop/mu_base.rs` holds an `OWNER_ONLY` row over `native_instance_clone_value`, the
  function the direct `.clone` call shares. The container clones stay with their own rows.
- `Mu.new`: an `OWNER_ONLY` row over `Interpreter::mu_new_base` (the nearest builtin ancestor's native constructor, the
  named-arguments-only check, then `bless`), the body the bridge used to hold inline. It is the base of a *deferral* only.
  The "user class defines `new`" guard the issue names as a prerequisite is for the **per-type** constructors, which this
  step does not touch: a plain `Foo.new(...)` still reaches `methods_object_dispatch_new.rs`, and `native_mu_base_next_candidate`
  is now four `invoke_owner_raw(&["Mu"], ..)` calls.
- Still open on #12423: the per-type constructor rows and their shared guard, and deleting `native_mu_base_next_candidate`
  with them.

### 9.49 Slice 4: the built-in constructors are one table, not method rows (2026-10-09)

`refactor/12423-per-type-constructor-rows` (the per-type constructor item of #12423), after §9.48.

**Why not rows.** The issue planned a `Type.new` row per built-in type. `rows_are_declared_by_rakudo` forbids that for most of
them: Rakudo declares `new` on `Array`, `List`, `Seq`, `Date`, `DateTime`, `Promise`, `Pair`, ... but `Hash`, `Map`, `Set`, `Bag`,
`Mix`, `Version`, `Channel`, `Supplier`, `Uni`, `StrDistance`, `CompUnit` and the rest inherit `Mu.new` (`T.^lookup('new').package`
is `Mu`), so a row owned by the type is not something the vendored oracle can declare, and regenerating the snapshot cannot
make Rakudo declare it. That is also the honest model: `Mu.new` allocates by type.

**What was done instead.** The constructors lived in three parallel type-name ladders. `src/runtime/native_ctor/` holds the
first as data: a sorted `CTORS` table (class name, constructor, `unless_user_new`, `fast`) over one function per former arm,
lifted verbatim, in six family files (`collections`, `numeric`, `io_temporal`, `concurrency`, `distribution`, `meta`).

- `dispatch_new_unallocated` replaces its 47-arm `match constructor_dispatch_name` (about 570 lines) with one `lookup`.
  A class a user declared under a builtin's name still skips the table (`constructor_dispatch_name`). `IO::CatHandle` is the
  one entry flagged `unless_user_new`.
- The VM's native construct path (`try_native_builtin_construct`) reads the same entries through the `fast` flag, which marks
  the pure-data ones; its 19 duplicate branches are gone. `Date` and `DateTime` stay out of it: the interpreter arm tries an
  `augment`ed `new` first, the VM path does not.
- Left: the third ladder (`methods_dispatch_new.rs`: `ObjAt`, the allomorphs, `Failure`), the VM-only arms (`Int`, `Num`, `Str`,
  `Capture`, `Buf`/`utf8` by predicate) and the parametric branch below the table (`Array[Int].new`, `Hash[Int,Str].new`, roles).
  `native_mu_base_next_candidate` stays: its `Mu.new` row calls `native_builtin_new_next_candidate`, which is the same
  nearest-builtin-ancestor lookup, now table-backed through `try_native_builtin_construct`.

### 9.50 Slice 4: the third constructor ladder joins the table (2026-10-09)

`refactor/12423-ctor-ladder-remainder`, after §9.49. `Int`, `Num`, `Str`, `IntStr`/`NumStr`/`RatStr`/`ComplexStr`,
`ObjAt`/`ValueObjAt`, `Capture` and `Failure` are `CTORS` entries (`native_ctor/basic.rs`); the `.new` arms of
`methods_dispatch_new.rs`, the basic-type arms of `dispatch_new` and the matching branches of
`try_native_builtin_construct` are gone. The `Buf`/`Blob`/`utf8` entries are `fast`; the VM keeps only the
`is_native_buf_constructible` fallback for spellings the table does not list, and `Date`/`DateTime`. `Capture.new` now also
dies on a positional argument through the interpreter path, as the VM path already did.
Left: the parametric branch (`Array[Int].new`, roles) and folding `native_*_next_candidate` into the rows.

### 9.51 Slice 4: no second parametric constructor branch (2026-10-09)

`refactor/12423-parametric-ctor`, after §9.50. The "parametric branch below the table" §9.49 listed as left was a stale
duplicate: `ctor_array` / `ctor_hash` already receive `type_args` through `CtorCall` and run the shared
`try_native_array_construct` / `try_native_hash_construct`. The copy (about 150 lines, with its own `make_shaped_array`) is
deleted. The role-pun branch stays: it is role composition, not a built-in constructor. Left on #12423: folding
`native_*_next_candidate` into the rows' handlers.

### 9.52 Slice 3D: `Failure`'s `exception`, `handled`, `gist`, `raku`, `Str` and `Bool` are rows (2026-10-09)

`refactor/12390-objects-rows-remainder` (the `Failure` item of #12390). The recognition table's `Failure` rows now carry
`DECLARED` (Rakudo declares all six on `Failure`), and `method_table::failure` registers them as `OWNER_ONLY` narrow rows.
`failure::answer` is the cascade's single entry: the zero-argument cascade calls it where the `exception`/`handled` safe
accessors were, and the `Failure` arm of the repr dispatch reaches it for `gist`/`raku`/`perl`/`Str` before its
throw-in-value-context fallback. `Bool` still defuses the failure through the shared handled registry. The `Failure`
arms of `dispatch_core_coerce.rs` (`Bool`/`Str`) stay behind the early answer, now unreachable for a plain `Failure`.
Left on #12390: `Failure.throw`/`resume` and `Exception.throw` (interpreter rows), `line`/`file`/`filename` (needs `X::Comp`
recognition rows), `Code`, `Mu`.

### 9.53 Slice 4: a `Native` entry names its bridge (2026-10-09)

`refactor/12423-fold-native-next-candidates`, after §9.51. `DeferralEntry::Native` carries a `NativeBase`
(`GrammarParse`, `MuBase`, `Metamodel`, `GrammarRule`, `Storage`, `Value`) that `build_method_dispatch_frame` decides from the
receiver and method name it already computed (`grammar_parse_override`, `mu_base_override`/`new_base_override`, `how_receiver`,
`grammar_rule_override`, `container_protocol_override`, else the value-decided core-type / default-rendering / mixin base). The
advance arm of `dispatch_next_candidate` is a `match` on it; the `.or_else` chain that probed four bridges by the dynamic
`samewith_context_stack` name is gone, so a core-type `parse` augmentation no longer reaches the `Grammar` row. The bodies keep
their guards and are renamed `native_*_base` (no longer "candidates" of a probe). The `NO_FRAME` fallbacks at the end of
`dispatch_next_candidate` still call `native_metamodel_base` / `native_builtin_new_base` directly: they run when no frame exists,
so there is no entry to name a bridge.

### 9.54 Slice 3D: `Exception.throw` and `Failure.throw` are rows (2026-10-10)

`refactor/12390-exception-throw-rows`. The hand-written `.throw` fast path in `methods_0arg/mod.rs` is gone: `throw` is a pure
`Narrow` row on `Exception` (Rakudo declares it only there; the subclasses reach it through the owner chain; it declines for `CX::*`, whose composed roles the
interpreter must inspect) and on `Failure`, where it now raises the wrapped exception like `Failure.Str`. The `throw`/`rethrow`
interpreter gate for classes with a computed `message` is unchanged. Not interpreter rows after all: the handler only builds the
`RuntimeError`. Left on #12390: `line`/`file`/`filename`, `Code`, `Mu`, `Backtrace` ancestors, `Date.IO`/`Instant`/`Match`/`RakuAST::*`.

### 9.55 Slice 3D: `Signature.arity`/`count`/`params`/`returns` and `Supply.list` are rows (2026-10-10)

`refactor/12390-signature-supply-rows`. The inventory had counted `Signature`'s four methods as interpreter rows, but each only reads
the structured parameter list (`extract_sig_info`) or an attribute, so they are `Narrow` rows in `method_table::signature`
reached by owner from the zero-argument cascade. `Supply.list` moved out of the `list`/`Array` arm of `methods_0arg/coercion.rs` into
`method_table::supply::listify`, shared by the row and the arm (`.Array` stays an arm, Rakudo declares it elsewhere). The slow-path
`arity`/`count` branches for `Sub` values stay until the `Code` slice. Left on #12390: `Code`, `Mu`, and the items the inventory does
not count (`line`/`file`/`filename` are mutsu's own accessors on `Exception`, not declared by Rakudo's `Exception`, so no row is owed
until `X::Comp` is recognized; `Backtrace` ancestors; `Date.IO`, `Instant`, `Match`, `RakuAST::*`).

### 9.56 Slice 3D: `Mu`'s base answers are deferral-base rows (2026-10-10)

`refactor/12390-mu-rows`. The `Mu` item of #12390 is registered the way `BUILDALL`/`POPULATE`/`clone`/`new` already are (§9.47):
`OWNER_ONLY | DEFERRAL_BASE` interpreter rows in `method_table::ctors_mop::mu_base` for `defined`, `Bool`, `so`, `not`, `WHICH`, `WHERE`,
`gist`, `Str` and `raku`. They answer **only** as the last candidate of a user override's `callsame`/`nextsame` chain, so no receiver's
ordinary answer changes (the shape audit of §9.17 is not needed: an `OWNER_ONLY` row is never reached through a shape's MRO). This fixes
wrong answers: `callsame` out of a user `Bool`/`defined`/`so`/`not`/`WHICH` answered `Nil`, out of a type object's `gist` answered `Nil`, and a
user `gist` calling `callsame` skipped the receiver's own `raku` (`Mu:D.gist` is `self.raku`). `mu_base_override` in `method_dispatch_lazy.rs`
selects the base for these names (`Str`/`raku` only for a type object, where the instance default stays `any_base_native_entry`).
Left on #12390: `Mu`'s `say`/`print`/`put`/`note`/`new`/`HOW`/`WHAT`/`WHY`/`DEFINITE` (plain calls, not deferral bases: they need the shape
audit), `Code` (ADR-12523), `Backtrace` ancestors, `Date.IO`/`Instant`/`Match`/`RakuAST::*`.

### 9.57 Slice 3D: `Mu`'s output methods are deferral-base rows; the rest of `Mu` stays in the cascade (2026-10-10)

`refactor/12390-mu-plain-base-rows`. `Mu.say`, `Mu.print`, `Mu.put` and `Mu.note` join §9.56's `OWNER_ONLY | DEFERRAL_BASE` rows: a user
`method say { ... callsame }` (a common wrapper around logging) reached no base and printed nothing; it now writes the receiver's `gist`
(`Str` for `print`/`put`, to `$*ERR` for `note`) through the same `dispatch_say`/`dispatch_print`/`dispatch_put`/`dispatch_note` the
plain call uses, declining for an `IO::CatHandle` whose write methods are its own.

The last six declared `Mu` names stay in the cascade, each for a stated reason, which closes the `Mu` item of #12390:

- `WHAT`, `HOW`, `WHY` and `DEFINITE` are pseudo-methods the compiler lowers before method dispatch (`WHAT`/`HOW` also as macros of the
  MOP), and a `callsame` out of a user override already answers Rakudo's result (probed: `WHAT` the type, `HOW` the ClassHOW, `WHY` `Nil`,
  `DEFINITE` `True`). A row would duplicate the lowering, not replace it.
- `Int` and `Numeric` of a plain object throw `X::Multi::NoMatch` in Rakudo (`Cannot resolve caller Int(A:D: )`) where mutsu throws
  `X::Method::NotFound` for the instance and stringifies the type object; that is a wrong answer of the cascade (filed as [#12551](https://github.com/tokuhirom/mutsu/issues/12551)),
  not a missing row, and a base row would have to reproduce the same error.

Left on #12390: `Code.gist`/`raku` (ADR-12523), `Backtrace`'s ancestors, `Date.IO`, `Instant`, `Match`, `RakuAST::*`.

### 9.58 Slice 3D: `Date.IO`, `DateTime.IO` and `Instant`/`Duration` `base` are rows (2026-10-10)

`refactor/12390-temporal-io-base-polymod`. `Date.IO` and `DateTime.IO` are interpreter rows (`method_table::instances::dateish_io`): `IO` is
`Cool`'s, the rendering of the date becomes an `IO::Path` through `Interpreter::make_io_path_instance` (the `$*CWD`/`$*SPEC` factory), and
`:CWD`/`:SPEC` are accepted and ignored, as the generic `IO` arm of `methods_dispatch_match.rs` does. `Instant.base` and `Duration.base`
(radix, and radix with digits) are pure rows in `instances::instant` that hand the seconds they hold to the `Num`/`Rat` `base` of
`native_method_1arg`/`native_method_2arg`, the way `Rat($epsilon)` already does.

`Instant.polymod` and `Duration.polymod` stay in the cascade: the row is variadic, and the cascade's answer is wrong where Rakudo keeps
the invocant's numeric type in the remainders ([#12557](https://github.com/tokuhirom/mutsu/issues/12557)); a row would have to reproduce
the wrong answer or fix it first. Left on #12390: `Backtrace`'s ancestor answers, `Match`'s seven pure rows, `RakuAST::*`, `Code.Str`
(ADR-12523).

### 9.59 Slice 3D: `Match`'s `Int`, `Num`, `Numeric`, `chars`, `not`, `WHICH` and `replace-with` are rows (2026-10-10)

`refactor/12390-match-rows`. The last seven declared-and-unregistered `Match` names join the pure rows of §9.18. `Int`, `Num`, `Numeric`
and `chars` are what `Cool` gives any value, applied to `Match.Str`, so each hands the matched text to the string's own method (the
answers, including the `Failure` for non-numeric text, are the string's); `not` negates `Match.Bool`; `WHICH` is the match object's
identity; `replace-with` moves from `dispatch_1arg.rs` into `regex_match::replace_with`, which the cascade arm still calls. Behaviour is
unchanged; the inventory for `Match` now shows no declared row unregistered.

With this slice every declared row of the objects group and of `Date`/`DateTime`/`Instant`/`Duration`/`Match` is registered or named in an
ADR with its reason (`Code.Str`, `Instant.polymod`/`Duration.polymod`, `Mu.WHAT`/`HOW`/`WHY`/`DEFINITE`/`Int`/`Numeric`), which is the
close condition of #12390 apart from `Backtrace`'s inherited answers and `RakuAST::*` (the oracle snapshot lists none of its owners).

### 9.60 The `Any` interpreter rows: `map`, `grep`, `first`, `reduce`, `produce`, `rotor`, `skip`, `squish`, `eager`, `iterator`, `match`, `classify`, `categorize` and `Hash`'s `classify-list`/`categorize-list` (2026-10-10)

`refactor/11276-any-interp-rows` (issue #12388). Fifteen `Handler::Interp` rows in `method_table::collections::any_interp`, thirteen on `Any`
and two on `Hash`. Each handler is the one `Interpreter::dispatch_*_method` the cascade's arm now calls too: `dispatch_map_method`,
`dispatch_grep_method`, `dispatch_first_method`, `dispatch_reduce_method`, `dispatch_produce_method`, `dispatch_rotor_method`,
`dispatch_skip_method`, `dispatch_squish_method`, `dispatch_eager_row`, `dispatch_iterator_method`, `dispatch_match_row`,
`dispatch_classify_method` and `dispatch_classify_list_method`. Eight were extracted from arms of `methods_dispatch_match{,2,3}.rs`
(`grep`, `first`, `rotor`, `squish`, `classify`, `classify-list`, `match`, `eager`); the arms stay in the slow path for the receivers the table
has no shape for (a `Supply`, a user instance), as `collate` does.

- **Arguments.** The rows are `ANY_ARGS | ANY_NAMED`: the guard admits a `Sub`, a `Regex` or a `Whatever` as the positional argument, and every
  named argument (`grep`'s `:k`/`:v`/`:kv`/`:p`, `first`'s `:end`, `rotor`'s `:partial`, `classify`'s `:as`/`:into`) reaches the handler, which
  hands them on as the trailing pairs the cascade has always taken. `skip` and `rotor` are `SLURPY` from arity 0.
- **Pre-gates (the ADR §9.38 lesson).** A row runs in front of the probes of `try_native_method_raw`, so the handlers decline what those probes
  deferred to the interpreter by name (`deferred_to_interpreter`): an `IterationBuffer`, an instance whose class supplies `iterator` but not the
  method, a mixin composing `iterator`, and for `map`/`grep` a lazy pipe source (`is_lazy_pipe_source`). A not-yet-run `Seq` is already declined
  by the table (`Receiver::of_settled`), so consumption is unchanged.
- **Behaviour.** No change; `t/oo/method/any-interp-method-rows.t` pins the calls against Rakudo's answers, the laziness and `Seq`-consumption cases
  and the user-`iterator` case.
- **Left on #12388.** `Any.splice` (a `Handler::Mut` question, it is `Array.splice`'s ancestor), and the `callsame`/`nextsame` out of a user
  override into these rows, which is slice 4's `resolve_sequence` work (#12387).

### 9.61 The list terminals on their declaring owners (2026-10-10)

`refactor/11276-collection-tail-rows` (issue #12389, item 4). Eight rows join `collections::terminals`: `List.head` and `Map.head` (counted),
`List.tail` and `Array.tail` (counted), `List.sum`, and `flat` (with `:hammer`) on `Map` and `Range`. Each shares the handler `Any`, `List`
or `Seq` already used, so a receiver the handler declines (a lazy list, a hash that reads as pairs) takes the cascade as before. A
differential probe over nine receiver kinds and fourteen terminals answers the same before and after.

- The tests that said `Any.head` / `Any.tail` / `Any.sum` answer for `List` and `Array` now say the declaring owner does
  (`List`, and `Array` for `tail`, `Map` for `Hash`'s `head`).
- **`Any.join` is not registered.** The shared `join` handler declines a `Hash` receiver, and `every_row_is_reached_and_answers` rightly
  forbids a row that does not answer for a shape that reaches it. `Hash.join` joins the pairs through the cascade; it needs its own handler first.
- **Left on #12389**: `Any.batch`/`hash`/`list`/`serial` (the handlers are shape-specific), `List.chrs`, `List.AT-POS`/`Array.AT-POS` (`positional.rs`
  explains why they have no row), `Map.AT-KEY`/`Str`, `clone`, `Junction`/`Nil` and `Map.gist`.

### 9.62 `clone` of the aggregates (2026-10-10)

`refactor/11276-clone-rows` (issue #12389, item 1). Three `Handler::Interp` rows in `method_table::collections::clone`: `clone` on `Array`, `Hash`
and `Pair` (arity 0; a `Map` shares the `Hash` shape, so `Hash`'s row answers it). One routine, `container_clone`, copies the receiver (the cascade's `clone` arm in `dispatch_core_coerce.rs`
calls it for `Array`/`Hash`/`Pair`/`ValuePair`); the row then carries the receiver's declared container type over with
`container_type_metadata` / `tag_container_metadata`, which is why it is an interpreter row: an array's element type lives in an
interpreter side table, while a hash embeds it in `HashData`. The by-name array arm of `methods_call_dispatch.rs` and `Value::array_shallow_clone`
(whose copy lost the `shape`, the holes and the container de-aliasing the cascade keeps) are deleted: `@a.clone` has one implementation.
No behaviour change; pinned in `t/oo/method/collection-clone-method-rows.t` against `raku`.

- **Left on #12389**: `Junction`/`Nil` shapes, `Map.gist`, `Hash.join`, `Any.batch`/`hash`/`list`/`serial`, `List.chrs`, `Map.AT-KEY`/`Str`.
  The remaining `clone` arms belong to other owners (`Set`/`Bag`/`Mix`, `Seq`/`Slip`, scalars, `Match`, the mixin path, `Mu`).

### 9.63 `Any.join` (2026-10-10)

`refactor/11276-join-rows` (issue #12389, item 4). Rakudo declares `join` on `Any`, `List` and `Seq` only (the oracle snapshot): `List` and `Seq` had
rows, so the one row owed is `Any.join` at arities 0 and 1 (`Handler::Narrow`, `method_table::collections::join`). `Hash.join` needs no row of
its own: `Hash` and `Map` inherit `Any`'s. `join_core` is the one body, shared by the row and the zero- and one-argument cascade arms (which had
two diverging copies): a hash reads as its `key\tvalue` pairs, a `Pair` as a one-element list, a `Capture` and a `Match` as their positionals, a
`Range` expanded, a `Uni` as its codepoints, the scalars and the four temporal classes (`Date`, `DateTime`, `Instant`, `Duration`, never a subclass)
as their own `.Str`. It answers `Joined::{Done, NeedsInterpreter, NotCovered}`: the row declines the last two, and the arms keep their old tails
for an instance, a `Nil`, a `Thread` and a lazy list.

- **Behaviour toward Rakudo**: `\(1, 2).join` is `"12"` (the zero-argument arm used to answer the Capture's rendering, `"1 2"`); a consumed `Seq`
  now dies the same way in the one-argument form. Pinned in `t/oo/method/any-join-method-rows.t`.
- **Left**: `Set`/`Bag`/`Mix` (closed shapes, and no recognition row: `join` dies with `X::Method::NotFound`), `Nil.join` (answers `Nil`, Rakudo `""`),
  both filed as issues.

### 9.64 `Any.serial`, `Any.batch` and `List.chrs` (2026-10-10)

`refactor/11276-any-coerce-rows` (issue #12389, item 4). Three rows in `method_table::collections::any_misc`, each the one body the cascade's arm
calls: `Any.serial` (`Pure`; a value is its own serial form, de-itemized), `Any.batch` at arity 1 (`Handler::Narrow`: it declines an argument that is
neither an `Int` nor the `:elems`/`:batch` pair, which the cascade then reports; an `Array` batches lazily through `ListGen::batch`, a `Blob`/`Buf` by
its bytes) and `List.chrs` (`Pure`). The three cascade arms (`dispatch_core_coerce.rs`, `dispatch_1arg.rs`, `dispatch_core_unicode.rs`) are
one call each. No behaviour change, pinned in `t/oo/method/any-misc-method-rows.t` against `raku`.

- **Not registered**: `Any.chrs`/`Cool.chrs` (Rakudo declares them, but a row on `Any` has to answer for every shape that reaches it, and the cascade's
  `chrs` reads only lists, ranges and one integer), `Any.list` and `Any.hash` (their arms are spread over `coercion.rs`, `collection.rs`, the
  `Capture`/`Uni`/`Supply` bridges and the stash cases: a slice of their own), `Map.AT-KEY`/`Map.Str`.
- **`RowFlags::LIVE_VIEW`** (new). `Array.batch` answers a lazy `Seq` over a cursor on the live array (a later `push` shows in the batches,
  `t/collections/lazy-seq/lazy-positional-views-and-combinatorics.t`). The debug cross-check (`dispatch::debug_assert_matches_full_path`) and the
  call-site lane's check render both answers with `gist_value`, which reifies the view and froze it at the first run: the test failed on the debug
  binary only. The flag joins `RANDOM` in `RowFlags::skips_cross_check`. **Lesson:** a row whose answer is a lazy view of its receiver needs the
  flag, or the net that guards the row ends the laziness it keeps.

### 9.65 `Any.list`, `Any.hash`; `Map.AT-KEY`/`Map.Str` need no row (2026-10-10)

`refactor/11276-map-key-str-rows` (issue #12389, item 4). Two `Handler::Narrow` rows in `method_table::collections::any_list_hash`. Every owner
below `Any` with a shape of its own already had `list` and `hash` rows (`List`, `Range`, `Seq`, `Map`, the quant hashes, `Capture`, `Uni`, `Blob`,
`Backtrace`, `Supply`), so these answer the rest. `Any.list` answers a scalar or one of the four temporal classes with the one-element `List`; the
cascade's fall-through arm of `list`/`Array` calls the same `list_of`. `Any.hash` answers through `hash_of` (an `Array`, `Seq` or `Slip` is read even
when itemized; anything else through `value_to_list`, so a scalar is an odd initializer) for every shape that reaches it, and declines what the
cascade routes elsewhere: a type object, `Nil`, an instance (a user class may have a `hash` accessor, a stash is a Map of its symbols), a hash, a quant
hash and a lazy list. No behaviour change, pinned in `t/oo/method/any-list-hash-method-rows.t` against `raku`.

- **`Map.AT-KEY` and `Map.Str` are not registered, and need not be.** `Hash` and `Map` share one dispatch shape, so a row on `Map` is unreachable
  (`every_row_is_reached_and_answers` says so) and `Hash` already has `AT-KEY` (`subscript.rs`) and `Str` (`render_names.rs`). The recognition table's
  `Map` rows for them are answered by `Hash`'s.
- **Found, not fixed**: `(set <a>).hash.raku` is `{:a(Bool::True)}` in mutsu and `(my Bool %{Mu} = :a(Bool::True))` in Rakudo.

### 9.66 `Capture.WHICH`, `List.Supply`; no shape for `Junction`, `Nil` and `Map` (2026-10-11)

`fix/12607-quanthash-nil-join` (issue #12389, items 2-3). Two rows and three decisions.

- **`Capture.WHICH`** is a row of `Capture` (`collections::render`, the `WHICH` family) and `which_of` gains a `Capture` arm. A Capture is a value type:
  `\(1, 2, :a(3)).WHICH` is the `ValueObjAt` `Capture|(Int|1)(Int|2)|a(Int|3)` (positionals as `(WHICH)`, then the named arguments sorted by key as
  `key(WHICH)`, the two blocks joined by `|`; an empty Capture is `Capture`). The cascade answered an `ObjAt` with a counter (`Capture|1`), so two equal
  Captures were not `===`. **Behaviour change toward Rakudo**, pinned in `t/concurrency/supply/capture-which-supply-method-rows.t`.
- **`List.Supply`** is a `Handler::Narrow` row (`any_misc::supply`) sharing `supply_of` with the cascade's `Supply` arm. It mints a new `Supply`
  instance per call, so the row carries `RowFlags::RANDOM`, whose doc now reads "the answer differs between two runs by design" (random, or a new
  object with an identity of its own): the debug cross-check cannot compare two runs.
- **Decision (item 3): `Map` stays the `Hash` shape.** An immutable `Map` is a `Hash` value with `declared_type == "Map"`. `Hash`'s rows answer `Map`'s
  `gist`, `raku`, `Str`, `AT-KEY` and `clone` (§9.36, §9.38, §9.62, §9.65), and a row on `Map` is unreachable (`every_row_is_reached_and_answers` forbids
  it). A `Map` shape would be closed until every `Hash` row is audited for it and would gain nothing. The recognition table's unregistered `Map` rows are
  therefore served, not owed; `method-rows-report.py --inventory` still lists them.
- **Decision (item 2): `Junction` and `Nil` stay receivers with no shape**, named here as ADR §10.3 rule 6 allows. A `Junction` does not dispatch by its
  own shape: the VM autothreads a method over the eigenstates before any row or cascade runs (`vm_call_autothread.rs`), and `Bool`/`gist`/`raku` are the
  few methods that apply to the junction as a whole, which the interpreter answers. `Nil` absorbs every method call (`Nil.foo` is `Nil` with a warning:
  `nil_predispatch_verdict`), which no row can express. Their cascade arms (`Junction`: `Bool`, `gist`, `raku`; `Nil`: `gist`, `raku`) stay until
  slice 5 decides what replaces the cascades for shapeless receivers.
- **Left (filed as #12622)**: `Array`/`Hash` `name` and `dynamic` (read the variable's metadata, not the value), `List.rotate`/`Array.rotate` (a `Mut` row), `Any.splice`
  (the `Handler::Mut` question of §9.60) and `List.AT-POS`/`Array.AT-POS` (`positional.rs` explains why they have no row).


### 9.67 `rotate`, `Any.splice`, `name`/`dynamic` and `AT-POS` (2026-10-11)

`refactor/12622-collection-rows` (issue #12622). The four items #12622 left, and none needed a new place kind.

- **`List.rotate` / `Array.rotate` are `Handler::Narrow` rows, not `Mut` ones.** `rotate` answers a new `Seq` and leaves the receiver alone (the
  recognition table's `Mut` kind came from the arm's position, not from a write). `rotate_value` (`collections::rotate`) is the one implementation;
  `dispatch_rotate` calls it. A second count now raises Rakudo's `Too many positionals passed; expected 1 or 2 arguments but got N`, which the
  cascade used to ignore.
- **`Any.splice` is a `Handler::Mut` row that is the end of every owner chain.** Rakudo declares `splice` on `Any` as a proto with no candidates,
  so every receiver that is neither an `Array` nor a `Buf` (an `Int`, a `Hash`, a `Seq`, a type object) gets `X::Multi::NoMatch`
  ("Cannot resolve caller splice(Int:D, Int:D, Int:D); ..."). `invoke_mut` appends `Any` to the owner chain of every receiver (and gives a
  receiver `owners_of` has no kind for the chain `[Any]`); the by-value entry probes `splice` regardless of kind. The handler declines a
  receiver that other entries resolve further and leaves it to them: a cell or variable reference around the value, any `Instance` (an `is Array` /
  `is Hash` / `is Bag` subclass reaches its native storage's row through the `builtins_dispatch_next` bridges, which this row must not pre-empt; a
  user class may declare `splice`) and a `Mixin`; a type object of a user class that declares `splice` also answers itself. Behaviour change toward
  Rakudo: `Hash`/`Seq`/`Set`/`Pair` raised `X::Method::NotFound`, `Nil.splice` answered `Nil`; the `Int`-family message and the type-object names were
  also off. A plain user-class instance and a `Blob` keep their old answers (`X::Method::NotFound`, `X::AdHoc`) until slice 5 resolves the instance
  bridges in the resolver. `make_no_candidates_error` names a type object or an instance
  by its user-facing name. The cascade's two `splice` branches (a `Range`, a definite scalar) are gone.
- **`name` / `dynamic` need no binding name.** Rakudo keeps the descriptor on the `Array`/`Hash` object, so `my $r = @a; $r.name` is `@a` and a
  container no declaration named is `element`; mutsu stamps the same name on `ArrayData`/`HashData::descriptor_name`. The rows (`collections::descriptor`)
  read the value, `dynamic` is `False` on a value (only a dynamic *variable* is dynamic). `@a.name` / `@a.dynamic` written on an `@`/`%` variable are
  rewritten by the compiler to `.VAR.name` / `.VAR.dynamic` (`compile_expr_method_on_var`), which consults variable metadata (`is_var_dynamic`, `our`/`state`
  names): a compile-time decision about the call site, not a dispatch fallback, so it stays. The decision the issue asked for is therefore
  "neither a `Mut` row nor a new place kind: a `Narrow` row over the value".
- **`List.AT-POS` / `Array.AT-POS` are `Handler::Interp` rows** over `Interpreter::builtin_at_pos` (the subscript opcode), the interpreter row
  `positional.rs` said they would be, now that slice 4 (#12387) is done.
- **Found, not fixed**: `my @e; my @a = 1; @e = @a; @e.name` is `@a` in mutsu and `@e` in Rakudo (an assignment into an empty container replaces the node
  and its descriptor).

### 9.68 `Any.splice` answers an `Instance` (2026-10-11)

`refactor/12647-instance-backing-in-resolver` (issue #12647). The first step of slice 5: the `Any.splice` row stops declining every `Instance`, because the
question that justified the decline ("does a bridge resolve this receiver further?") now has one answer in one place.

- **`InstanceBacking`** (`src/runtime/instance_backing.rs`) names the reserved attribute of each native storage an instance delegates to
  (`__mutsu_array_storage`, `__mutsu_hash_storage`, `__baggy_data__`). The three storage bridges (`native_{array,hash,baggy}_storage_base`) and the `Any.splice`
  row ask it instead of each spelling the attribute.
- **What the row declines is now exactly what has a row of its own further on:** a cell/variable reference or a `Mixin` (unwrapped by their own paths), an
  `is Array` instance (its storage is an `Array`, whose `splice` row the bridge reaches), an instance whose class declares `splice`, and a user class rooted at
  `Mu` (`class C is Mu` has no `splice`; `is_user_class_without_any`). Everything else is answered: a plain user-class instance, a `Blob`, an `is Hash` /
  `is BagHash` instance (their storage has no `splice`), a `Date`, a `Match`.
- **`FALLBACK` is not consulted.** The earlier row text said a user class "may declare `splice` or `FALLBACK`"; Rakudo resolves `splice` to `Any`'s proto first, so
  `FALLBACK` never sees it. Only a declared `splice` stops the row.
- Behaviour change toward Rakudo: those receivers raised `X::Method::NotFound`, now `X::Multi::NoMatch`. `t/types/any-splice-instance-receivers.t` pins it.
- **Found, not fixed**: `Exception`/`X::AdHoc` instances and `utf8` still answer `X::Method::NotFound` for `.splice` (an earlier path intercepts them before
  `invoke_mut`); a `Mixin` and a cell reference still decline.

### 9.69 `Any.splice` sees through a cell, a variable reference and a mixin (2026-10-11)

`refactor/12653-unwrap-mixin-ref-receivers` (issue #12653). The remainder of #12647: the row no longer carries a `ContainerRef | VarRef | Mixin` decline list.

- **The row unwraps the receiver once** (`unwrap_varref_value_for_dispatch`: the cell and the transient variable reference of an `is rw` argument). A `Mixin` stays a mixin
  for the answer (`Int+{Tag}`, which is how Rakudo names it in the `X::Multi::NoMatch` message; `make_no_candidates_error` now names a mixin by its user-facing type),
  but what it wraps decides whether the call is this row's.
- **The decline is now only "someone else has the real `splice`"**: an `Array` and a `Buf` (the owner lookup does not see through a wrapper, so a cell around an
  `Array` reaches `Any` and must be declined here), an `is Array` instance, a user `splice`, a `Mu`-rooted user class. A wrapper alone is never a reason to decline.
- Behaviour change toward Rakudo: the mixin's type name in the message. The instance and cell cases already matched.
- **Found, not fixed**: Rakudo answers `(my @a = 1,2,3; @a but Tag).splice(1,1)` with `[1, 3, Mu]` (a length quirk of its mixin copy); mutsu answers `[1, 3]`. Not pinned by a test.

### 9.199 Slice 5: native integer coercer entry (2026-11-01)

`refactor/11276-native-int-coercer-entry` removes the entire native integer
coercer branch from `native_method_0arg_cascade`. All eleven coercion names
already have `Cool` and `Int` rows in `scalars::real_misc` (22 rows). The
cascade had recognized the names separately and called the primitive directly:
narrow-width names were absent from the audited `Cool` numeric names of closed
collection shapes, so those receivers could not reach the existing rows.

The initial `scripts/method-rows-report.py --arms` inventory was 4 pure-cascade
arms, 216 runtime arms and 3 VM mutation-helper arms (223 total). These counts
do not measure this branch, which used a predicate rather than quoted-name arms.

Admission now reads `native_types::NATIVE_INT_COERCE_METHODS`: closed shapes
reach the existing `Cool` rows, and the shapeless resolver admits only `Cool`
receivers. This preserves the exclusion of `Pair` and ordinary user instances.
The rows accept type objects too: `Cool` coerces to zero, while the shared
primitive now refuses the undefined `Int` with Rakudo's concrete-integer error.
The latter is a behavior correction toward Rakudo, pinned alongside the
collection, IO path, native-array and shaped-array cases in
`t/nativecall/native-int-coercer-entry-rows.t`. A Rust test additionally pins
that the closed Range and IO path receivers are answered by rows themselves,
before any cascade can run. No other cascade guard is removed in this slice.

### 9.200 Slice 5 inventory: the pure Code entry (2026-11-01)

`refactor/11276-code-pure-row-lookup` audits the surviving pure Code entry.
The zero-argument cascade gates on `Capture`/`clone`, and `code::pure_answer`
matches those names again and calls the handlers directly. The registry already
has the three relevant rows: `Code.Capture`, `Code.clone` and `Block.clone`.
The owner chain is already centralized by ADR-14477's `code_class::owners_of`.
`Mu.clone` is the third caller of the pure entry, for a `Sub` receiver.

The deletion unit is both method-name gates and their direct handler routing.
The replacement looks up the nearest registered owner row and uses the existing
pure invocation/guard machinery. An interpreter-only row must decline without
falling through to a pure ancestor. No per-class shape decision changes here.
The initial arm inventory remains 223 total arms, 119 naming built-in methods;
the method-name gates here are not the report's quoted-name cascade arms.

The pure owner entry resolves the nearest zero-argument row before invoking it
through `admits`/`invoke`; a nearest interpreter row therefore declines without
running an ancestor. Code's pure entry and `Mu.clone` use that lookup. Instance
code-class classification is memoized by its immutable class symbol, including
negative results and mixin base names, so expanding the receiver-based entry
does not copy or re-scan class names on every native method call. This is a
classification memo, not a cache of mutable method resolution.
