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
