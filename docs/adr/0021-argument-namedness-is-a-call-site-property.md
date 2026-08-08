# ADR-0021: Argument named-ness is a call-site property — Pair flavour unification

- **Status**: Proposed
- **Date**: 2026-08-08
- **Related**:
  [todo/deep/pair-namedness-is-a-value-property-not-a-call-site-property.md](../../todo/deep/pair-namedness-is-a-value-property-not-a-call-site-property.md),
  [news/2026-08/hash-derived-pairs-are-positional-arguments.md](../../news/2026-08/hash-derived-pairs-are-positional-arguments.md),
  [news/2026-08/hash-list-coercion-yields-pairs.md](../../news/2026-08/hash-list-coercion-yields-pairs.md)

## Context

### The Raku model

In Raku, whether an argument is *named* is decided by the **syntax of the call
site**, never by the value. Only these forms produce a named argument:

- a bareword fat-arrow written directly in the argument list: `f(a => 1)`;
- a colonpair written directly in the argument list: `f(:a(1))`, `f(:$a)`,
  `f(:flag)`, `f(:!flag)`;
- a flattening `|` of a `Pair` or a `Hash`/`Map`: `f(|$pair)`, `f(|%h)`, or of
  a `Capture`'s named lane: `f(|c)`.

Everything else — `Pair.new(...)`, a `Pair` held in a variable, an
array/list element, a sub's return value, `%h.pairs[0]`, a quoted or computed
key (`f('a' => 1)`, `f($k => 1)`), a parenthesised pair (`f((a => 1))`) — is an
ordinary positional argument of type `Pair`. Slipping an `Array`/`List` with
`|@l` yields *positional* arguments even when the elements are `Pair`s.

### The mutsu model: named-ness encoded in the value

mutsu instead carries named-ness **in the value representation**:

- `ValueRepr::Pair(String, Box<Value>)` (`src/value/mod.rs:1345`) — the
  "named-argument flavour"; binding and dispatch treat it as a named argument.
- `ValueRepr::ValuePair(Box<Value>, Box<Value>)` (`src/value/mod.rs:1346`) —
  the "positional flavour"; binding treats it as a plain positional `Pair`.
  (Its doc comment still says "non-string key", which is stale: since the
  hash-derived slice, `HashData::typed_pair` mints it for `Str` keys too.)

`Value::is_string_pair_value` (`src/value/view.rs:598`, a pure NaN-box tag
probe) is the discriminator; 31 consumer sites use it to split an argument
`Vec<Value>` into positionals and nameds (canonical comment:
`src/runtime/types/args_matching.rs:99`). A 2026-08 audit confirmed the two
flavours are **observably identical everywhere else**: `.WHAT`, `~~ Pair`,
`===`/`.WHICH` (`src/runtime/utils/errors.rs:324-327` builds the same WHICH
string by design), `eqv`, `cmp`, `.Str`, `.gist`, `.raku`, hash construction,
`.antipair`, destructuring. Argument classification is the *only* observable
difference. That is the central fact this ADR builds on: the flavour bit is
not a data property at all — it is a call-site marker that happens to live in
the value.

### Measured divergence (2026-08-08, mutsu vs raku)

Probe: `multi method m(Pair $p)` / `multi method m(:$a!)` on a class, and the
sub/slurpy equivalents. ✓ = matches raku.

| Argument shape | raku | mutsu function call | mutsu method call |
|---|---|---|---|
| `f(a => 1)` / `f(:a(1))` literal | named | named ✓ | named ✓ |
| `f('a' => 1)` / `f($k => 1)` computed key | positional | ✓ | ✓ |
| `f((a => 1))` parenthesised | positional | ✓ | ✓ |
| `Pair.new('a', 1)` as arg | positional | ✓ | **named ✗** |
| variable holding `a => 1` / `:a(1)` | positional | ✓ | **named ✗** |
| array element `@l[0]` | positional | ✓ | **named ✗** |
| sub return value `mk()` | positional | ✓ | **named ✗** |
| `f(|$p)` slip a Pair | named | ✓ | ✓ |
| `f(|%h)` slip a Hash | named | ✓ | ✓ |
| `f(|@l)` slip an Array of Pairs | positional | **named ✗** | — |
| forward `outer($p)` → `inner(|c)` | positional | **named ✗** | — |
| `callsame` with a Pair positional | positional | — | **named ✗** |
| `D.new($p)` default ctor | dies (positional) | — | **binds as named ✗** |
| `$bag.pairs.map(&show)` runtime-invoked | binds `$p` | **"Too few positionals" ✗** | — |

The method column is broken for every non-syntactic Pair; the function column
is correct except slip/forwarding. This asymmetry has a precise mechanical
cause (next section). The practical impact: any library that iterates pairs
and hands them to a typed callee misbinds — the case that surfaced it is
`Cro::HTTP::Client`, where `headers => [Authorization => '...']` dies with
`X::Multi::NoMatch` on `append-header` while raku returns 200.

### Why the function path is (mostly) right: boundary erasure already exists

`compile_call_arg_with_escape` (`src/compiler/helpers_call_args.rs:256-258`)
appends **`OpCode::ContainerizePair`** to every function-call argument that is
not syntactically named (`is_named_arg_expr`, `:199-206`: a fat-arrow, a
literal `Pair` constant, or a `|` slip). `ContainerizePair`
(`src/vm/vm_exec_dispatch.rs:2207-2215`) rewrites a named-flavour `Pair` into
a `ValuePair` and is a fast no-op for everything else (`PAIR_PATTERN` tag
check; Tier-B JIT no-op). So on the function path, **named-ness is already
erased at the call boundary** unless the call site wrote it syntactically —
which is exactly the Raku model, implemented as value normalization.

The method path has no such normalization: `compile_method_arg_with_escape`
(`src/compiler/helpers_call_args.rs:156-179`) never emits `ContainerizePair`,
so whatever flavour the value happens to carry leaks into
`vm_method_dispatch.rs`/`bind_function_args_values` and is classified there by
`is_string_pair_value`. One compiler line is the difference between the two
columns above.

Two more holes are independent of the method gap:

1. **Slip expansion** (`append_slip_item`, `src/vm/vm_call_helpers.rs:5-38`)
   promotes a `ValuePair` with a `Str` key back to the named flavour and
   passes `Array` elements through unnormalized, so `f(|@l)` turns Pair
   elements into named args. Capture construction (`exec_make_capture_op`,
   `src/vm/vm_data_ops.rs:278-330`, and `|c` parameter binding) classifies by
   value flavour too, so forwarding `outer($p)` → `inner(|c)` manufactures a
   named argument out of a positional one.
2. **Runtime-invoked calls** (a builtin handing values to user code: `map`,
   `grep`, `sort`, supply taps, …) have no compiled call site at all, so
   boundary erasure never runs. Any *minting* site that produces the named
   flavour for plain data therefore misbinds: `$bag.pairs.map(&show)` dies
   with "Too few positionals" today because `quanthash_typed_pair` mints named
   pairs. This is why fixing the compiler alone is not enough — the minting
   default must flip as well.

### Minting and consumer inventory (2026-08-08 audit)

Constructor chokepoints: `Value::pair` (151 call sites; named flavour) and
`Value::value_pair` (92 sites; positional flavour) — nothing else builds the
reprs. The named-flavour minting sites fall into exactly two groups:

- **Argument-position synthesis** (correct to stay named): statement-call
  named args (`src/compiler/stmt.rs:2566-2580`,
  `src/compiler/helpers_control_flow.rs:750-766`), `CallFuncNamed` fallback
  materialization (`src/vm/vm_call_func_ops.rs:190-201`), slip promotion of a
  Pair/Hash, regex/subst adverbs, `MAIN` CLI args, and ~30 internal
  runtime sites that build a named argument on purpose.
- **Data-value minting** (wrong; each is a latent misbind through any of the
  holes above): fat-arrow `MakePair` outside argument lists
  (`src/vm/vm_mixin_does_ops.rs:483-512`), `Pair.new`
  (`methods_object_native_ctors_buf_num.rs:298`), `pair()`
  (`builtins_collection.rs:353`), `quanthash_typed_pair`
  (`src/runtime/utils/quanthash_keys.rs:47-52`) + its two inline copies
  (`methods_dispatch_match3.rs:499,598`) + `setty_typed_pair`
  (`methods_collection_ops/sort.rs:676`), hash iteration's live-pair path
  (`src/vm/vm_control_ops.rs:83`), hash flattening in `map`/`grep`
  (`builtins_collection_mapgrep.rs:66`) and in `set`/`bag`/`mix`
  (`builtins_collection.rs:161,218,274`), a fourth QuantHash→pairs path
  (`coerce_containers.rs:555-571`), `Capture.pairs` named lane
  (`methods_0arg/mod.rs:805`), `Match.pairs` (`:1901,1908`), enum
  `.pair`/`.pairs` (`methods_enum_dispatch.rs:51-54,115-120`), `IO::Path.flat`
  (`:1766`), `Z=>` (`vm_meta_ops.rs:125-129`), `[=>]` reduction
  (`ops_reduction.rs:673-676`), `.antipair`/`.Pair` coercions
  (`coercion.rs:229`, `dispatch_core_coerce.rs:265`), hyper Pair-leaf rebuild
  (`vm_hyper_ops.rs:313-326`).

The family is also internally split: Hash-derived pairs are positional (the
2026-08 slice) but QuantHash/enum `.pairs` are named while their `.antipairs`
/ `.invert` are positional.

Of the 31 `is_string_pair_value` consumers: 15 split call args for
dispatch/binding, 7 gate object construction, 9 classify positional-vs-adverb
in builtins. **None of them changes** under this ADR — they keep reading the
flavour; the ADR changes *who is allowed to mint it*.

### Why flipping `quanthash_typed_pair` aborted Cro before

The earlier attempt to flip Set/Bag/Mix pairs broke
`http-middleware.rakutest` mid-file. The audit found the concrete reason:
several QuantHash *consumers* have `ValueView::Pair`-only arms with no
`ValuePair` sibling, so a flipped pair changes meaning, not just flavour:

- `coerce_value_to_quanthash` list and scalar branches
  (`src/runtime/utils/set_coerce.rs:170-192, 194-199`) — a `ValuePair` falls
  to the `_` arm and inserts the **whole pair** as a set element instead of
  inserting the key with a weight;
- `coerce_to_bag` / `coerce_to_mix` scalar branches
  (`src/vm/vm_set_arith_ops.rs:223-229, 282-288`) — weight collapses to 1;
- `addition_bag_weights` / `addition_mix_weights` scalar arms
  (`src/runtime/ops_set.rs:575, 656`);
- `unique`/`squish` adverb readers
  (`methods_collection_ops/unique_squish.rs:12, 105, 197`).

These are ordinary two-arm gaps, fixable independently — the flip was blocked
on missing prep work, not on a semantic conflict.

## Decision

Adopt the Raku model with the **in-band marker mechanism the function path
already uses**, generalized and made law. The named flavour stops being a
property of Pair data and becomes a transient *named-argument marker* that
only call sites may create.

### Invariants (the end state)

- **I1 — Marker, not data.** A named-flavour `Pair` exists only between a
  call site and the callee's binder. No value-producing operation (literal
  outside an argument list, constructor, `.pairs`, iteration, coercion, meta
  op) may return one.
- **I2 — Minting default is positional.** Every data-minting site produces
  the positional flavour, for `Str` keys included. The named flavour is
  minted only by (a) compiled argument positions whose syntax is a bareword
  fat-arrow or colonpair, and (b) runtime argument synthesis that *intends* a
  named argument (internal builtin→builtin calls, slip promotion, adverb
  forwarding).
- **I3 — Boundary erasure everywhere.** Every compiled call site normalizes
  every non-syntactically-named argument with `ContainerizePair` — method
  calls exactly like function calls. (After I2 this is belt-and-suspenders;
  it stays until I2 is complete and measured, then becomes a candidate for
  removal on the hot path.)
- **I4 — Slips classify by container type.** `|$pair` and `|%h` produce named
  arguments; `|@l` / `|$list` produce positional arguments (elements
  containerized); `|c` replays the Capture's two lanes verbatim. Element
  flavour never decides.
- **I5 — Captures are two-lane.** Capture construction (`\(...)`, `|c`
  params, `callsame`/`nextsame` forwarding) fills its positional/named lanes
  from call-site classification, and forwarding replays the lanes without
  reclassification.

The 31 flavour consumers keep working unchanged: they still read
`is_string_pair_value`, which now answers "was this argument named at the
call site" — because nothing else can mint the flavour anymore.

### Rejected alternatives

1. **Two-lane argument plumbing now** (pass `(Vec<Value>, Vec<(Symbol,
   Value)>)` through every call op, dispatch fn, and binder — the "honest"
   direction 1 of the todo ticket). Rejected *for now*: it touches all 31
   consumers, every dispatch/binding signature, the light-call caches, the
   JIT call helpers, and `eval_block_value` re-entry, for zero additional
   observable semantics over I1–I5 — once minting is restricted to call
   sites, the in-band marker *is* call-site information. The
   `CallFuncNamed`/`NamedArgsSpec` out-of-band mechanism
   (`src/opcode.rs:6112`) remains the beachhead if we later want the
   structural version for performance; nothing in this ADR forecloses it.
2. **Value-default inversion without boundary erasure.** Flipping minting
   sites alone leaves the method path trusting whatever flavour arrives;
   any missed minting site (there are 151 `Value::pair` calls to audit)
   becomes a silent misbind. Erasure at the boundary makes a missed site
   cost nothing on compiled paths — a sound mechanism over a complete-audit
   assumption, per the project's gain/risk rules.
3. **Heuristic classification in the binder** (treat a Pair as named only if
   its key matches a declared named parameter). Rejected outright: raku
   semantics say `sc($p)` *dies* with "Too many positionals" even when the
   key matches `:$a` — the heuristic is observably wrong, and it would make
   binding outcomes depend on the callee's signature shape in a way that can
   go flaky.

### Phasing

Each phase is independently shippable and lands with its own tests; CI's
`make test` + `make roast` plus the vendored Cro suites gate every step.

**P1 — Method-call boundary parity** (the Cro fix)

- Emit `ContainerizePair` for non-syntactically-named method arguments in
  `compile_method_arg_with_escape` (`src/compiler/helpers_call_args.rs:156`),
  sharing `is_named_arg_expr` with the function path. This covers every
  `CallMethod` / `CallMethodMut` / `CallMethodDynamic*` / hyper-method /
  `TempMethodAssign` emission site (`src/compiler/expr_method.rs:174, 183,
  247, 270, 554, 586, 592`) because they all funnel through it.
- Audit the non-funnelled arg compiles (`Expr::CallOn`, parent/trait arg
  chunks) for the same treatment.
- Acceptance: the method column of the divergence table matches raku;
  `D.new($p)` dies like raku; `Cro::HTTP::Client` accepts
  `headers => [Authorization => '...']` (the `tmp/hdr2.p6` repro from the
  todo ticket, plus the vendored `http-client`/`http-middleware` suites).
- Risk: `t/` tests that (wrongly) rely on a variable Pair binding as a named
  method argument — fix the tests to raku behavior, per the "roast is
  authoritative" rule. Internal runtime callers are unaffected (they bypass
  the compiler and keep synthesizing named-flavour args deliberately).

**P2 — Slip, Capture, and forwarding rules (I4/I5)**

- `append_slip_item` (`src/vm/vm_call_helpers.rs:5-38`): keep Pair→named and
  Hash→named promotion; add List/Array/Seq element **containerization** (the
  current pass-through leaks element flavour); keep the Capture arm
  lane-faithful (positional lane must stay positional — containerize on
  append rather than trusting stored flavour).
- Capture construction: `exec_make_capture_op`
  (`src/vm/vm_data_ops.rs:278-330`) and the `|c` parameter binding path must
  fill lanes by the same classification (named lane ⇐ named-flavour marker
  only, which after P1 means call-site-named only); `Capture.pairs`' named
  lane output stays a *data* pair (moves to P3).
- `callsame`/`nextsame`/`samewith` forwarding: verify the saved argument
  vector preserves markers unchanged (forwarding is a call boundary that
  must not reclassify).
- Acceptance: `f(|@l)` positional; `outer($p)`→`inner(|c)` positional;
  `B::m` → `callsame` reaches the `Pair` candidate; existing pins
  (`.subst(|(:g), ...)` adverb promotion) stay green.

**P3 — Minting-default inversion (I2), including QuantHash**

- Prep (P3a): add the missing `ValuePair` arms listed in Context §"Why
  flipping quanthash_typed_pair aborted Cro" (set_coerce, vm_set_arith_ops,
  ops_set, unique_squish). This is safe standalone — it only widens
  pattern matches.
- Flip the data-minting sites enumerated in the Context inventory to
  `Value::value_pair`, including `quanthash_typed_pair` (+ its 3 clones),
  `hash_live_pairs`' `Str` arm, enum `.pair`/`.pairs`, `Capture.pairs` /
  `Match.pairs`, `Z=>`, `[=>]`, `pair()`, `Pair.new`, `.antipair`, `.Pair`,
  the map/grep and set/bag/mix hash-flattens, and the
  `coerce_containers.rs:555` path.
- Split the fat-arrow opcode: `MakePair` becomes positional-minting; a new
  `MakeNamedArg` (same payload; or a bool packed into `MakePair`) is emitted
  only by argument-position synthesis (`src/compiler/stmt.rs:2566-2580`,
  `helpers_control_flow.rs:750-766`, `expr.rs:301-312`), preserving the
  in-band named marker for `ExecCallPairs`/capture sites. Constant-folded
  `:flag` in argument lists keeps the named constant; a `:flag` at
  expression level mints positional.
- Acceptance: `$bag.pairs.map(&show)` binds; the runtime-invoked probe
  matrix matches raku; `t/setbagmix-gist-pair-elements.t`,
  `t/for-pairs-value-quanthash-writeback.t` and the QuantHash roast files
  stay green; full Cro suite run (the previous abort must not reproduce).

**P4 — Representation cleanup and documentation**

- Fix the stale doc comments (`ValueRepr::ValuePair` "non-string key",
  `value_collections.rs:104-107`) and rename the concepts in code docs:
  `ValuePair` is *the* Pair representation; `Pair(String, _)` is the
  named-argument marker. (An actual repr rename — e.g. `NamedArg` — is
  optional and mechanical; decide by churn at the time.)
- Consolidate the duplicated flavour-pair arms that exist only because both
  flavours occur as data today (`.raku`/`.gist`/`eqv`/`cmp` cross-arms stay;
  data-path Pair arms that can no longer be reached get simplified as they
  are touched, not in a sweep).

**P5 — Performance follow-up (optional, measured)**

- With I2 complete, per-argument `ContainerizePair` on the *function* path
  guards against nothing on most sites; measure (bench CI, not local) and
  drop it where the compiler can prove the argument expression cannot yield
  a named marker (it already skips syntactic named args; a static "cannot be
  Pair at all" filter would remove the opcode from int/str-typed hot loops —
  though the op is a tag-check no-op, fetch/decode slots are not free).
- If method-call named-arg sites show up in profiles, extend the
  `CallFuncNamed`/`NamedArgsSpec` out-of-band mechanism to a
  `CallMethodNamed` (one extra `u32` side-table index, respecting the
  48-byte `OpCode` budget pinned by `opcode_size_guard`). This is a pure
  optimization; semantics are fixed by P1–P3.

### Test plan

- New pins, one file per phase: `t/method-pair-argument-is-positional.t`
  (the full divergence matrix on methods, incl. `D.new($p)` dying),
  `t/slip-array-of-pairs-is-positional.t`, `t/capture-forwarding-preserves-
  pair-positionality.t`, `t/quanthash-pairs-are-positional-arguments.t`,
  `t/runtime-invoked-call-pair-positional.t` (`$bag.pairs.map(&show)`).
- Existing pins that must stay green: `t/pair-positional-arg.t`,
  `t/multi-dispatch-positional-pair.t`, `t/hash-pair-is-positional-argument.t`,
  `t/hash-first-positional.t`, `t/nested-pair-subsignature.t`,
  `t/pair-subsignature-dispatch.t`, `t/colon-const-fatarrow-key.t`,
  `t/setbagmix-gist-pair-elements.t`, `t/for-pairs-value-quanthash-writeback.t`.
- The vendored Cro suites are the integration gate for P1 and P3 (the two
  slices that previously interacted with them), run per the memory's note:
  never two Cro suites concurrently (fixed ports).
- Full roast is delegated to CI per repo policy.

## Consequences

- The observable Raku rule "named-ness is call-site syntax" holds on every
  path — compiled function calls, method calls, slips, captures, forwarding,
  and runtime-invoked callbacks — with one mechanism (mint-at-call-site +
  boundary erasure) instead of per-path classification.
- The flavour bit degrades from a semantic landmine into an implementation
  detail of argument passing; a future two-lane argument refactor becomes a
  performance decision, not a correctness campaign.
- `D.new($pair)` and `sc($pair)` start *dying* where they silently
  mis-bound before — matching raku, but any downstream code that leaned on
  the mutsu bug will surface in CI (that is the safety net working).
- Cost: one extra tag-check opcode per method argument until P5 measures
  whether it can be narrowed; the QuantHash prep widens a handful of match
  arms.

## Implementation status

- [x] P1 method-call boundary parity
- [ ] P2 slip/capture/forwarding rules
- [ ] P3a QuantHash consumer prep
- [ ] P3 minting-default inversion
- [ ] P4 representation cleanup
- [ ] P5 measured perf follow-up
