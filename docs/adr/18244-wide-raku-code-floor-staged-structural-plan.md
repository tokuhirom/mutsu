# ADR-18244: Wide Raku code is bounded by the per-op and per-call floor — a staged structural plan

- **Status**: Accepted (maintainer decision 2026-11-21; proposed 2026-11-19)
- **Issue**: [#18244](https://github.com/tokuhirom/mutsu/issues/18244). It carries the structural
  half of [#16897](https://github.com/tokuhirom/mutsu/issues/16897), which keeps its own close
  condition: MCP::Server::Tool::Web `t/12-extract.rakutest` `html-to-text` within 2x raku and
  under the test's 30 s bound.
- **Builds on**:
  - [ADR-12529](12529-frames-resolve-names-through-the-static-link.md): static link; Accepted,
    phases 1-3 in progress.
  - [ADR-14633](14633-call-frame-materializes-no-name-keyed-env.md): no name-keyed `Env` per
    call; Accepted, its lazy `Env` deferred by A1.
  - [ADR-15737](15737-raku-level-call-path-footprint.md): D1 per-site call plan; Accepted, not
    built.
  - [ADR-17558](17558-compile-time-use-loads-in-the-program-interpreter.md): compile-time `use`;
    Accepted.
  - [ADR-7576](7576-grammar-actions-at-reduction.md): actions at reduction.
  - [docs/vm-single-store.md](../vm-single-store.md).
- **Symptom issues it would close or move**:
  - #16897 (Web t/12), #15688 and #15498 (Mooish, Red);
  - #12520 (FunctionalParsers), #12917 and #18207 (grammar actions), #17265 (Moneymoor);
  - #17971, #17966, #17659, #9171, #12151, #18208 and #18211 (the individual floors).

## 1. Context

`html-to-text` on the `t/12` megabyte takes 27.9 s on a profiling build of main@0ce9c1e1 (4-core
container) and 7.1 s on raku 2026.x: **3.9x**. The ecosystem sweep runner is about 2x slower
than this box, so the file needs **≥2x** to pass the sweep's 30 s per-file limit, and about 2x
more to meet #16897's own 2x-of-raku goal.

Twelve perf sessions have each landed 1-6% slices against it and against #15737:

- #18057, #18059, #18063, #18069;
- #18214, #18223, #18234, #18238.

The profile is still flat: the top function is 4.2% self, and no source line is above ~5%. By
§5a of the `perf-tuning` skill, slices of that size cannot multiply to 2x. The three facts below
say why.

### 1.1 The floor, per construct

All numbers are callgrind slopes (the Ir per iteration difference between N=2000 and N=6000) on
the profiling build. Wall times are second-run µs per iteration, from `tmp/floor.raku` (§7), with
the mutsu and raku runs taken minutes apart.

| construct | mutsu Ir | mutsu µs | raku µs | ratio |
| --- | ---: | ---: | ---: | ---: |
| `for ^N {}` iteration | 337 | 0.02 | 0.57* | — |
| `$s = $l` | 1.1k | 0.09 | 0.31* | ≤1 |
| `my $x = $_` | 2.6k | 0.21 | 0.17 | 1.2 |
| `while $i < $n { $i++ }`, all native `int` locals | 2.0k | 0.50 | 0.064 | 8 |
| same, `$n` an outer lexical read by `GetGlobal` | 4.6k | | | |
| `my Str ($a, $b, $c, $d)` | 47k | 7.9 | 1.1 | 7 |
| sub call `f($_)` | 4.8k | 0.67 | 0.15 | 4.5 |
| typed `my sub is-sp(Str:D $c --> Bool)` call | 6.4k | 0.88 | 0.11 | 8 |
| closure call `c($_)` (`my &c = { $_ }`) | 18.1k | 2.6 | 0.16 | 16 |
| method `$o.m0` | 11.2k | 1.66 | 0.11 | 15 |
| accessor `$o.a` | 4.0k | 0.55 | 0.11 | 5 |
| `method setk($v) { $!k = $v }` | 16.8k | 3.4 | 0.16 | 21 |
| `T.new(kind => 'text', name => 'p')`, 4 attributes | 20.7k | 3.3 | 1.1 | 3 |
| `%h{'k'} = $_` | 6.3k | 0.97 | 0.10 | 10 |
| `$str.substr($_, 1) eq ' '` | 5.0k | 0.88 | 0.20 | 4.4 |
| grammar, 1 word, no actions | | 1.9 | 5.0 | **0.4** |
| grammar, 1 word, 2 trivial actions | | 17.9 | 12.9 | 1.4 |

\* Raku had not warmed up for these rows (its spesh had not kicked in), so the ratio is not
meaningful there.

Straight-line statements are already at or below raku's cost: stores, `for` over a range,
concatenation, and regex matching itself. The gap sits in four places:

- **Calls**: 4-21x.
- **Declarations that are not a plain scalar**: 7x.
- **Loops over native `int` with a compare and `++`**: 8x.
- **Aggregate element stores and short-lived strings**: 4-10x.

These are exactly the constructs the tokenizer and renderer of `Extract.rakumod` are made of. The
code is idiomatic small-sub Raku: `$probe++ while $probe < $len && is-space($html.substr($probe,
1))`, nested `my sub`s closing over `$html`, `%memo` and `@tokens`, and `Token.new` per tag.

### 1.2 Where one cheap op's 400 Ir go

`while $i < $n { $i++ }` compiles to `WhileLoop`, `GetLocal`, `GetLocal`, `NumLt`,
`PostIncrement`, `SinkPop`. That is 2.0k Ir per iteration, about **400 Ir per op**, where a lean
interpreter spends 10-30. Per iteration (`MUTSU_JIT=off`, callgrind):

| cost | Ir |
| --- | ---: |
| `exec_one_dispatch` + `exec_ops_group_*`, the two-level dispatch | ~560 |
| `run_range*` loop driver | ~320 |
| `exec_scalar_incdec_op` + `set_local_scalar_metadata` | ~330 |
| **name-keyed env mirror of the `$i` write** (`set_env_plain_lexical` → `Env::insert_sym` → `Tier::insert` → HashMap) | **~230** |
| `check_named_incdec_readonly`, a readonly check **by name** | 90 |
| `num_lt_values`, `eval_truthy_impl`, `Value::truthy` | ~170 |

Moving `$n` to an outer scope adds a `GetGlobal` by name, which costs **+2.2k Ir per read**. It
goes through `active_unit_lexical_entry`, `qualified::flags::of`, `memchr` and a `HashMap<String,
Value>`. The `--dump-bytecode` of a nested `my sub find` in the tokenizer shows the same thing for
closures: `$html` is `GetGlobal("html")` and `%memo` is `GetHashVar("%memo")`. ADR-12529 §2.1
decided that these are compile-time slot or upvalue reads; for this shape they are not, yet.

### 1.3 The workload's profile, by family

The `html-to-text` slope between scale 0.1 and 0.2 is 8.8G Ir. Its self cost, merged per function
with `cg-summary.py` and bucketed by name, splits as follows:

| family | share | typical functions |
| --- | ---: | --- |
| name / env / symbol lookup | 18.9% | `Env::get_sym`, `walk_with_fallback`, `static_link::lexically_scoped` → `Symbol::as_str`, `HashMap<&str>`, `memcmp`, `CaptureView::collect_unshared` |
| value tag decode + op dispatch | 18.6% | `exec_one_dispatch`, `view_kind`, `payload_op`, `run_range*`, JIT `step` |
| call / frame machinery (self only) | 10.5% | `call_compiled_method_fast_in_unit`, `call_compiled_function_positional_light_at`, frame push/pop |
| alloc / free / drop | 10.0% | `malloc`/`_int_free`, `release_nested`, `Tier::new` |
| local and declaration stores | 7.8% | `exec_set_local_op_inner`, `exec_set_var_dynamic_op`, `exec_get_local_op_inner` |
| long tail of generic handlers | 32% | `exec_index_op_with_positional`, `lexsub_alias_slot_sym`, `try_resolved_type_capture_name`, `user_infix_override`, `mark_failure_handled_on_stack`, ... |

`MUTSU_VM_STATS` at scale 0.1 counts:

- 4.4M interpreted opcodes, among them `LexImportBuiltinOp` 342k, `SetVarDynamic` 292k,
  `GetGlobal` 223k, `SetLocalDecl` 159k and `CheckReadOnly` 52k;
- 270k native method calls, 92k accessor calls, 30k user method calls and 10k constructors;
- 117k sub calls and 16k closure calls;
- 91k env deep copies and 140k scoped env overlays;
- 1,000 Cro::Uri grammar parses with their actions.

The JIT neither helps nor hurts: `MUTSU_JIT=off` is equal at full scale. It is subroutine
threaded, with one helper call per op, so a fat op stays a fat op.

## 2. The structural causes

Five causes, each the *design* of a layer rather than a slow function in it.

- **S1. Names are still resolved by name on the hot path.** Three things in a running frame are
  reached by name:
  - an outer lexical (`GetGlobal`, `GetHashVar`, `exec_array_push_op` → `Env::get_sym` for
    `@tokens.push`);
  - a write that something might read by name (the env mirror);
  - a declaration's bookkeeping (`SetVarDynamic`: the dynamic map, the readonly set,
    `env_free_decl_slot`).

  A latch decides at run time whether the name side is needed. Rakudo resolves all three at
  compile time to (outer depth, index). This is the unfinished part of ADR-12529.
- **S2. A call is a protocol of ~17 functions, not a frame push.** It pushes five side stacks
  (`loop_local_vars`, `loop_local_saved_env`, `active_loop_param(_rw)_names`,
  `block_declared_vars`) plus the routine stack, the call-frame stack, an `Env` clone with a
  scoped child, and a dispatch frame. On return it reconciles attributes and merges the env.
  Things that depend only on the code and the class are recomputed per call:
  - `is_role`, `method_body_package` and `attr_default_classes`;
  - `method_return_fast` and the role-binding probes.

  This is ADR-15737 D1 and ADR-14633, decided but not built.
- **S3. Every op handler is generic and checks the rare cases per execution.** For example,
  `PostIncrement` asks about readonly, the env mirror, atomic and typed. The dispatch is
  two-level: `exec_one_dispatch`, then `exec_ops_group_NN`. The lean lanes added piecemeal
  (`exec_set_local_decl_fast`, the scalar-store lane, the attr lane) are each tried *after* the
  generic entry, so every op still pays the triage.
- **S4. The grammar-action harness is a dynamic-scope emulation.** Each action:
  - snapshots the visible env into a `HashMap` and diffs it afterwards;
  - inserts `/`, `_` and `made` as `String` keys;
  - materializes the lazy Match (`MatchNode::materialize_map`);
  - builds candidate method names with `format!`.

  That is ~73k Ir of harness per action against ~0 on raku (#18207). Matching without actions is
  already faster than raku.
- **S5. Operators and short-lived values are decided late.** Every built-in operator in a unit
  with an operator-exporting `use` carries a `LexImportBuiltinOp` prefix, because the compiler
  runs before `use` does (342k executions at scale 0.1; #18211, ADR-17558). `substr` allocates a
  `String` plus an `Arc` per call.

## 3. Options

Gains are estimated against the 8.8G Ir slope of §1.3, from the bucket a change removes and the
per-construct floors of §1.1. Where an upper bound was measured, it is quoted.

### O1. Finish static lexical addressing (ADR-12529 for free variables, declarations and the env mirror)

The compiler resolves every lexical a unit can see to a slot (own frame) or an outer reference
(depth plus slot, or an ADR-0018 cell for an escaping mutable). That covers:

- `my sub`s' free variables;
- the unit-level `my`s that subs read (#17966, #17659);
- aggregate method targets (`@tokens.push`).

`GetGlobal` remains only for names the compiler cannot see: `EVAL`, symbolic `::()`, pseudo-
packages and `CALLER::`. The env mirror of a write, and `SetVarDynamic`'s name bookkeeping, are
emitted only when the compiler proves a name-keyed reader can exist. That proof is per unit, plus
the existing process-wide reflective latches (#18020). Otherwise a declaration is one slot
initialization.

- **Gain**: removes most of the name bucket (18.9%) and the declaration and store tail (7.8%).
  Allow for the legitimate hash lookups that remain (`%memo{$needle}`, method tables): **~15-18%
  of Ir (≈1.2x)**. Per construct:
  - an outer read goes from 4.6k to ~2.4k (it costs what a local does);
  - the `$i++` loop drops the ~320 Ir of mirror and named readonly (-16%);
  - `my Str ($a,$b,$c,$d)` goes from 47k to ≲8k.
- **Cost / risk**: the largest of the five. It touches the compiler's scope analysis, closure
  capture and every by-name consumer. The risk is code that only works because a callee sees a
  name it should not (ADR-12529 §5); roast and the ecosystem ledger are the net. A missed name
  reader is a wrong answer, not a crash. The latch fallback is the existing by-name path, not a
  new slow path.
- **Also helps**: every closure-heavy dist; #12520 (ADR-12529's own target, 2.5x upper bound);
  #12151; bench-tak, bench-fib and poly-call.
- **Correctness**: the by-name path stays as the guarded remainder for the reflective consumers
  named in ADR-12529 §2.1, and nothing new routes through it.

### O2. One lean call frame: ADR-15737 D1 plus ADR-14633 §2.3, for every Raku-level call

There is one frame record, and every call kind uses it: subs, methods, closures, accessors,
submethods/`BUILD`/`TWEAK`, and grammar action methods.

- **Pushed per call**: the code, the outer link, the slots window, the return target and the
  caller link.
- **Moved into the frame record**: the five side stacks become fields that are initialized
  lazily.
- **Kept in the call site's plan** (ADR-15737 D1): everything that is a function of (code, class),
  keyed by the class stamp and the registry stamp. The generic path stays as the guard-miss tail.
- **Not built**: no `Env` overlay. O1 removes the reason for one; until it does, the overlay is
  materialized only where `needs_env` is set.

- **Gain**: call overhead is the per-call floor minus the body. Targets per call:
  - method 11.2k → ≤3k;
  - sub 4.8k → ≤2k;
  - closure 18k → ≤3k;
  - accessor 4k → ≤1.5k;
  - constructor 20.7k → ≤10k.

  Weighted by §1.3's counts (30k methods, 92k accessors, 117k subs, 16k closures and 10k ctors at
  scale 0.1), that is **~15-18% of Ir (≈1.2x)** on this workload. It is more on Mooish and Red,
  where calls are the bulk (#15737 measured 4.4M Ir per Mooish iteration, mostly calls).
- **Cost / risk**: high. The frame layout is shared by `callsame`/`nextsame`, wrappers, `LEAVE`,
  `CALLER::` and `callframe`. Plan invalidation must be exact: a missed stamp is a wrong dispatch.
  ADR-15737's multi, wrap and `add_method` tests are the pins.
- **Also helps**: #15688 Mooish, #15498 Red, #8880, bench-class, bench-ctor, method-call,
  wrapped-method and bench-multi-dispatch.
- **Depends on**: O1 for the "no `Env` per call" half. The plan half is independent.

### O3. Specialized ops chosen at compile time, with a single-level hot dispatch

The compiler emits a narrow op whenever it can prove the generic op's rare cases are absent:

- `IncNativeIntSlot(slot)` and `LtIntSlots(a, b)`;
- a plain scalar store for an untyped, non-readonly, unmirrored scalar slot;
- a direct built-in operator op when no operator import is in lexical scope.

Each narrow op handles exactly its case. When a process-wide latch that the proof depended on is
raised (`OPERATOR_IMPORT_POSSIBLE`, the reflective latches), the code is recompiled or the narrow
op checks one word and falls into the generic handler. That is the "guarded lane", not a new
slow path. The hottest ~30 ops are dispatched from one flat `match` before the grouped
dispatcher.

- **Gain**: roughly half of the dispatch and generic-handler families (18.6% + part of the 32%
  tail): **~10% (≈1.1x)**. The native-int loop target is ≤600 Ir per iteration, from 2.0k.
- **Cost / risk**: medium, mechanical and measurable per op. The risk is a proof that is too
  optimistic, which gives a wrong answer. Each narrow op gets a parity test against the generic
  op, the shape of `t/vm/nqp-str-prim-parity.t`.
- **Also helps**: every loop benchmark (int-arith, num-arith, bench-mandelbrot,
  bench-ripemd-rounds) and #12151.
- **Depends on**: O1. Most of what the generic handlers triage per execution is name-keyed state
  that O1 removes, so O3 after O1 is cheaper and larger than O3 alone.

### O4. A grammar-action harness without dynamic-scope emulation (#18207)

An action runs on the lazy Match node, extending `invoke_leaf_action_lazy` to non-leaf nodes
whose children have reduced. `$/` and `made` are slots of the action's frame, not `String` env
keys, so there is no env snapshot or diff. The action method plan is resolved once per (actions
class, rule) and handed to O2's frame.

- **Gain**: #18207's close condition is ≤10k Ir of harness per action, from ~73k. Here that is
  the bulk of the 840k-of-1.29M Ir harness share per `parse-ref`, on two parses per link:
  **~7-9% (≈1.09x)**.
- **Cost / risk**: medium and contained to `src/runtime/regex/` and `methods_grammar*`. Action
  side effects through `make`, `$/` assignment and `.made` must stay observable, as ADR-7576
  requires.
- **Also helps**: bench-yaml-parse(-big) (#12917, ~5x rakudo), bench-grammar-json-tiny,
  bench-json-fast(-spdx) and every Cro-based dist.
- **Depends on**: nothing. It benefits from O2, but it can land first.

### O5. Allocation and late-decided values

- `substr` (and `substr`-like slices) return a `StrBody::Slice { parent, range }` view instead of
  copying.
- The `LexImportBuiltinOp` prefix is not emitted once compile-time `use` (ADR-17558) gives the
  compiler the unit's import set.

- **Gain**:
  - the prefix: #18211's unsound upper-bound experiment measured **~5%** overall;
  - the slice view: about a third of the 10% alloc bucket, **~3%**.

  Together ≈1.08x.
- **Cost / risk**: low for the view, which hides behind `StrBody`'s existing `Lazy` arm. The
  prefix part rides on ADR-17558's own migration.
- **Also helps**: bench-string, bench-regex-long-subject and every string-scanning dist.

### Rejected for now: a register-allocating JIT tier

With fat ops, a native tier can only call the same fat helpers: the JIT is already measured at 0%
on this workload. After O1-O3 the remaining op cost is the helper call itself, and only then does
a tier that keeps native `int` slots in registers have something to remove. Revisit after O3.

## 4. Arithmetic

| option | share of Ir removed (this workload) | multiple alone |
| --- | ---: | ---: |
| O1 static lexical addressing | 15-18% | 1.18-1.22x |
| O2 lean call frame | 15-18% | 1.18-1.22x |
| O3 specialized ops | ~10% | 1.11x |
| O4 action harness | 7-9% | 1.08-1.10x |
| O5 slices + operator prefix | ~8% | 1.09x |
| **all five** | **~50-58%**, less overlap (O1/O2 share the per-call env overlay; O1/O3 share the latches): **~45-52%** | **~1.8-2.1x** |

So 27.9 s becomes **~13-15.5 s**, against raku's 7.1 s: **~1.9-2.2x raku**. That passes the
sweep's 30 s limit with margin; #16897's 2x goal is reached only at the top of the range.

The estimate ignores instruction speed. This workload runs at ~3.2 Ir/ns, against ~5 Ir/ns on a
plain call loop (ADR-15737 §Context 2). O2 and O3 shrink the hot code to a few functions, and
that is the only code-level lever on that factor, so the wall-clock result may beat the Ir
estimate.

**No subset of the options reaches 2x.** O1 plus O2 alone give ~1.4-1.5x, so 27.9 s becomes
19-20 s here and ~40 s on the sweep runner. That is why a plan, not the next slice, is the
decision this ADR asks for.

## 5. Decision (proposed)

1. Treat O1-O5 as **one program with an order**, and stop landing slices on #16897 / #15737
   that are not a step of it. Bounded fixes found on the way (a copy where a borrow exists) are
   still fine, but are not reported as progress on the ratio.
2. **Order** (each stage re-measures `tmp/web.raku` at full scale plus the §1.1 table, and quotes
   the stage target):

   | stage | content | target (Ir, callgrind slope) |
   | --- | --- | --- |
   | A | **O4** action harness (#18207), independent and contained | #18207's ≤10k per action |
   | B | **O1** first cut: free variables of `my sub`s and closures, plus unit-level `my`s read by subs, to slot or outer references; `exec_array_push_op` and friends on outer aggregates | outer read = local read (±10%); `GetGlobal` executions on `tmp/web.raku` 0.1 ≤ 5k (from 223k) |
   | C | **O1** second cut: declarations and the env mirror emitted only on a compile-time proof of a name-keyed reader | `my $x = $_` ≤ 1.4k; `$i++` loop ≤ 1.7k; `SetVarDynamic` executions ≤ 10% of today's |
   | D | **O2** lean frame: one record, side stacks folded in, per-site plan (ADR-15737 D1a/D1b), no `Env` overlay | method ≤ 3k, sub ≤ 2k, closure ≤ 3k, accessor ≤ 1.5k |
   | E | **O3** specialized ops plus the flat hot dispatch | native-int loop ≤ 600 / iteration |
   | F | **O5** `StrBody::Slice` (any time) and the operator prefix (after ADR-17558) | `LexImportBuiltinOp` executions 0 on `tmp/web.raku`; `substr-eq` ≤ 3.5k |

   A runs first because it is the cheapest, it is independent, and it lands a measured ~8% while
   B-D are designed in detail. B and C are the bulk of the risk and of the gain, and D needs them.
3. **Stop rule**: after each stage, if `html-to-text` on the sweep runner is under 30 s, report
   that on #16897. The remaining stages continue only against their own issues' goals, which are
   #15737, #12520 and #12917.
4. **Each stage gets its own child issue and, where its design is not already an accepted ADR,
   its own short ADR**:
   - B and C extend ADR-12529, so a phase entry there suffices;
   - D is ADR-15737 D1 plus ADR-14633 and needs no new ADR;
   - E needs one, for the deopt/latch contract of narrow ops.

## 6. Consequences

- **Positive**: one plan whose arithmetic reaches the goal, instead of slices whose arithmetic
  cannot. Every stage removes mechanism instead of adding a lane in front of it, which is what
  makes ADR-15737's factor (2) (code footprint) move at all. Every stage also helps Mooish, Red,
  FunctionalParsers and the YAML/JSON grammar benchmarks, not only this file.
- **Negative**: B-D are each several weeks of work on the interpreter's most-shared paths, and
  the compatibility risk of ADR-12529 phase 3 applies. Until stage D lands, #16897 stays red on
  the sweep.
- **Unknown**: the overlap between O1 and O2, which is the 45-52% versus 50-58% in §4. It is
  measured after B.

## 7. Reproduction

Scaled fixture (`tmp/web.raku`), run with the `-I` list from
`.agents/skills/ecosystem-dist-fix/checkout-dist.py MCP::Server::Tool::Web`:

```raku
use MCP::Server::Tool::Web::Extract;
my $s = +(@*ARGS[0] // 1);
my sub sc($n) { ($n * $s).Int max 1 }
my $doc = join '',
    '<!doctype html><html><head><title>Pathological</title>',
    '<script>var s = "</div></p><!-- ";</script></head><body><main>',
    (1 .. sc(5000)).map({
        '<div class="c1 c2" data-json=\'{"k":">"}\'><p>Para with '
            ~ '<a href="/page/' ~ $_ ~ '">a link</a> and &amp; entities.</p></div>'
    }),
    '&' x sc(30000), '<' x sc(30000), '<!' x sc(15000),
    ('<ul><li>item</li><li>item<ul><li>deep</li></ul></li></ul>') x sc(2500),
    ("<pre>code &lt;here&gt;\n  indented</pre>") x sc(3000),
    '<div>' x sc(5000), 'deep', '</div>' x sc(5000),
    '<p ', ('data-x="1" ' x sc(10000)), '>giant tag</p>',
    '</main></body></html><!-- and a comment that never ends';
my $t = now;
my $text = html-to-text($doc, base => 'https://example.test/dir/page.html');
say "html-to-text: ", (now - $t).round(0.01), " s";
```

The micro floors in §1.1 come from one `bench(name, block)` per row. Each body runs once to warm
and once timed, as `for ^$N { <construct> }`. Ir slopes are taken with
`valgrind --tool=callgrind` at N=2000 and N=6000.
