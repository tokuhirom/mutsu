# Running an embedded regex code block costs more in scope setup than in the block

Successor to `todo/perf/closure-sequence-evolution-performance-gap.md`, whose
headline (a Weasel-program search "85x slower than raku") turned out to be a
debug-build number and whose one real defect — recompiling an embedded regex
code block at every cursor position — is fixed in
`news/2026-09/regex-inline-code-recompiled-per-cursor-position.md`. What that
fix left behind is this: evaluating a two-token assertion body costs roughly
1.5x as much in carrier-block bookkeeping as it does in the body itself.

## Repro

```raku
my $seed = "A" x 29;
for ^10000 { $seed.subst(/<?{ rand < .001 }> . /, "X", :global); }
```

The assertion is evaluated once per cursor position: 290,000 evaluations of
`rand < .001`. Release, `taskset -c 2`, median of 5:

- `raku`: 0.23s (0.34s wall minus its ~0.11s startup)
- mutsu: **0.83s** (was 1.53s before the compile-cache fix)

That is ~2.9us, ~8000 cycles, per evaluation of a two-operand comparison.

## Where it goes

`perf record --call-graph=lbr` on `target/profiling/mutsu`, children percentages:

| | |
| --- | --- |
| `eval_regex_inline_code` | 92.3% |
| ... of which `eval_block_value_inner` | 63.1% |
| ... ... of which `run_compiled_block` (the body actually running) | **37.2%** |

So ~29% of the benchmark is `eval_regex_inline_code`'s own per-position binding
setup and teardown, and a further ~26% is `eval_block_value_inner`'s block-scope
prologue/epilogue around a body that costs 37%.

The flat profile has no single dominant symbol — the cost is spread across
`LocalKey<T>::with` (17.4% self, split over two instantiations),
`eval_block_value_inner` itself (4.5%), malloc/free (~13% combined),
`Symbol::intern` (13.7% children), `HashMap::retain` (2.0%),
`RawIterRange::fold_impl` (1.6%) and `Env::insert_sym` (1.7%).

## The one measured lead

`eval_block_value_inner` snapshots every `&`-code-var env entry on entry and
`retain`s + reinstalls them on exit:

```rust
let saved_code_env: HashMap<Symbol, Value> = self.env.iter()
    .filter(|(k, _)| k.starts_with("&") || k.starts_with("__mutsu_callable_id::"))
    .map(|(k, v)| (*k, v.clone())).collect();
// ... body ...
self.env.retain(|k, _| !(k.starts_with("&") || k.starts_with("__mutsu_callable_id::")));
for (k, v) in saved_code_env { self.env.insert_sym(k, v); }
```

That is two full env walks per carrier call, and each key tests two prefixes via
`Symbol::as_str()` — which is where a large share of the 17.4% `LocalKey::with`
comes from (`RESOLVE_CACHE` is thread-local). Deleting both halves outright
(an unsound experiment, purely to size the prize) took the benchmark from
**0.83s to ~0.69s, about 17%** of what is left.

Two candidate fixes, neither implemented:

1. **Cheap and contained:** add a `CODE_VAR` bit to the existing `SymFlags`
   memo in `src/symbol.rs` (there are free bits; `TYPE_META` / `NQP_OP` are the
   precedent) so the predicate is one flag lookup instead of two `as_str()`
   round trips. Recovers maybe half the 17%.
2. **The real fix:** an undo log. `Env` would record `(sym, old value)` for every
   code-var write made while a block scope is open, so the common block — which
   writes none — pays nothing on either side. Needs every env write path
   (`insert`/`insert_sym`/`insert_through`/`insert_through_sym`/`remove`/
   `remove_sym`/`retain`/`retain_overlay`) to funnel through the log; that is the
   part that makes this a design-sized change rather than a patch.

## Two hypotheses that were measured and are WRONG

Do not re-derive these:

- **The registry snapshot is not the cost.** `eval_block_value_inner` clones
  `registry().functions`, `proto_subs_snapshot()`, `proto_functions`,
  `operator_assoc` and `user_declared_infix_ops` on entry and (since the
  `registry_write_gen` guard) usually throws them away. Replacing all five with
  `Default::default()` and disabling the restore changed the benchmark by
  **nothing** (0.83s -> 0.84-0.87s).
- **Building `$/` is not the cost.** `eval_regex_inline_code` constructs a full
  `Match` object (`make_match_object_full`) per cursor position even when the
  body never mentions `$/`. Skipping it entirely moved the benchmark from 0.83s
  to 0.75-0.82s — inside the noise band.

## Notes for whoever picks this up

- Measure in **release**. The debug binary is ~50x slower here and its cost
  breakdown is different; that is exactly what made the predecessor ticket
  misleading.
- `callgrind` cannot attribute this benchmark as-is: mutsu's JIT is on by
  default and its generated code shows up as unresolved `???` addresses (~22%
  of Ir). Use `perf record --call-graph=lbr`, which does work, or set
  `MUTSU_JIT=off` first.
- `perf` here is `/usr/lib/linux-tools/6.8.0-138-generic/perf`, needs no `sudo`,
  and the box is hybrid — read the `cpu_core/` rows and pin with `taskset -c 2`.
