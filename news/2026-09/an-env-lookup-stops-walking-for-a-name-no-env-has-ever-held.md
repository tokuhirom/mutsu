# An env lookup stops walking for a name no env has ever held

`Env::get_sym` was 7.0% of a `JSON::Fast` parse — the single largest entry in
its profile — and most of that was spent proving that a speculative metadata
key is *not* there. A one-bit per-symbol latch answers that without touching a
map, and takes the whole function down by 66%.

## What was happening

A miss is the expensive direction of an env lookup: it walks every overlay tier,
then the base tier, then each capture fallback, before it can say `None`. And a
miss is the *common* outcome, because the VM speculatively probes a handful of
`__mutsu_*` metadata keys derived from a variable's own name on every `my`,
every store and every call entry (`MetaNs`).

`src/env.rs` already guards those probes with monotonic process-global
booleans — `BOUND_KEY_SEEN`, `SHAPED_ARRAY_DIMS_SEEN`, `PLACEHOLDER_KEY_SEEN`
and friends. But they are per *namespace*: one `:=` anywhere in the program
arms `BOUND_KEY_SEEN` for the whole process, and from then on every other
variable pays a full chain walk to discover that *it* has no `__mutsu_bound::`
entry. A program that loads any real module arms most of them.

Measured on the 100-record `bench_json.raku`, warm:

| | Ir | share | calls | per call |
|---|---:|---:|---:|---:|
| `Env::get_sym_with_fallback` | 133,098,987 | 7.01% | 659,610 | 202 |
| `Env::base_get` | 8,341,485 | 0.44% | | |

## What it does now

`flags::EVER_ENV_KEY`, a new bit in the per-symbol flags word that
`src/symbol.rs` already keeps in a lock-free chunked `AtomicU16` table. Unlike
every other bit there it is not a property of the symbol's string: it is a
one-way latch set by `symbol::mark_env_key` at the sites that can put a key
into an env. `symbol::maybe_env_key` reads it, and a **clear bit is a proof of
absence** — no env in the process has ever held this name, so the walk cannot
find anything.

The set of marking sites is closed, and that is not a review promise: `Tier`
keeps its map private and every mutator lives in `src/env_tier.rs`, so the only
ways a key can enter an env are `Tier::insert`, `Tier::new` (every whole-map
rebuild) and the two base-tier installs. The one escape hatch that handed out a
`&mut SymMap` — `Tier::map_mut`, reached through `Env::inner_mut`, which had no
callers at all — is gone; its test-only replacement takes a closure, so there
is no borrow to hold past the marking pass.

Two things make an under-marking bug loud rather than silent:

* a `debug_assert!` in both `get_sym` and `contains_key_sym` runs the real walk
  whenever the latch says "never", and fails naming the key. The `gc-stress-tap`
  and `jit-stress-tap` CI jobs run the whole TAP suite on a debug binary, so
  every insert path the suite exercises is checked against the latch.
* `mark_env_key` calls `flags()` before setting the bit, which guarantees the
  slot is `COMPUTED` and so takes the one plain `store` that could clobber it
  (`compute_and_store_flags`) permanently off this id's path. A symbol id beyond
  the flag table's reach answers `true`, the conservative side.

The latch is monotonic and never cleared: a key removed from its only holder
keeps its bit, which merely means the (correct) walk still runs for it.

### The probe goes after the leaf overlay, not before it

The first version asked the latch first and cost the two loop benchmarks ~1.1%.
That is not noise and it is not mysterious: `my int $i` really does create
`__mutsu_type::$i`, so a typed loop's probes are all for keys that *do* exist,
and the filter never fires while still being paid ~31 Ir per iteration.

Probing the running frame's own overlay first fixes it. A name the frame owns
answers there and pays nothing; only a miss reaches the latch. That is worth
0.8% on `JSON::Fast` **and** turns the loops' regression into a small win.

## Measurements

Baseline `8a89bae6`, release-optimized `--profile profiling` builds of the same
tree with and without the change, first run after each build discarded (the
module precompilation cache is cold there — worth 650M Ir on this benchmark).

| | before | after | |
| --- | ---: | ---: | ---: |
| `bench_json.raku`, 100 records | 1,926,066,578 | 1,843,150,440 | **-4.30%** |
| `nqp::islt_i` + `nqp::add_i` loop | 284,875,976 | 283,921,640 | -0.34% |
| plain `$j = $j + 1` loop | 226,058,474 | 225,108,859 | -0.42% |

The two loops are the no-regression check, not the point: their probes are for
keys that exist, so the latch can only pay for itself there by being cheap.

Where it went, on the pre-rebase pair (`61176bc7`, 1,899,012,715 -> 1,820,915,838,
**-4.11%**):

| | before | after | |
| --- | ---: | ---: | ---: |
| `Env::get_sym_with_fallback` | 133,098,987 | 45,544,765 | **-65.8%** |
| `Env::base_get` | 8,341,485 | 1,169,447 | **-86.0%** |
| `HashMap::contains_key` | 54,934,840 | 44,719,837 | -18.6% |
| `Env::insert_sym` | 9,183,541 | 8,283,807 | -9.8% |
| `Env::get_sym` (the filter itself) | 3,320,169 | 14,333,245 | +331.7% |
| `Tier::insert` (the marking) | 0 | 5,151,553 | new |

The change pays about 16M to save about 95M.

## What this is not

It is not a step toward parity, and the issue it comes from
([#8830](https://github.com/tokuhirom/mutsu/issues/8830)) should stop being read
as one. `from-json` on a 272KB document is **1.90s against rakudo's 0.032s —
about 59x** — and summing the 100-record profile by category shows the gap is
not concentrated anywhere a name-resolution campaign can reach:

| | Ir | share |
|---|---:|---:|
| runtime name/type resolution (this issue's thesis) | ~416M | 22% |
| allocation (`malloc`/`free`/`memcpy`) | ~289M | 15% |
| interpreter dispatch | ~137M | 7% |

Eliminating *all* of the first row is 1.28x. All three rows is 1.8x, which
leaves 33x. Getting under 10x needs the unit of compilation to change, not the
hot spots inside it — see the register-residency note in
`news/2026-09/a-typed-native-scalar-store-takes-the-fast-path.md`, whose real
constraint is that `vm_jit_tier_b.rs` reloads the stack data pointer and length
at every opcode.
