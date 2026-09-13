# One `my $x := $y` taxed every scalar store in the program

`bench-threads-serial` was the worst row in the whole benchmark suite — ratio **2.07** against
rakudo (1.88 with the JIT on), where the next worst, `bench-threads`, was 1.59 and everything else
sat between 0.04 and 1.18. Read together with its threaded twin, as the file's own header asks you
to, that says the per-op cost of a container is bad rather than that threading scales badly. It
turned out to say something much less specific than that: **the row is not about containers either,
and it is certainly not about threads.**

## What the measurement actually said

Splitting the benchmark into its component costs at mainline scope (release build, this container,
against Rakudo v2026.07) refuted the obvious hypothesis first. The `:=`-bound containers the file
goes out of its way to exercise cost the same as plain ones:

| 400,000 iterations, mainline scope | mutsu | raku |
| --- | ---: | ---: |
| `$s = $s + @data[$i +& 255]`, plain `@data` | 824 ns/iter | 224 ns/iter |
| the same through a `:=`-bound `@alias` | 828 ns/iter | 232 ns/iter |

So celling is free here. What is not free is the presence of the file's one scalar binding,
`my $sref := $seed`. Adding an *unused* `my $x := $y` to a loop that touches no array at all:

| 400,000 iterations of `$s = $s + ($i +& 255)` | ns/iter |
| --- | ---: |
| as written | 480 |
| with an unrelated `my $x := $y` in scope | 905 |

**1.87x slower, with no array, no container and no thread in sight.** Every loop in
`bench-threads-serial` paid that, which is most of what put the row above rakudo — and it put the
blame on the one subsystem that had nothing to do with it.

## Root cause: three whole-program latches for a per-slot fact

A scalar `:=` arms three gates, and all three are consulted by the plain-scalar store fast path
(`exec_set_local_scalar_fast`, #8094) and by the read-modify-write path behind `++`/`--`/`OP=`:

1. `local_bind_pairs` becomes non-empty. The fast path declined on `!is_empty()`, but the only
   thing the full store does with those pairs is propagate a write *from a pair's source slot*.
2. `SIGILLESS_READONLY_KEY_SEEN` is armed, because the bind wrote
   `__mutsu_sigilless_readonly::<name>` **even when the value was `False`** — i.e. even for a
   binding that is not readonly at all. Every reader of that key tests for `Bool(true)`
   specifically, so the `False` entry answered no question; it only armed the latch, and the latch
   gates a `format!`ed, interned env probe on every `CheckReadOnly` and every `++`/`--`.
3. `CLOSURE_META_KEY_SEEN` is armed by the `__mutsu_sigilless_alias::<name>` key, which the bind
   genuinely needs. That latch stands for "walk the forward alias chain", a walk that starts at one
   slot's own key and does nothing for any other slot.

Each is monotonic and process-global, so one binding anywhere turned them on for the rest of the
run. The tax is diffuse rather than concentrated — differencing two `callgrind` runs shows it as
hashbrown probing and `Symbol` interning spread across a dozen sites, which is why it never showed
up as a hot spot.

## The fix: ask the per-slot question

- `local_bind_pairs` is consulted as `slot_is_bind_pair_source(idx)` — exactly the condition under
  which the full store's reverse-propagation loop does anything. (The forward walk, the other way a
  pair's slot can be reached, has its own gate in the same list, so it is already excluded.)
- The `:=` store writes `__mutsu_sigilless_readonly::<name>` only when the source really is
  readonly, and clears a stale marker instead of overwriting it with `False`. A program that never
  binds a readonly name now never arms that latch.
- `CLOSURE_META_KEY_SEEN` is split into `SIGILLESS_META_KEY_SEEN` (the `__mutsu_sigilless_*`
  family) and `CLOSURE_STATE_META_KEY_SEEN` (`__mutsu_state_key::`,
  `__mutsu_predictive_seq_iter::`), with `closure_meta_keys_possible()` unchanged as the union so
  every existing consumer keeps its current answer. The store fast path then asks
  `slot_has_sigilless_meta(code, idx)`: the narrow latch as the cheap `false`, and otherwise one
  probe of *this slot's* pre-interned alias key.
- `propagate_sigilless_alias_chain` takes the caller's local slot, so the three
  read-modify-write sites that have one use `CompiledCode::alias_sym`'s pre-interned key instead of
  re-interning the name on every increment.

A program that makes no `:=` binding is unaffected: it answers all of this with the same handful of
relaxed atomic loads as before, and reaches no env at all.

## Result

Two thirds of the bind tax is gone. The same 400,000-iteration `$s = $s + ($i +& 255)` loop,
release build, idle box, with and without an unrelated `my $x := $y` in scope:

| | no binding | with an unrelated binding | the tax |
| --- | ---: | ---: | ---: |
| before | 490 ns/iter | 905 ns/iter | +415 ns |
| after | 494 ns/iter | 625 ns/iter | **+131 ns** |

It is not gone — the residue is one env probe per store and per RMW, plus the general cost of a
`Symbol`-keyed miss walking the overlay and the `GLOBAL_BASE` tier — and the rest of
`bench-threads-serial`'s gap is separate work: an indexed element *read* still costs ~330 ns of
marginal cost against rakudo's ~39 ns, and a `:=`-bound array's element *store* costs ~3,400 ns
against a plain array's ~800 ns, because #8151's fast store lane declines for a celled container.
Both are filed separately.

Pinned by `t/vm/writeback/scalar-bind-unrelated-store-semantics.t`, which exercises the stores that now reach
the fast path for the first time — typed lexicals, `is default`, `state`, readonly and `is rw`
parameter binds, sigilless aliases, three-name bind groups and redeclaration — in a file that has a
scalar `:=` in scope.
