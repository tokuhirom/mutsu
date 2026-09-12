# Env tiers carry a key-set memo, so closure capture skips what it can never keep

`use Test` taxes every hot loop in the file that loads it ([#7565]). Four rounds
have taken that tax apart; what was left at the top of the list was the walk
itself. `Env::filtered_flat` visits every name visible from the creating scope
on **every closure creation**, and after a bare `use Test` a closure created
inside a sub visits 96 keys to keep 31. Of the 65 it rejects, 49 are
`__mutsu_callable_id::<pkg>::<name>` routine-registration markers — one per
routine the import made visible — and the rest are attribute-twigil
materializations and `__mutsu_type::` metadata whose subject is one of those.

That is the ticket's thesis in miniature: the importing scope's ordinary code
gets slower in proportion to how much the module exports, and the mechanism is a
per-creation walk over names no closure can name.

## Why the obvious memo could not be built before

Every earlier attempt keyed the memo on the tier's **address** and failed the
same way. An address memo has to hold the tier's `Arc` to stop the allocator
recycling the address under it, and holding it forces `Arc::make_mut` to clone
the map on the next ordinary value write — so in a loop that writes a mainline
lexical the memo is invalidated before it is ever read. #7707 measured a
whole-chain version of this at +18%; #8019's note recorded the per-tier version
as unreachable for the same reason, and named the process-global
`skip_env_write` latch as the thing that would have to be fixed first.

It does not, because the address was never the right key. The rejects are a pure
function of the tier's **key set**, and a value write does not change that. So
the memo belongs on the tier, not beside it: identity becomes structural rather
than positional, and a copy-on-write clone carries the memo across, because a
clone has exactly the key set the original had.

## The change

`Env::inner` is now an `Arc<Tier>` (`src/env_tier.rs`) rather than an
`Arc<SymMap>`. A `Tier` is the overlay map — **private**, with every mutator
living on `Tier` — plus the indexes derived from its key set. Making the map
unreachable is the point: a key-set memo is only safe if nothing can add a key
behind its back, and this turns that from a review obligation into a
compile-time one. Mutators that can only *remove* a key leave the indexes alone,
because they are supersets by contract and the reader looks each key up.

The one index so far is `capture_candidates`: the tier's keys that a capture
could keep, i.e. everything `env_tier::capture_never_keeps` does not reject.
`Env::filtered_flat_capture` walks it instead of the map when it is much shorter
(below two thirds), and walks the map otherwise. A tier narrower than 32 keys
never builds the memo at all — a call frame's own overlay is a handful of
entries *and* is allocated fresh per call, so it would rebuild the list on every
capture and never read it twice.

A tier is memoized only on the **second** capture that walks it, which is the
same "wait for a repeat" discipline `vm_capture_cache` arms its own memo with. A
tier can be both wide and allocated fresh per call — a method frame that flattens
its chain gets one — and a capture inside it would build a list, read it once and
drop it. That is a pure loss, and it was **2.4% of `benchmarks/bench-ctor.raku`**
before the second-ask gate went in.

Three things were built, measured and thrown away on the way, all worth recording:

- **Hoisting the key-only half of the filter out into the walk is a
  regression.** It looks like the natural shape — the tier already knows that
  verdict, so why ask again — and it cost **+880 instructions per closure
  creation on an import-free program**, more than the memo saves on one that
  imports. Both halves of the filter open by loading the key's memoized flags
  word, and splitting them made every walked key pay that load twice. `keep` is
  therefore the whole filter in both arms; the memo's copy of the verdict is
  used only to decide *which keys to visit*, never to skip work inside the
  filter.
- **Sharing the loop body between the two arms through a closure is a
  regression too**, for the plainer reason that a capturing `FnMut` there is not
  inlined. The arms spell their body out.
- **Walking the candidate list unconditionally is a regression** on a tier where
  almost everything is a candidate: the list costs a hash probe per key where
  the map costs an iterator step. Hence the two-thirds threshold, and the
  32-key floor below which no memo is built at all.

`Env::filtered_flat` also stopped taking `&dyn Fn` and is monomorphized on its
predicate, which is where a little of the gain below comes from.

## Measured

Warm-run callgrind instruction slopes between 6 000 and 14 000 iterations,
`MUTSU_JIT=off MUTSU_GC=off`, release, against `main` at `23c7c216` built with
the same procedure. "Tax" is the `use Test` line minus its floor — the thing
this ticket is about.

| | base | after | |
| --- | --- | --- | --- |
| leaf loop, no `use Test` (the floor) | 76 344 | 76 777 | |
| leaf loop `+ use Test` | 88 237 | 87 323 | -1.0% |
| **its tax** | **11 893** | **10 546** | **-11.3%** |
| calling loop, no `use Test` | 83 007 | 83 659 | |
| calling loop `+ use Test` | 97 622 | 96 686 | -1.0% |
| **its tax** | **14 615** | **13 027** | **-10.9%** |

On the ticket's own loop the memo takes the mainline tier's walk from 92 keys to
43, and the whole capture from 96 visited keys to 47.

The floor lines are up 0.4-0.8%, which needs saying plainly: it is at the edge of
the drift band this particular filter is documented to sit in — #7964 built four
shapes of *identical* logic and measured them spanning 0.8% — and two whole
programs measured end to end say the same thing, so I read it as codegen shape
rather than added work:

| whole program, instructions | base | after | |
| --- | --- | --- | --- |
| `benchmarks/bench-ctor.raku` | 1 401 425 034 | 1 398 995 804 | -0.2% |
| `benchmarks/bench-class.raku` | 1 171 088 568 | 1 173 525 194 | +0.2% |

Neither imports anything, and both create closures inside method frames — the
shape the second-ask gate exists for. Numbers here are local; anything that ends
up in PERFORMANCE.md or PLAN.md has to come from the bench CI.

## What is left

Unchanged from #8042's note, with the walk item re-measured:

1. **The no-op capture merge** — the capture-as-a-fallback-tier design change,
   still architectural and still the biggest single item. `Tier` does not help
   it: the obstacle there is an identity token for the caller chain, and that is
   an address question, not a key-set one.
2. **`skip_env_write`'s process-global latch**, with the `EVAL`-scope question
   from #8019's note. It no longer gates anything on this list — the memo it was
   said to block is shipped — but it is still a per-store cost.
3. **The remaining walk.** 43 candidates for 31 kept, at a hash probe per
   candidate. The next step that looks worth having is putting each candidate's
   *resolved key and flags word* in the memo beside it, so the filter does not
   reload them — roughly 650 instructions per creation on this loop. It needs a
   second predicate for the candidate arm; the shared tail is
   `capture_never_keeps_resolved`, so the two cannot drift.
4. **The ~20 built-in dynamics** — floor, not tax.

[#7565]: https://github.com/tokuhirom/mutsu/issues/7565
