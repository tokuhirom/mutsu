# ADR-18443: A live Supply combinator stage is instantiated per tap, not registered when the method runs

- **Status**: Proposed

Related: [ADR-0008](0008-push-based-supply-event-delivery.md) (push delivery through
supplier subscriptions), [ADR-0031](0031-supply-quit-ownership-and-cold-source-tapping.md)
(quit ownership; cold sources are tapped, not replayed),
[ADR-0028](0028-supply-schedule-on-deferred-tap-delivery.md) (the `"tap" | "act"`
chokepoint). Issue: #18443 (Red 0.2.16 `t/190-resultseq-supply` tests 33, 34, 40-43).

## Context

In rakudo a `Supply` is a recipe: it wraps a `Tappable`, and every combinator
(`map`, `grep`, `head`, `unique`, `batch`, ...) returns a new recipe over the old
one. Nothing subscribes until `.tap` is called, and each `.tap` instantiates the
whole chain for that one subscriber. Closing the `Tap` — or the chain completing,
as `head(1)` does after its first value — closes every upstream subscription that
tap made. `share` is the one explicit point where a chain is instantiated once and
broadcast.

mutsu's live (Supplier-backed) combinators do the opposite. `Supply.map` on a
supply carrying a `supplier_id` calls `make_live_transform_supply`
(`src/runtime/methods_supply_dispatch.rs`), which **at method-call time** allocates
a downstream supplier and registers a transform tap on the source
(`register_supplier_transform_tap`, `src/runtime/native_methods/state_supplier.rs`);
the derived Supply is just that downstream supplier. `head`, `tail`, `lines`,
`words`, `elems`, `unique`, `produce`, `reduce`, `batch`, `flat`, `classify`,
`start`, `zip` and `zip-latest` do the same through their own `register_supplier_*`
functions (about 30 call sites in `native_supply_dispatch.rs`,
`native_methods/mod.rs`, `supply_classify.rs`). Consequences, all observable:

```raku
my $n = 0;
my $s = Supplier.new;
my $m = $s.Supply.map({ $n++; $_ });
$s.emit(1);                       # raku: $n == 0 (no tap) — mutsu: 1
$m.tap(-> $ {}); $m.tap(-> $ {});
$s.emit(1);                       # raku: +2 (one chain per tap) — mutsu: +1
my $t = $s.Supply.map({ $n++ }).tap(-> $ {}); $t.close;
$s.emit(1);                       # raku: map not run — mutsu: still runs
$s.Supply.map({ $n++ }).head(1).tap(-> $ {});
$s.emit(1); $s.emit(2);           # raku: map ran once — mutsu: twice
```

`Tap.close` (`tap_close` → `close_supplier_tap`) only marks the leaf subscription
closed; the stage taps registered by the combinator calls live on the source for
the rest of the process. That is also a leak: a long-running program that builds
`$live.map(...)` per request accumulates transform taps that never go away.

Red's live ResultSeq supply (`Red::ResultSeq::Supply`) counts row
materializations through exactly this machinery (`.map(...).share` per model, then
`.map(-> &resolve { resolve() })` per tap), and its 0.2.16 tests assert that a
closed or completed tap stops materializing and that simultaneous taps share one
materialization through `share`.

## Decision

1. **A live combinator returns a recipe, not a subscription.** The derived
   Supply records its source Supply and a stage descriptor (the kind and its
   arguments: the callable, the count, the `:with`, ...). Calling the method
   registers nothing and allocates no supplier.
2. **`tap` instantiates the chain for that subscriber.** Tapping a recipe
   instantiates its source first (recursively, down to a Supplier-backed,
   on-demand or channel source), allocates a fresh downstream supplier for each
   stage, registers the stage's tap on the stage below, and registers the user's
   tap on the top. Every subscription made this way is recorded in the returned
   `Tap`'s `upstream_taps`, the list `tap_close` already cascades through.
3. **Completion releases the chain.** When a stage completes on its own
   (`head` reaching its count, a `done` downstream, a quit), the subscriptions
   below it are closed exactly as a `Tap.close` would close them.
4. **`share` (and `Supplier.Supply` itself) stay the eager points.** `share`
   instantiates its source chain once, at `share` time, as rakudo does, and its
   taps attach to the shared output; that is the only way two taps observe one
   instance of a stage.
5. **One instantiation routine.** All live combinators go through one
   "instantiate stage" routine keyed by the stage descriptor; the per-kind
   `register_supplier_*` functions become what that routine calls, not what the
   combinator methods call. A `whenever` over a recipe and a `react` over one use
   the same routine (today they read the source's `supplier_id` directly).

## Consequences

- Each tap of a non-shared derived supply runs the stage callables once per tap
  per value, as in rakudo; side effects of `map`/`grep`/`do` change count for
  programs that tapped one derived supply several times (they now match rakudo).
- A stage with no tap does no work and holds no subscription.
- Code that inspects a derived supply's `supplier_id` before it is tapped (the
  `whenever`-marker loop in `native_supply_mut_methods.rs`, `supply_get_values`,
  `Supply.Channel`, `Supply.Promise`, the `react` source classification) must ask
  the instantiation routine instead. That is the bulk of the work and the main
  risk: every branch that today recognizes a live source by `supplier_id` has to
  recognize a recipe too.
- Materialized (non-live) supplies are unaffected: their combinators already
  compute eagerly over `values`.

## Alternatives considered

- **Keep eager registration, add reference counts** (a stage tap is dormant while
  its downstream has no open tap, activated on the first tap, closed on the last).
  Fixes "untapped map runs" and "closed tap keeps running", but not "two taps on
  one map run it once", and it keeps two lifecycles (method call and tap) for one
  object. Rejected as a second mechanism next to the one rakudo has.
- **Only cascade close/complete through the existing eager stages.** Smallest
  change, fixes the Red tests that close or complete taps, but leaves the leak for
  untapped stages and the shared-callable divergence. Acceptable only as an interim
  slice if the full change is not accepted.

## Open questions

- Whether `Supply.on-demand` / `supply { }` sources need any change: they are
  already instantiated per tap (the body runs per tap), so a recipe over one
  should only need the generic routine.
- The order of slices: `map`/`grep`/`do` and `head` first (the Red tests), then the
  buffering stages (`batch`, `tail`, `reduce`, `produce`), then `zip`/`classify`.
