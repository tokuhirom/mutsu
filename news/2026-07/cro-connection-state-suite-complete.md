# Cro::Core composer 134/134, connection-state 11/11, policy 6/6 — start-thread self leak, live pipeline done-groups, Supplier::Preserving backlog

Three Cro::Core test files went fully green in one slice (vendored Cro::Core,
`-I lib`): `composer.rakutest` **134/134** (was 133/134 — the "thread-side
attribute reads see a hollow instance" ticket this file replaces),
`connection-state.rakutest` **11/11** (was 4/11 then death on a closed
channel), and `policy.rakutest` **6/6** (was 0, aborting on `Real`). Each fix
is a general interpreter repair, pinned by new `t/` tests and raku-verified.

## `self` leaked across `start` blocks via the shared-var store (composer 134)

`clone_for_thread` seeded every env key into the name-keyed shared store,
including `self` and the `?`-pseudo-lexicals (`?CLASS`, `?ROLE`). A `start`
block running a method body would flush its `self` into the store; when a
*different* object's `start` block later executed an `await`, the resume-path
`sync_shared_vars_to_env` pulled the stale `self` back in — swapping the
invocant mid-block. `Cro::CompositeConnector.connect`'s `start` block read
`@!before`/`@!after` off the *inner connector's* instance (scalar
`$!connector` was read before the `await`, which is why it "survived").
Fixed by excluding `self` and all `?`-prefixed keys from thread seeding and
from the dirty-key sync (both directions). Pin: `t/start-self-not-shared.t`.

## Chained on-demand supplies fired a spurious `done` (connection-state)

A supply block whose only `whenever` wraps *another on-demand supply*
(`supply { whenever $t1 {...} }` over `$t1 = supply { whenever $in.Supply
{...} }`) counted zero supplier-backed sources, so the tap path treated it as
finite and fired `done` immediately — while the chained pipeline stayed live
underneath. Three related repairs:

- on-demand whenever sources now join the **done group** (the group marker is
  passed as the chained inner tap's `done`, bundled with LAST phasers via a
  new `__SupplyDoneChain` marker; done-callback dispatch now goes through
  `invoke_done_callback` consistently so markers nest);
- a `whenever` registered **at dispatch time inside another whenever's body**
  (the `Cro::Connector.establish` shape: `whenever $connection { whenever
  $transform.transformer($incoming) {...} }`) also joins the enclosing group —
  the body callback learns its group through an env key injected at tap
  registration, and `run_whenever_with_value` increments the group and wires
  the decrement into the nested tap's done (promise sources decrement on
  resolve);
- `.Channel` on an on-demand supply now TAPS the pipeline and wires the
  supply-block emitter to the channel (previously it snapshot-drained and
  immediately closed the channel — the "Cannot receive on a closed channel"
  failure).

Pin: `t/supply-chained-live-pipeline.t`.

## `Supplier::Preserving` did not preserve (connection-state)

Values emitted while no tap listened were simply dropped for live supplies;
`Supplier::Preserving` behaved identically to `Supplier`. The supplier's
global state now keeps a `preserved_consumed` watermark: emissions delivered
to a live tap advance it, and the next tap (direct `.tap` or a supply-block
`whenever` subscription) replays the backlog exactly once. The
`preserving` flag is set at construction and propagated through `.Supply`.
Pin: `t/supplier-preserving-backlog.t`.

## `Real(...)` / `Numeric(...)` coercion calls (policy 6/6)

`Real($x)` and `Numeric($x)` were "Unknown function"; they now delegate to
the `.Real` / `.Numeric` method form like `Rat(...)`/`Complex(...)` already
did. `Cro::Policy::Timeout`'s BUILD uses `Real($default)` on every phase
default. Pin: `t/coerce-real-numeric.t`.

## Remaining in the Cro::Core suite

- `connection-conditional.rakutest` 8/13 — all five failures are the
  `state`-vars-per-clone problem (`todo/deep/state-vars-per-clone-named-subs.md`).
- `tcp.rakutest` — `:nodelay` subtests blocked by
  `todo/tickets/in-memory-socket-native-descriptor.md` (in-memory sockets
  have no fd by design).
- `uri.rakutest` — large, untouched.
