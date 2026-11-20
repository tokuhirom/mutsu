# ADR-18356: The instance's attribute cell is the only store of an attribute; method frames hold no copy

- **Status**: Accepted (user-directed implementation, 2026-11-20)
- **Deciders**: tokuhirom, Claude
- **Context**: [#18356](https://github.com/tokuhirom/mutsu/issues/18356) (carrier and measurements),
  [#16897](https://github.com/tokuhirom/mutsu/issues/16897) (the Web budget that named attribute
  slots), [#17335](https://github.com/tokuhirom/mutsu/issues/17335) (AttrX::Mooish). Extends
  [ADR-0121](0121-instance-attributes-live-in-per-class-slots.md) D3 (per-site slot caches) to the
  forms it left by name, and finishes the "cell is the single source" direction that Phase 3
  Stage 2 started.

## 1. Context

An attribute has two homes in a method today:

- the instance's attribute cell (`InstanceAttrs`, laid out per class by ADR-0121), which is
  shared by every frame and thread holding the instance;
- a copy in the running method frame: a local slot seeded at entry for each `$!x`/`$.x` the
  body names (`LocalInit::Attr*`), an env entry under the twigil spelling for each `@!x`/`%!x`
  (which have no slot), and, for a `:=` bind, a `ContainerRef` in the frame that the exit has to
  find again.

Three mechanisms keep the two consistent:

- **reads** prefer the cell (`read_self_attr_cell_slot`);
- **writes** update the copy and then mirror it into the cell. Inert scalars go through the
  store lane; everything else goes through `mirror_attr_local_to_cell` or the by-name pair
  `attr_env_snapshot` / `mirror_attr_env_to_cell`;
- **binds** were recovered at exit by `reconcile_attrs`, which scanned the frame's slots and env
  overlay for `ContainerRef`s and matched them against every attribute key under seven
  spellings.

### Measured cost

Callgrind slopes on main@383187b6:

- An aggregate attribute pays two by-name probes plus the generic env container op on every
  method call or element op:
  - `@!x.push` costs 20.8k Ir against 5.9k for a local array;
  - `%!h<k> = 1` costs 30.0k against 5.0k;
  - `@!x.elems` costs 13.1k against 2.8k.
- `reconcile_attrs` is 2.6% of an AttrX::Mooish t/090 round.
- The by-name snapshot and mirror run on every `CallMethodMut`, attribute target or not. They
  are ~1.3% of MCP::Server::Tool::Web `html-to-text`.

The copies are also the lost-update hazard that `reconcile_attrs`' own documentation describes:
a whole-map commit from a stale frame copy could clobber a concurrent cell write.

## 2. Decision

**The instance's attribute cell is the only store of an attribute.** A method frame holds no
copy of an attribute that anything must reconcile, mirror or write back:

- **D1. Binds seat at bind time.** Every `:=` whose target is an attribute — `$!x`, `@!x`,
  `%!x`, the public forms, and the twigil-less `has $x` alias — writes the bound container (or
  value) into the instance when it runs. The method exit does no attribute work. This retires
  `reconcile_attrs` and its frame-ownership helper (`vm_reconcile_bare_reading.rs`).
- **D2. Aggregate attributes are addressed through the attribute site, not by name.** A
  mutating op on `@!x`/`%!x` mutates the container the cell holds. A whole-value store writes
  the cell. ADR-0121 D3's `(layout id, slot)` site cache resolves the slot. A miss — a role
  owner, a mixin, an `extra` (undeclared) attribute, or a different layout — takes the general
  resolution (`with_self_attr`), whose answer the cache only ever replays. The by-name snapshot
  and mirror (`attr_env_snapshot*`, `mirror_attr_*env_to_cell*`) go.
- **D3. No entry seeding.** Method frames stop seeding attribute slots from the cell. An
  attribute read is a cell read through its site.

Layout changes need no new guard:

- `augment` and a `^add_attribute` before composition give the class a new layout id;
- a mixin (`but`/`does`) changes the receiver's layout;
- an attribute added to one instance lands in `extra`.

Each of these misses the site cache. If a future case needs a process-wide invalidation, it
takes a bit in the narrow-op latch set of
[ADR-18357](18357-latch-guarded-narrow-ops-for-native-int-and-plain-str.md), not a parallel flag.

## 3. Consequences

- One write path per attribute. A nested call, another thread and the running frame all see
  the same storage at once, and there is no window where a frame copy differs from the
  instance.
- The method exit stops scanning the frame for every method. That is a gain proportional to
  method calls, not to attribute use.
- A frame-local alias of an attribute — the twigil-less `has $x` name, a closure capture of
  `$!x` — must resolve to the cell, never to a snapshot. Sites that captured an attribute by
  value become bugs under this ADR. They are fixed by reading through the site, never by
  re-adding a writeback.

## 4. Rejected alternatives

- **Skip `reconcile_attrs` by a static per-method proof that the body makes no attribute
  bind.** It keeps the frame copy and its mirror, and with them the lost-update hazard. It also
  needs an analysis that sees binds made by nested closures and by `EVAL`. The cell-only rule
  needs no analysis.
- **Keep aggregates by name and make the env probes cheaper.** That is the "cache inside each
  layer" shape the perf-tuning skill warns against: the by-name path is three layers per op,
  and the attribute site already has the answer.
