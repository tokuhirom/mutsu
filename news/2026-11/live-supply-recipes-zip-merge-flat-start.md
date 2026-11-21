# Live `merge`, `zip`, `zip-latest`, `flat`, `start` and `classify` are per-tap recipes

ADR-18443 slice 3a. Over a live source (or a recipe built on one),
`Supply.merge`, `.zip`, `.zip-latest`, `.flat`, `.start`, `.classify` and
`.categorize` -- as methods and
as the `Supply.merge(...)` / `Supply.zip(...)` / `Supply.zip-latest(...)`
class methods -- no longer register stages on their sources when called. They
return recipes that each `tap` instantiates with its own buffers, and closing
the tap releases every source.

Completion now follows rakudo: `merge` and `zip-latest` are done when every
source is, and `zip` as soon as a source is done with nothing left buffered.

Fixes on the way:

- The class methods dropped a live recipe argument (`Supply.merge($s.Supply.map(...),
  ...)` lost the mapped values), and `Supply.zip` over two plain live sources
  emitted nothing at all.
- The internal forwarders passed a value on through a `$v` parameter, so a
  List coming out of a stage arrived itemized (`$(1, 10)`) and a `flat` below
  it did not flatten it. They now pass it on decontainerized.

Each tap of a `classify` / `categorize` now has its own groups (each still
backed by a preserving supplier), and the groups are done or quit with the
source, before the classified supply itself.

The per-tap state of the combined supplies is in
`native_methods/supply_derive_multi.rs`, the groups in
`native_methods/supply_derive_classify.rs`.
