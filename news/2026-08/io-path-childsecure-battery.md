# IO::Path::ChildSecure bundled as the secure-path-join battery

`IO::Path::ChildSecure` (`zef:raku-community-modules`, v1.2, Artistic-2.0) is
vendored at `modules/IO-Path-ChildSecure/` and resolves with zero config. Both
upstream test files (11 subtests) pass, matching raku. It is a hard dependency
of Cro::HTTP (docroot escape protection in the static-file router) — the
second Cro::HTTP dependency locked in behind the release gate, after
`Crypt::Random`.

One general interpreter fix got it there; the vendored source is untouched:

- **`X::IO::Resolve` and `X::IO::NotAChild` are registered exception
  types.** The module constructs them from user code
  (`X::IO::NotAChild.new: :path(...), :child(...)`), which previously died
  with `X::Method::NotFound` because only mutsu-internal code paths (the
  native `IO::Path.child(:secure)`) knew the names. They are now registered
  like the other `X::*` types and carry the rakudo message texts
  (`Failed to completely resolve "..."` / `Path "..." is not a child of
  path "..."`). Pin: `t/x-io-resolve-notachild.t`.

Packaging: `batteries.lock` row + both files whitelisted in the release gate,
`t/io-path-childsecure-battery.t` smoke test, the selection record
`docs/batteries/io-path-childsecure.md`, the BATTERIES.md §7 index row, and a
site row via `scripts/gen-batteries-manifest.py`.
