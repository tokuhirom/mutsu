# samewith in plain subs, sprintf on slice containers, `use Foo:auth<…>` types

Three independent gaps found by the ecosystem roulette on Timezones::ZoneInfo:

- `samewith` in an ordinary (non-multi) sub died with "samewith called outside
  of a dispatch context" whenever the call took a light call path or the
  `state`-variable path, neither of which pushed the samewith context. Code
  that calls `samewith` is now flagged at compile time (`uses_samewith`) and
  kept off the light paths, and the `state` path pushes the context.
- `sprintf('%d', %h<a b>)` formatted each hash-slice element as `0`: the
  elements arrive as item containers and the numeric conversions did not
  look through them. `sprintf` now formats the contained value.
- `use Foo:auth<zef:x>` followed by a parameter typed with a class `Foo`
  exports failed with "Invalid typename": the compile-time pre-pass looked the
  module file up by the selector-decorated name. It now strips the selectors
  first.
