# Concurrent::Queue: `cas` on accessors, `while ... -> @t`, `[>>+<<]` over hashes

Taking `Concurrent::Queue`'s own test suite green exposed three general gaps:

- `cas($obj.attr, $expected, $new)` was an unknown function. An rw accessor is now a
  CAS target, swapped atomically under the instance's attribute-cell lock. The generic
  fallback comparison also uses `===` (identity) instead of `==`, so type objects no
  longer warn about numeric context.
- `while COND -> @t` assigned the condition into `@t`, so a `Failure` became a
  one-element array (always true) and the loop never ended. The condition is now tested
  through a scalar temporary and `@t`/`%t` are bound afterwards.
- `[>>+<<] %a, %b` used a private list-only hyper loop. The reduction layer now calls
  the same `hyper_op_pair` as the `>>op<<` operator, so hashes, nested lists and the
  non-dwimmy length error agree.
