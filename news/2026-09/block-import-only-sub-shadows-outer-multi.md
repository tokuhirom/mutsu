# A block-scoped import of an `only` sub now hides an outer multi

`{ use Green :harness; ok 1 == 1 }` imports Green's `only sub ok` into that
block. Raku then calls Green's `ok` inside the block, because the imported
routine shadows the `multi sub ok` that `use Test` put in the outer scope.
mutsu used to merge the imported routine next to Test's candidates, and
dispatch picked Test's `ok`, so Green's `t/03-more_concise.t` emitted two TAP
lines against a plan of 1.

`import_module` already hid an outer candidate family when a *proto* was
imported into a nested import scope. It now does the same for an imported
`only` sub, and restores the outer family when the block exits, just as it does
for a proto. Pinned by
`t/modules/import-export/block-import-only-sub-shadows-outer-multi.t`.

The same distribution exposed two larger gaps, filed as issues rather than
patched:

- #10049: a caller's typed `my $x` constraint leaks through the name-keyed
  `__mutsu_type::` env lane into a called routine's write to its own outer
  `$x`. This is Green's `t/02-concise.t`.
- #10050: `is export` on a `my sub` nested inside another routine's body is
  never exported. This is Green's `t/01-time.t`.
