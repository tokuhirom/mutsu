When an existing `Array` containing an explicit `Any` element is updated by
element or slice assignment, converting it with `.List` now preserves that
`Any` element instead of rendering it as `Nil`. Hole tracking now retains the
already-present range when an untracked array receives its first element
assignment, while still distinguishing genuinely autovivified gaps.

Closes #8571.
