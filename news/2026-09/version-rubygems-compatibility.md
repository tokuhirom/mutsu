# Version::RubyGems compatibility fixes

`Version::RubyGems` 0.0.1 now runs its two upstream test files under mutsu.
The fixes cover custom `EXPORT` operator candidate families, comparison and
method dispatch, pointy blocks installed through `.^add_method`, inherited
type-object representations, `is List` attributes passed through `bless`, and
once-only evaluation/writeback for indexed `with` topics.

The distribution's source remains unchanged; the interpreter now preserves
the callable and container semantics those upstream modules rely on.

Pinned by the focused regressions added under `t/modules/import-export/`,
`t/oo/`, and `t/control/`, and by the held `Version::RubyGems` ecosystem
record.
