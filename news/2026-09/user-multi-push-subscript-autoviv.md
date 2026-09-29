# `push(@a[2], 1)` autovivifies again when a user `multi push` is in scope

With a user `multi push` in scope, `push(@a[2], 1)` silently did nothing instead of
falling back to the core `push`. The compiler now vivifies an undefined subscript slot to
an empty array before routine dispatch, so the core candidate pushes into it, while a
defined slot still reaches user candidates. ADR-0044 §8 is updated (#9904).
