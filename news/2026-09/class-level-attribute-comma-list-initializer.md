# A class-level `@`/`%` attribute initializer now behaves like a list assignment

`our @.x = 1, 2, 3` (and `my @.x = ...`) kept only the first element, and
`our @.x = (1, 2, 3)` stayed a bare `List` instead of coercing to an `Array`.
Both were specific to the class-level (`our`/`my`) attribute spelling — the
per-instance `has @.x = ...` spelling already got this right.

Two independent causes:

1. `try_dot_twigil_attr` parsed the initializer with `expression`, which
   stops at the first comma, so the rest of a bare comma list was silently
   dropped. An `@`/`%` sigil initializer now parses the whole comma list via
   `parse_comma_or_expr`, mirroring `has_decl`'s `=` branch.
2. `class_body_has_decl` stored the evaluated value as-is, skipping
   `coerce_attr_value_by_sigil` — the rule every other attribute-store path
   goes through, and the reason `has @.x = (1, 2, 3)` was already an
   `Array`. The class-level `=` branch now coerces before detaching its
   copy (the `:=` bind and the `=` copy semantics from #8150 are both
   preserved).

See [#8175](https://github.com/tokuhirom/mutsu/issues/8175) and the
regression test `t/oo/attribute/class-level-attribute-list-initializer.t`.
