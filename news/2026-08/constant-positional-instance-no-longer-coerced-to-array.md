# `constant @x = SomeClass.new` no longer collapses a custom Positional to a plain Array

```raku
class Bar does Positional { }
constant @iias9 = Bar.new;
say @iias9.^name;   # raku: Bar   mutsu (before this fix-forward): Array
```

A regression from the same-branch `array_container_writethrough_value`
fix (see `news/2026-08/closure-write-to-bound-typed-array-loses-element-type.md`):
the by-name (`SetGlobal`) assignment path's "preserve declared/inherited
element type" branch was made unconditional for every `@`-sigil name, but
`constant @x = ...` reaches the SAME opcode with its own `raw_mode`
coercion already applied above (List coercion, with an explicit
`does Positional` check that keeps a custom Positional instance as-is). The
now-unconditional branch ran again on top of that already-correct value,
and since a non-Array input falls through to a generic `coerce_to_array`
wrap, it discarded the custom class identity. Fixed by skipping the
writethrough/typed-coercion logic entirely when `raw_mode` (a `constant`
declaration) is set — a fresh declaration is never a write into an existing
container, so neither concern applies.

Caught by `roast/S04-declarations/constant-6.d.t` (whitelisted) regressing
in CI on the multi-param `for`-loop bind PR.
