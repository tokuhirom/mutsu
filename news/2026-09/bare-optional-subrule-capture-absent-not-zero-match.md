# Bare optional subrule captures are absent, not a spurious zero-width match

A named capture attached directly to a bare subrule-call atom
(`$<x>=<.subrule>?`, `$<x>=<subrule>?`, including zero-arg builtins like
`<lower>`) that took the `?` quantifier's zero branch used to materialize a
defined, empty (zero-width) `Match` for `$<x>`. Raku instead leaves the name
entirely absent: `$<x>` is `Nil` and the key is missing from `.caps`.

mutsu's regex engine had one rule for every atom kind quantified with a bare
`?` and a name attached directly to it: `$<x>=[...]?` / `$<x>=<[cd]>?` (a
group or a character class) always "ran" as a unit and rendered as an empty
`Match` even on the zero branch, while `$<x>=(...)?` (a capturing group) was
already known to differ and yield `Nil`. A bare subrule call turned out to be
a *third* case, sharing the capturing-group's `Nil` behavior rather than the
character-class one, even though both are written `$<x>=<...>?` — confirmed
against `raku` for user-defined subrules, zero-arg builtins like `<lower>`,
and the same subrule wrapped in a group (`$<x>=[<.subrule>]?`), which is
unaffected and still yields an empty `Match`.

This produced a spurious extra ordered capture in `CSS::Grammar::CSS3`'s
`AnB-expr` rule: the pattern
`$<op>=<.op-sign>? $<int>=<.uint>? $<op>=<.op-n> ...` built an extra
`{ :int(0) }` entry before the `n` operator for inputs like `-n+2`, because
the absent `$<int>=<.uint>?` capture was showing up in `.caps` at all instead
of being skipped.

The fix narrows the atom kinds eligible for the zero-width-Match rendering:
a bare `Named` atom (a subrule/rule/token call) is now excluded alongside
`CaptureGroup`, so both leave the name absent on the zero branch.

See [issue #9054](https://github.com/tokuhirom/mutsu/issues/9054).
