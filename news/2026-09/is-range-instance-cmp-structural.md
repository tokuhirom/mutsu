# `cmp` between a Range and an `is Range` instance now compares structurally

Rakudo lets a class inherit from `Range` (`class Interval is Range { has Range
$!range is built handles <min max bounds ...> }`, the shape `Math::Interval`
uses for its arithmetic operators), and `cmp` between such an instance and a
plain `Range` — or two such instances — compares them structurally: by
min, excludes-min, max, then excludes-max, exactly like two native `Range`
values. mutsu instead fell back to a generic type/identity-based ordering,
so `(Interval.bless(range => 3..6) cmp (3..6))` answered `More` instead of
`Same`.

`range_cmp`'s ordering logic is now factored into `range_cmp_parts`, which
takes `(min, max, excludes-min, excludes-max)` directly instead of reading
them off a native `Range` variant. `cmp` now checks whether either side is
an `is Range` instance (walking the MRO); if so it reads that side's four
structural fields via method dispatch (`min`/`max`/`excludes-min`/
`excludes-max`), so a delegated or inherited accessor participates just
like a native Range's own fields, and compares both sides with the same
rules a Range-vs-Range comparison already used.

This was found while verifying the `Math::Interval` distribution's test
suite (locked on the ecosystem board, #7884) against mutsu, as a follow-up
to #8807/#8812: with those fixed, `Math::Interval`'s `t/01-rop.rakutest`
and `t/02-iop.rakutest` stopped crashing but still failed most assertions,
all of which use `cmp ... ~~ Same` to check an overload's result against an
expected plain `Range`.

Pinned by `t/oo/class/is-range-inherited-cmp-structural.t`.

Closes [#8814](https://github.com/tokuhirom/mutsu/issues/8814).
