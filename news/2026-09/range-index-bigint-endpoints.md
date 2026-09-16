# Range indexing falls back correctly for BigInt and fractional endpoints

`($start..$end)[$n]` and its slice/list-index siblings computed their fast
path by converting each endpoint to `i64` with `start.to_f64() as i64`. For
a `GenericRange` whose endpoints are `BigInt`s beyond `i64::MAX` — as
`Net::Netmask`'s `$!start`/`$!end` are for an IPv6 `/128` netmask — the
`f64` cast silently saturates to `i64::MAX`/`i64::MIN` instead of erroring
or falling back, so every index answered a value derived from the wrong,
saturated bound. The same conversion also silently truncated a fractional
`Num`/`Rat` endpoint (`(1.5..5.5)[0]` answered `1` instead of `1.5`),
losing the fractional part entirely.

`range_params`'s `GenericRange` arm now extracts each endpoint's *exact*
`i64` representation — `Int` and in-range `BigInt` convert directly, a
whole-numbered `Num`/`Rat` converts via its integer value, and anything
else (an out-of-range `BigInt`, a fractional `Num`/`Rat`, `Inf`/`NaN`)
returns `None`. Every call site already had a `value_to_list`-based
fallback for the `None` case (used previously only for string/degenerate
ranges), which correctly expands `BigInt` and fractional ranges via
`.succ` stepping, so no new fallback logic was needed — the fast path just
had to stop claiming ranges it could not represent exactly.

Pinned by `t/collections/range-pair/range-bigint-index.t`.

A related bug in `range_elems_f64` (used by WhateverCode range indexing,
e.g. `range[*-1]`) also loses precision for BigInt-scale bounds via the
same kind of `f64` rounding, but on the *count* rather than a single
element; that is tracked separately as #8591.

Closes #8588.
