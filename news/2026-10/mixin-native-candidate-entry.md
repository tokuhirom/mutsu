# The builtin behind a role mixed into a native value is a deferral entry

`callsame`/`nextsame` out of a role's method on a `but`/`does`-mixed native value (`"abc" but Loud`, `%h does R`)
reaches the builtin on the inner value through a `DeferralEntry::Native` of the mixin's frame, not a probe after the
user candidates (ADR-11276 slice 4).
