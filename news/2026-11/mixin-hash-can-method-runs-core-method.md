# A core method object saved before `does` keeps running the core method

`my $at = %h.can('AT-KEY').head; %h does R` (where `R` overrides `AT-KEY`) made
`$at(%h, 'q')` run `R`'s override, not the Hash method it names; a wrapper that calls
the saved original from inside the override therefore recursed until the stack ran
out. The compiled call path now runs such a method object on the mixin's native base,
as `call_sub_value` already did. Hash::Util's `lock_keys`/`lock_value` tests stop
overflowing (`lock_keys.t` 0 to 16 of 34, `lock_value.t` 0 to 3 of 17); the remaining
failures are a separate gap (`h does R` inside a routine does not reach the caller's
`%hash` container) filed as its own issue.
