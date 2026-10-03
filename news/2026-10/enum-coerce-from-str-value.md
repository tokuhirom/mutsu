# Enum coercion from a Str matches by value

`ST("1")` now finds the variant whose value stringifies to `1`, as Rakudo does, and a variant
name is no longer a lookup key. Found with Monitor::Monit, whose `t/030-status.t` now passes.
