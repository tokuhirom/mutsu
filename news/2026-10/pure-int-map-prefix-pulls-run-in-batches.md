# A pure Int map is pulled in growing batches

A `for` over a deferred `.map` pulls one element per iteration, and every pull paid the full
nested-register setup (about 1k instructions) plus the inline loop's environment entry.
`(1..300000).map({ $_ + 1 })` consumed by `for` took 0.44 s against Rakudo's 0.16 s.

When the callback is a total, side-effect-free Int expression (`$_ + 1`, `*+1`, `$_ %% 3`, ...)
over an immutable snapshot of plain Ints, a prefix pull now runs ahead by as many elements as the
Seq has already consumed (capped at 4096), so the setup is amortised and the first pull still maps
exactly what was asked (`.head(1)` of a long map costs one callback call). A live Array, a
container cell and any impure callback keep the per-element pull. A chained `.map.map` with a
pure upstream benefits too, since the downstream stage then finds a batch ready. See #14410.
