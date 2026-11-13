# Subs that read a top-level `my` take the light call paths

A named sub that reads or writes a mainline lexical (`my $count; sub bump { $count++ }`,
ADR-0024) was excluded from every light, positional-light, fast and multi-light
call path and always took the general named-call path. The exclusion dated from
when those paths pushed no routine frame for ADR-0024's frame predicate to see;
ADR-0037 Slice 1 made all of them push one with the routine's name and code
address, so the exclusion only cost speed. It is retired (ADR-0024 amendment).

callgrind, per call: `bump()` (`$count++`) 21.3k → 11.9k instructions,
`add(2)` (`$count += $n`) 31.4k → 13.8k, `get('a')` (`%cache{$k} // 0`)
30.5k → 20.6k.
