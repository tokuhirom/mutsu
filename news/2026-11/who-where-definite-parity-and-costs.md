# `.WHO` / `.WHERE` / `.DEFINITE` are pinned against `raku` and measured (ADR-15829)

ADR-15829 held `.WHO`, `.WHERE` and `.DEFINITE` back from the pseudo-method lane until
`raku`-checked parity tests existed. They exist now
(`t/oo/method/pseudo-method-who-where-definite-parity.t`, 16 tests, identical under `raku`:
the three answer through the macro, never through a user method of the same name).

Measured on an Int receiver (callgrind slope between 100 and 300 iterations, profiling build,
warm): `.DEFINITE` 2.0k Ir per call, `.WHERE` 7.2k, `.WHAT` 2.7k, `.HOW` 3.9k; `.WHO` 0.45M
since the stash-member fix, because it builds the whole stash (O(registry), #9171). None of
`.DEFINITE` / `.WHERE` is expensive enough to earn a lane, so the ADR's lane list stops at
`.WHAT` / `.HOW` / `.VAR`; `.WHO` waits for a persistent stash, not a shortcut.
