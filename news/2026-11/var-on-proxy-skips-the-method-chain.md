# `.VAR` on a Proxy skips the method-call chain

An unquoted, argument-less `.VAR` on a `Proxy` (or a subclass of it) is answered in the
pseudo-method lane of `CallMethod` / `CallMethodMut` (ADR-15829): the result is the same
decontainerized Proxy the generic path built, without the ~25-function probe chain in front.

Measured with callgrind on a profiling build, warm (slope over 100 -> 300 iterations of
`$p.VAR.defined` on a plain Proxy): 31.0k -> 21.2k Ir per iteration, so about 10k Ir less per
`.VAR` call. On AttrX::Mooish 1.0.10 (`scripts/call-path-ab.sh mooish-lazy`, whose
`AttrProxy is Proxy` makes 6 of its 7 per-iteration pseudo-method calls `.VAR` on a Proxy):
4.083M -> 4.027M Ir per iteration (-1.4%), which is what the ADR's upper bound for this kind
(~8%) turned out to be worth.

A Proxy subclass that declares its own `method VAR` no longer has it called, matching Rakudo,
which compiles `.VAR` itself (`class Sub is Proxy { method VAR { "mine" } }` answered `mine`
before and `1` under `raku`).
