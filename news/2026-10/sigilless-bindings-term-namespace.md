# Sigilless bindings live in the term namespace

A sigilless binding (`my \x`, a `\x` / `+x` / `|x` parameter, a pointy `-> \x`) is now stored
under its term key `\x`, the key sigil-less constants already used (#9962). A same-named `$x` is
therefore a different variable everywhere: `sub f(\t, $t) { say t, $t }; f(1, 2)` prints `12`,
`-> $t { ... }` inside a `\t` scope keeps both, and `for 1 -> \t { my $t = 5; say $t + t }`
prints `6` (all three printed the scalar twice before).

The decision is [ADR-12441](../../docs/adr/12441-sigilless-bindings-live-in-the-term-namespace.md).
It retires the declaration-side workaround that copied (#12434) or bound (#13540) a shadowed
sigilless value into the term key whenever a `my $t` was compiled while `\t` was visible, along
with `sigilless_declared`, `CompiledCode::shadowed_sigilless_reads` and the forced capture of an
outer `$t` past a `\t` parameter. The private `__mutsu_sigilless_underscore` key for `my \_` is
gone too: `\_` is simply the term `_`.
