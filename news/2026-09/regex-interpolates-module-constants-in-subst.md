# Regex patterns see module-scope constants in `s///`

A sub inside a module that interpolated a constant into an `s///` pattern
(`s:g/ $WS ** 2..* /$WS/` with `$WS` imported via `use Vars`, or declared with
`our constant` in the same module) matched nothing, because the substitution
pattern is resolved from the running frame's env at match time and the
constant lives only in the package / module-scope tables. The bare `$name`
arm of regex scalar interpolation now uses the same package-chain fallback the
`${name}` and `@name` arms already had, plus the module-scope lexical table as
a last resort. Found via the Text::Utils ecosystem distribution
(`t/14-normalize-string.t`).
