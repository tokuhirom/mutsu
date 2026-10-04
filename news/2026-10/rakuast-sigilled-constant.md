# RakuAST: sigilled constants

"sigilled constant" was one of the most common `.AST` refusals left, with 36
`t/` files: `constant @a = 1, 2`, `constant %h = …`, `constant $x = …` and
`constant &f = …`.

Measured on rakudo 2026.09, these are the same `VarDeclaration::Constant` as
a sigilless `constant X`. The only difference is that `name` carries the
sigil: `"@a"`, `"%h"`, `"$x"`, `"&f"`.

mutsu's parser stores the name inconsistently across sigils. It keeps `@`,
`%` and `&` in the name and moves a `$` into its `__constant_sigil` trait,
which records every constant's sigil. The converter now builds rakudo's
sigilled name from those two parts. The lowering splits it back the same way,
so the round trip yields the parser's own declaration again.

One spelling reuses the same storage. In `constant term:<$bar> = 42`, the
parser records the name `$bar` with no sigil of its own. Rakudo names it
`term:<$bar>`, and the converter now renders it that way so the lowering can
tell the two apart. Splitting `$bar` as a `$` constant broke the existing
`t/lang/constant-sigiled-term.t` under `MUTSU_RAKUAST=1`, and the ratchet
caught it.

Typed constants (`constant Int $x = …`) stay refused. They carry a type
that this slice does not model.
