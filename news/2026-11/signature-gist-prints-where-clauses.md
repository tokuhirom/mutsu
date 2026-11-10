# `Signature.gist` / `.raku` print a parameter's `where` clause

`:($ where { $_ > 1 }).gist` was `($)`; rakudo prints `($ where { ... })` (the constraint's
source is never shown, whatever its form). `render_param` now emits ` where { ... }` after the
traits and before the default, for positional, named, optional and sigilless parameters.
Test::Mock's mismatch messages (`called shave 2 times with arguments matching :($ where { ... })`)
quote such signatures, so its `t/computing`, `t/returning` and `t/with` now match raku, and with
them all ten of its test files.

`.assuming` also kept dropping the `where` clause of the parameter it primed
(`pd.where_constraint = None`), which the missing printing had hidden; rakudo keeps it
(`(:$a where { ... } = 10, :$b)`), and `roast/S06-currying/named.t` expects it. The binding check
was never affected, only the primed signature's text.
