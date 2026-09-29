# Commands distribution: punned roles honour `is built(False)`, pair-block statements end at the brace, `Mu.say(x)` prints through `print`

Working the `Commands` ecosystem distribution (`t/01-basic.rakutest`, 38 assertions, previously dying at construction) exposed three interpreter gaps:

- A role instantiated directly (`Role.new(...)`) built its pun class with an empty `attribute_built` map, so an `is built(False)` attribute was filled from a same-named constructor argument. `ensure_role_punned_to_class` now carries the role's (and its parent roles') `attribute_built` onto the pun.
- A statement whose last term is a pair with a block value (`@a.push: $key => { ... }`) now ends at that block's `}` at end of line, so the next line's `if COND -> $x { ... }` is a statement rather than a modifier that silently swallowed the pointy block.
- `$obj.say(...)` / `$obj.put(...)` with arguments, on a class that has its own `print` but no `say`/`put`, now follows Rakudo's `Mu.say(\x)`/`Mu.say(|)` fallback: gist (or `Str`) of the arguments plus `nl-out`, passed to that `print`.

Regression tests: `t/oo/role/punned-role-honours-is-built-false.t`, `t/lang/statement-ends-at-pair-block-value.t`, `t/oo/class/mu-say-put-with-args-use-own-print.t`.
