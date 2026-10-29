# A role-mixed Array binds to `@` params, `-> $t is copy` keeps the aggregate, `.WHICH` names List and mixins

Three gaps that Data::Dump::Tree's `t/04_flat.rakutest` and `t/01_all.rakutest` hit:

- `[1,2,3] but R` held in a `$` variable and passed to `sub f(@c)` was one item to
  `for @c -> $x, $y, $z` (`Too few positionals passed`). The `@` parameter bind now
  decontainerizes a role mixin over an itemized Array like it does a plain one
  (`deitemize_for_sigil_bind` handles `Mixin`).
- `for ... -> $t is copy` copied the Array/Hash an item held, so `$t === $outer`
  was False. Only `@`/`%` copy parameters own a fresh aggregate; a `$` one copies the
  Scalar, as in rakudo.
- `(1, 2).WHICH` answered `Array|N` and a `but`-mixed object `Any|N`. It is now
  `List|N` and `Class+{Role}|N`, stable across reads.

Data::Dump::Tree: `t/04_flat` 2/23 -> 23/23, and `t/01_all` dumps the same 81 lines as
rakudo (its remaining difference, the `+{Whatnot}` attribute origin marker, is #15417).
