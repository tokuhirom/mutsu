# `.^can` of an `^add_method`-installed sub keeps its own signature

A sub installed with `Class.^add_method('name', sub ($a, $b = 5) {...})` already carries its
invocant as the first positional. `Class.^can('name')[0]` used to prepend a second synthetic
invocant (arity 2 instead of 1, so `$m.($obj)` died with "Too few positionals"), and a declared
(named) sub came back as a callable with no body that silently returned `Nil`. Both now match
rakudo. Found through Humming-Bird's `t/opt/plugin-dbiish.rakutest`.
