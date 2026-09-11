# Role pseudo-types resolve in method parameters

`::?CLASS` and `::?ROLE` used as method parameter types inside a role now
resolve when the role is composed into a class. Both named parameters and
anonymous type-only parameters accept the appropriate consuming value and
reject unrelated values, matching Rakudo.

Fixes [#7985](https://github.com/tokuhirom/mutsu/issues/7985).
