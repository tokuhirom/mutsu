# A user subset named like a native width shadows the native default

`subset int8 of Int where ...; my int8 $c;` now leaves `$c` as the `int8`
subset's type object (`.defined` is False, `.^name` is `int8`) instead of
initializing it as the native `int8` with value 0. The parser, the compiler's
native-default seeding and the VM's Nil seeding all defer to a registered user
subset of that name (#12359).
