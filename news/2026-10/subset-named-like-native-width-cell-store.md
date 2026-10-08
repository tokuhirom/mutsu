# A subset named like a native width is type-checked on aliased stores

Stores through a container cell (`for $x, 1 -> \a, $v { a = $v }`) narrowed the value to the
native width before type-checking it, so a user `subset int8 of Int where -128 <= $_ <= 127`
silently wrapped `1000` to `-24` instead of throwing `X::TypeCheck::Assignment`. The cell store
now checks first and wraps afterwards. `Native::Overflow`'s `t/01-basic.rakutest` passes 30/30.
