# A type capture's smiley constrains the parameter

rakudo reads `sub f(::T:D $x)` as `::T Any:D $x`: the capture still takes the
argument's type, and the smiley makes the parameter refuse a type object
(`::T:U` refuses an instance). mutsu's parser consumed the smiley after the
capture name and dropped it, so `f(Int)` bound (#19020).

The parameter now gets the smiley as an `Any:D` / `Any:U` constraint when it
has no other type. `&f.signature.raku` reads `:(::T Any:D $x)`, as on rakudo.
The RakuAST view's `Type::Capture` smiley waits on #18978.
