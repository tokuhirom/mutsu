use Test;

plan 10;

# A `where` refinement on one parameter and a nominal type on another are
# incomparable: the first declared candidate that binds wins (#11943).
{
    multi g(Str:D $p where { $_ eq 'a' }, $v) { "where-any" }
    multi g(Str:D $p, Str() $v) { "coerce" }
    is g('a', '5'), 'where-any', 'where candidate declared first wins';
}
{
    multi g(Str:D $p, Str() $v) { "coerce" }
    multi g(Str:D $p where { $_ eq 'a' }, $v) { "where-any" }
    is g('a', '5'), 'coerce', 'coercion candidate declared first wins';
    is g('b', '5'), 'coerce', 'where fails: coercion candidate';
}
{
    multi h(Str:D $p where { $_ eq 'a' }, $v) { "where-any" }
    multi h(Str:D $p, Str $v) { "str" }
    is h('a', '5'), 'where-any', 'plain Str second param, where first';
}
{
    multi h(Str:D $p, Str $v) { "str" }
    multi h(Str:D $p where { $_ eq 'a' }, $v) { "where-any" }
    is h('a', '5'), 'str', 'plain Str second param, Str first';
}
{
    multi k($x where * > 0, Int $y) { "where" }
    multi k(Int $x, $y) { "int" }
    is k(1, 2), 'where', 'incomparable pair, where first';
}
{
    multi k(Int $x, $y) { "int" }
    multi k($x where * > 0, Int $y) { "where" }
    is k(1, 2), 'int', 'incomparable pair, nominal first';
}

# A single parameter stays comparable: the nominal type outranks a bare where.
{
    multi f($x where * > 0) { "where" }
    multi f(Int $x) { "int" }
    is f(42), 'int', 'nominal Int beats bare where';
}
{
    multi f(Int $x where * > 0) { "refined" }
    multi f(Int $x) { "int" }
    is f(42), 'refined', 'refined Int beats plain Int';
}

# Purely nominal split is still ambiguous.
{
    multi n(Int $a, Any $b) { 1 }
    multi n(Any $a, Int $b) { 2 }
    throws-like { n(1, 2) }, X::Multi::Ambiguous, 'nominal split is ambiguous';
}
