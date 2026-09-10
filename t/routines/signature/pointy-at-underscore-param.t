use v6;
use Test;

plan 11;

# A pointy block whose single parameter is `@_` must bind the argument to `@_`,
# not leave it shadowed by the implicit args array (regression: zef's
# Zef::Utils::FileSystem `list-paths` uses `-> @_ { grep {...}, @_ }`).
{
    my &w := -> @_ { @_.elems };
    is w((1, 2, 3)), 3, '-> @_ binds argument (elems)';
}

{
    my &w := -> @_ { @_[1] };
    is w((10, 20, 30)), 20, '-> @_ binds argument (indexing)';
}

{
    my &w := -> @_ { grep { $_ > 1 }, @_ };
    is-deeply w((1, 2, 3)).List, (2, 3), '-> @_ works as grep source';
}

{
    my &w := -> @_ { map { $_ * 2 }, @_ };
    is-deeply w((1, 2, 3)).List, (2, 4, 6), '-> @_ works as map source';
}

{
    my &w := -> @_ { @_ };
    is-deeply w((1, 2, 3)).List, (1, 2, 3), '-> @_ returns the whole list';
}

# `%_` as an explicit pointy parameter binds the argument hash.
{
    my &w := -> %_ { %_<a> };
    is w(%(a => 5, b => 6)), 5, '-> %_ binds the hash argument';
}

# A named sub with an `@_` parameter still works (unchanged behaviour).
{
    sub s(@_) { @_.elems }
    is s((7, 8)), 2, 'sub with @_ param binds argument';
}

# Coercion-typed for-loop variables must parse and coerce.
{
    my @out;
    for (1, 2, 3) -> Int() $x { @out.push($x) }
    is-deeply @out.List, (1, 2, 3), 'for with Int() coercion loop var';
}

{
    my @out;
    for <1 2 3> -> Int() $x { @out.push($x + 1) }
    is-deeply @out.List, (2, 3, 4), 'for coerces Str elements via Int()';
}

# Coercion for-loop var over a function-call iterable (the zef idiom).
{
    my &wp := -> @_ { grep { $_ > 1 }, @_ };
    my @out;
    for wp((1, 2, 3)) -> Int() $x { @out.push($x) }
    is-deeply @out.List, (2, 3), 'for over -> @_ result with coercion var';
}

# Coercion with an explicit source type: Str(Int).
{
    my @out;
    for (1, 2) -> Str(Int) $x { @out.push($x ~ '!') }
    is-deeply @out.List, ('1!', '2!'), 'for with Str(Int) coercion loop var';
}
