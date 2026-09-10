use v6;
use Test;

# `is default(V)` combined with a `where` clause (WhateverCode or Junction) and
# used in expression position must evaluate to its default V — mutsu previously
# (a) rejected the default at compile time as "will never bind" to the anonymous
# subset the `where` desugars to, and (b) returned Nil for a decl-as-expression.
# It also must smartmatch a Junction `where` predicate on a class attribute.
# (roast/S02-types/whatever.t "compile time WhateverCode / Junction evaluation".)

plan 9;

# WhateverCode `where` on a variable, in expression position.
is (my $a is default(42) where * == 42), 42,
    'is default + WhateverCode where, decl as expression';

# Junction `where` on a variable, in expression position.
is (my $b is default(42) where 42|3), 42,
    'is default + Junction where, decl as expression';

# Plain `is default` decl used as an expression returns the default.
is (my $c is default(7)), 7, 'is default decl as expression returns default';

# Subset + default on a variable, in expression position.
{
    my subset Foo where * == 42;
    is (my Foo $d is default(42)), 42, 'subset (WhateverCode) + default as expression';
}
{
    my subset Bar where 1|42;
    is (my Bar $e is default(42)), 42, 'subset (Junction) + default as expression';
}

# `where` clause + `is default` trait on a class attribute (WhateverCode).
is (my class { has $.z is default(42) where * == 42 }.new.z), 42,
    'attribute is default + WhateverCode where';

# `where` clause + `is default` trait on a class attribute (Junction).
is (my class { has $.z is default(42) where 42|3 }.new.z), 42,
    'attribute is default + Junction where';

# The subset-permissiveness must not defeat a genuine `:U` mismatch.
throws-like { EVAL 'my Int:U $y is default(0)' }, X::Parameter::Default::TypeCheck,
    ':U scalar still rejects a concrete default value';

# A genuine built-in type mismatch is still caught.
throws-like { EVAL 'my Int $z is default("x")' }, X::Parameter::Default::TypeCheck,
    'Int scalar still rejects a Str default value';
