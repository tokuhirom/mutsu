use Test;

# `@:name`, `%:name` and `&:name` are named placeholders just like `$:name`.

plan 9;

my &f = { %:c };
is-deeply f(:c({a => 1})), {a => 1}, '%:c takes a named hash argument';

my &g = { @:a };
is-deeply g(:a([1, 2])), [1, 2], '@:a takes a named array argument';

my &h = { &:x() + $:y };
is h(:x({ 40 }), :y(2)), 42, '&:x and $:y mix';

is { @:a.elems + %:b.elems }(:a([1, 2, 3]), :b({x => 1})), 4, 'mixed named sigils';

sub s { %:opts<k> }
is s(:opts({k => 'v'})), 'v', 'a sub takes a sigiled named placeholder';

my &req = { @:a };
throws-like { req() }, X::AdHoc, 'a missing @:a placeholder argument is rejected';
throws-like { req(:b(1)) }, X::AdHoc, 'an unexpected named argument is rejected';

is { %:c.elems }(:c({a => 1, b => 2})), 2, 'method call on a hash placeholder';
is { "x $:s y" }(:s(3)), 'x 3 y', 'a scalar named placeholder interpolates';
