use Test;

# `*@v is raw` / `+@v is raw` / `**@v is raw` bind a List, not an Array
# (annotations' Buffer.STORE / STAMP take `+@values is raw`).
plan 10;

sub f(+@v is raw) { @v.^name }
is f(1, 2), 'List', '+@v is raw with several args is a List';
is f([1, 2]), 'List', '+@v is raw with one Array arg is a List';
sub h(*@v is raw) { @v.^name }
is h(1, 2), 'List', '*@v is raw is a List';
is h(), 'List', '*@v is raw with no args is a List';
sub p(**@v is raw) { @v.^name }
is p(1, 2), 'List', '**@v is raw is a List';
sub g(+@v) { @v.^name }
is g(1, 2), 'Array', '+@v without is raw stays an Array';
sub q(*@v) { @v.^name }
is q(1, 2), 'Array', '*@v without is raw stays an Array';
sub m(+@v is raw) { @v.raku }
is m(1, 2), '(1, 2)', '+@v is raw renders as a List';
sub n(*@v is raw) { @v[0] = 5 }
throws-like { n(1, 2) }, X::Assignment::RO, '*@v is raw elements are read-only';
sub k(*@v is raw) { @v.elems }
is k(1, 2, 3), 3, 'element count is unchanged';
