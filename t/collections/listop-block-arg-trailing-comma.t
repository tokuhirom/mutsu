use Test;

# A trailing comma after a listop's block argument ends the argument list, as
# `foo 1, 2,` does. DB::Migration::Declare's tests write
# `{ check { ... }, }` (a block whose last statement is `check { ... },`).

plan 5;

sub check(&b) { b() }
my $r = {
    check {
        42
    },
};
is $r(), 42, 'check { ... }, before a closing brace';

sub foo(&b) { b() }
my @x = foo { 1 },;
is-deeply @x, [1], 'block arg with trailing comma before ;';
is (foo { 2 },), 2, 'block arg with trailing comma before )';

sub many(*@a) { @a.elems }
my @y = many { 1 }, 2;
is-deeply @y, [2], 'a block arg followed by more args still works';
my @z = many { 1 }, 2, 3,;
is-deeply @z, [3], 'trailing comma after further args still works';
