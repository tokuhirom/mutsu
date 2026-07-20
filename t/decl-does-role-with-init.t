use Test;

# `my @a does R = <init>` applies the `does R` role trait to the freshly-declared
# container AND then runs the `= <init>` initializer. mutsu's operand parser
# greedily absorbed the `= <init>` into the role operand (`R = <a b>`), so the
# role was lost and the statement mis-parsed to a Bool.

plan 10;

role R  { method greet { 'hi' } }
role R2 { method bye   { 'bye' } }

# Array
my @a does R = <a b c>;
is @a.^name, 'Array+{R}', 'array does-trait survives the initializer';
is @a.elems, 3,           'array initializer filled the elements';
is @a[1], 'b',            'array elements are correct';
is @a.greet, 'hi',        'role method reachable on the array';

# Array with comma-list initializer
my @b does R = 1, 2, 3;
is @b.^name, 'Array+{R}', 'comma-list initializer keeps the role';
is @b.elems, 3,           'comma-list filled three elements';

# Hash
my %h does R = (x => 1, y => 2);
is %h.^name, 'Hash+{R}', 'hash does-trait survives the initializer';
is %h<x>, 1,             'hash initializer filled a pair';
is %h.greet, 'hi',       'role method reachable on the hash';

# The bare no-init form still works (regression guard)
my @c does R2;
is @c.^name, 'Array+{R2}', 'bare does-trait (no init) still works';
