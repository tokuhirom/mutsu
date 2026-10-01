use Test;

plan 6;

class Foo is Array {}

my @f is Foo = 1, 2;
@f[1] += 5;
is-deeply @f.List, (1, 7), '+= on an `is Array` subclass element reads the old value';
@f[1] -= 2;
is-deeply @f.List, (1, 5), '-= on an `is Array` subclass element';
@f[0] ~= "x";
is-deeply @f.List, ("1x", 5), '~= on an `is Array` subclass element';
@f[4] //= 9;
is @f[4], 9, '//= stores into an unset element';
@f[1] //= 100;
is @f[1], 5, '//= keeps a defined element';

my @g = 1, 2;
@g[1] += 5;
is-deeply @g, [1, 7], 'plain array compound assign unchanged';
