use Test;

# A lazy `@` array carries its container descriptor (default and element
# type), so `.default` and `.of` answer from the value even after it leaves
# its variable.
plan 8;

my Int @t = lazy 1..*;
sub dflt(@x) { @x.default }
sub elem(@x) { @x.of }
is-deeply dflt(@t), Int, 'a typed lazy array keeps its default through a parameter';
is-deeply @t.default, Int, 'and on the variable';
is-deeply elem(@t), Int, '.of through a parameter';
is-deeply @t.of, Int, '.of on the variable';

my @u = lazy 1..*;
is-deeply @u.default, Any, 'an untyped lazy array defaults to Any';
is-deeply @u.of, Mu, 'and its element type is Mu';

my @d is default(7) = lazy 1..*;
is-deeply @d.default, 7, 'an is default value is carried';
is-deeply dflt(@d), 7, 'through a parameter too';
