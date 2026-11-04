use Test;

# A typed declaration with an initializer used as a call argument keeps its
# value: `ok my IntTree(Str) $tree = "1(2)(3)"` left `$tree` the type object
# (BTree's t/simple-int-trees.rakutest, #16291). Checked against rakudo.

plan 11;

sub f($x) { $x }

f(my Int $u = 5);
is $u, 5, 'a typed declaration passed to a plain parameter keeps its value';

f(my Str $s = "a");
is $s, "a", '...for Str';

sub g(Int $x) { $x }
g(my Int $m = 6);
is $m, 6, '...for a typed parameter';

sub h($x, $y) { $x }
h(my Int $p = 7, 1);
is $p, 7, '...as the first of two arguments';

f(my Int $q);
ok !$q.defined, 'a bare typed declaration is still the type object';
is $q.^name, 'Int', '...of its declared type';

f(my Int(Str) $c = "5");
is $c, 5, 'a coercion-typed declaration is coerced and kept';
ok $c ~~ Int, '...to the declared type';

class K { has $.a }
f(my K() $k = K.new(a => 1));
is $k.a, 1, 'a class-coercion typed declaration keeps its object';

is (ok my Int $t = 9), True, 'ok with a typed declaration still passes';
