use Test;

plan 14;

proto p($x) {*}
multi p(Int $x) { 1 }
proto q($x) { 5 }
sub s() { }

is-deeply &p.onlystar, True, 'a proto whose body is {*} is onlystar';
is-deeply &q.onlystar, False, 'a proto with a body of its own is not';
is-deeply &s.onlystar, False, 'a plain sub is not';
is-deeply &say.onlystar, True, 'a built-in routine is a proto of its candidates';
is-deeply /a/.onlystar, False, 'a regex is not';

is-deeply &s.soft, Nil, 'Routine.soft on a sub';
is-deeply &p.soft, Nil, 'Routine.soft on a proto';
is-deeply /a/.soft, Nil, 'Routine.soft on a regex';

ok so(&p.^can('onlystar')) && so(&p.^can('soft')), '.^can agrees for onlystar and soft';

my $rx = /a/;
is $rx.cando(\("x")).elems, 1, 'a regex takes one positional argument';
is $rx.cando(\()).elems, 0, '... not none';
is $rx.cando(\("x", "y")).elems, 0, '... nor two';
is $rx.cando(\(:a)).elems, 0, '... and named arguments do not count';
is $rx.cando(\("x", :a)).elems, 1, '... beside a positional one';
