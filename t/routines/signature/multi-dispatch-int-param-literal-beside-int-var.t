use Test;

plan 8;

# A literal beside a genuinely native (`my int`) argument of the same call binds
# a native `int` parameter of a multi, as in rakudo; two literals still do not
# (#16483).

multi f(int, int) { "ii" }

is (f 42, my int $x), "ii", 'a native declaration as an argument beside a literal';
is $x, 0, 'the declared variable is the default 0';

my int $y = 5;
is f(7, $y), "ii", 'literal then native variable';
is f($y, 7), "ii", 'native variable then literal';
is f($y, $y), "ii", 'two native variables';
throws-like { EVAL 'multi g(int, int) { 1 }; g 7, 7' }, X::Multi::NoMatch,
    'two literals do not bind native int parameters';
my $z = 3;
throws-like { EVAL 'multi h(int, int) { 1 }; my $w = 3; h $w, 7' }, X::Multi::NoMatch,
    'a plain Int variable beside a literal does not either';

multi d(int) { "int" }
multi d(Int) { "Int" }
is (d(5), d($y)).join(","), "Int,int", 'candidate ranking is unchanged';
