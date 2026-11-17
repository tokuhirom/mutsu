use Test;

# A quoted or run-time method name (`$o."$name"()`) dispatches like the bare
# name, so it shares the accessor and plain-method lanes (#17811); only a MOP
# pseudo-method name (`WHAT`, `HOW`, `VAR`, ...) differs, where the quoted
# form reaches a user method of that name.

plan 12;

class P {
    has $.a = 1;
    has $.b is rw = 2;
    method m() { "m" }
    method WHAT() { "user WHAT" }
}

my $p = P.new;
my @names = <a b m a b m>;
is-deeply @names.map({ $p."$_"() }).List, (1, 2, "m", 1, 2, "m"),
    'run-time names: accessors and a method, repeatedly';

my $n = "a";
my @seen = (^5).map({ my $v = $p."$n"(); $v });
is-deeply @seen, [1 xx 5], 'a hot dynamic accessor read';

{
    my $name = "b";
    my $c := $p."$name"();
    $c = 42;
    is $p.b, 42, 'a bound dynamic `is rw` accessor still hands back the container';
}

is $p."WHAT"(), "user WHAT", 'a quoted pseudo-method name reaches the user method';
ok $p.WHAT === P, 'the bare pseudo-method is the reflection macro';
my $what = "WHAT";
is $p."$what"(), "user WHAT", 'a run-time pseudo-method name reaches the user method';

class Q is P {
    method a() { "overridden" }
}
my $q = Q.new;
is $q."$n"(), "overridden", 'a subclass method shadows the accessor for a dynamic name';
is $p."$n"(), 1, 'the parent accessor still answers for the parent';
is $q.b, 2, 'an inherited accessor read';

class R { has $.x; }
my $r = R.new(:x(7));
$r.x for ^3;
is $r."x"(), 7, 'a quoted literal name after bare calls';
my $s = R.new(:x(8));
is (^3).map({ $s."x"() }).List, (8, 8, 8), 'a quoted literal name before any bare call';

my $wrapped = R.^find_method("x").wrap(-> \self { callsame() * 10 });
is $r."x"(), 70, 'a wrapped accessor runs its wrapper for a quoted name';
