use Test;

# A planned method whose body calls an argument-less method on one of its
# parameters (`$s.chars`) runs in the slot frame, where that call is the call
# on the parameter's value: a readonly parameter is never rebound, so none of
# the by-name variable machinery runs (#19266). The slot frame shares its
# caller's env, so these pin that nothing leaks into or out of it. Each case
# repeats so the planned calls are the ones checked.

plan 25;

class Box { has $.n = 0; method inc { $!n++; self } }
class P {
    method boom  { die "pboom" }
    method warny { warn "careful"; 'after-warn' }
}

class C {
    method chars(Str $s) { $s.chars }
    method lc(Str $s)    { $s.lc }
    method succ($a)      { $a.succ }
    method pop($a)       { $a.pop }
    method shift($a)     { $a.shift }
    method inc($a)       { $a.inc }
    method item($a)      { $a.item }
    method what($a)      { $a.WHAT }
    method var($v)       { $v.VAR.^name }
    method nope(Str $s)  { $s.nope }
    method boom($p)      { $p.boom }
    method warny($p)     { $p.warny }
}

my $c = C.new;

is (^3).map({ $c.chars('héllo') }).join(','), '5,5,5', 'a Str method on a typed parameter';
is (^2).map({ $c.lc('ABC') }).join(','), 'abc,abc', 'another';
is (^2).map({ $c.succ(41) }).join(','), '42,42', 'an untyped parameter';

my @a = 1, 2, 3, 4;
is (^2).map({ $c.pop(@a) }).join(','), '4,3', 'a mutator reaches the array the parameter holds';
is @a.join(','), '1,2', 'the caller sees it';
is $c.shift(@a), 1, 'shift too';

my $b = Box.new;
$c.inc($b) for ^3;
is $b.n, 3, 'a user method mutating the object the parameter holds';

is $c.item([1, 2]).raku, '$[1, 2]', '.item';
is $c.what(1/2).^name, 'Rat', '.WHAT';
is (^2).map({ $c.var(1) }).join(','), 'Scalar,Scalar', '.VAR keeps its own frame';

# A caller variable of the parameter's name is not touched.
my $s = 'outer';
$c.lc('XY') for ^3;
is $s, 'outer', 'a caller variable named like the parameter is untouched';
sub f { my $s = 'caller-s'; my $r = $c.chars('xy'); "$r $s" }
is (^2).map({ f() }).join('|'), '2 caller-s|2 caller-s', 'also a sub-local one';

# Exceptions and the caller's handlers.
my @msg;
for ^2 { try { $c.nope('x'); CATCH { default { @msg.push: .^name } } } }
is @msg.join(','), 'X::Method::NotFound,X::Method::NotFound', 'a missing method dies as usual';

my $hits = 0;
my $*dyn = 'start';
for ^3 {
    try {
        $c.boom(P.new);
        CATCH { default { $hits++; $*dyn = "set$hits" } }
    }
}
is $hits, 3, "the caller's CATCH runs for every throw";
is $*dyn, 'set3', "a dynamic the CATCH writes reaches the caller";

sub inner { my $loc = 'L'; try { $c.boom(P.new); CATCH { default { $loc ~= '!' } } }; $loc }
is (^3).map({ inner() }).join(','), 'L!,L!,L!', 'a lexical the CATCH writes';

my @log;
for ^2 {
    my $r = do { $c.warny(P.new); CONTROL { default { @log.push: .message; .resume } } };
    @log.push: $r.raku;
}
is @log.join('|'), 'careful|Any|careful|Any', 'a resumed warning';

my $x = 0;
for ^3 { try { $c.nope('y'); CATCH { default { $x += 10 } } } }
is $x, 30, 'a CATCH that writes an outer variable after a missing method';

# After all of that the plain calls still answer.
is $c.chars('abcd'), 4, 'chars still answers';
is $c.lc('Q'), 'q', 'lc still answers';
is $c.succ('a'), 'b', 'succ on a Str';
is $s, 'outer', 'the caller variable is still untouched';

# A caller variable of a parameter's name that a closure captured is a shared
# cell in the caller's env. The slot frame shares that env, so it must not
# adopt the cell as its parameter's, also for a parameter holding Nil.
class D {
    method m($n)     { $n }
    method k(Str $t) { $t.chars }
}
my $d = D.new;
my $n = 42;
my $t = 'outer-t';
my &cl = { $n++; $t ~= '!' };
cl();
is (^3).map({ ($d.m(Nil) // 'nil') }).join(','), 'nil,nil,nil', 'a Nil parameter is not the caller\'s captured cell';
is (^3).map({ $d.k('ab') }).join(','), '2,2,2', 'nor is a Str one';
is "$n $t", '43 outer-t!', 'the captured caller variables are untouched';
