use Test;

# #18267: the slot method frame (ADR-18393) also admits `**`, `%`, `div`,
# negation, the numeric and string comparisons, `~`, item wrapping and an
# explicit `return`. These pin that such bodies still answer exactly as the
# lean frame did, including through user operators, coercions and `return`.

plan 16;

class P {
    has $.x;
    has $.y;
    method dist(P $o) { (($!x - $o.x) ** 2 + ($!y - $o.y) ** 2).sqrt }
    method m($a, $b) { $a % $b + $a div $b }
    method neg($a) { -$a }
    method lt($a, $b) { $a < $b }
    method seq($a, $b) { $a eq $b }
    method cat($a) { return $.x ~ "/" ~ $a }
    method early($a) { return $a ** 3 }
}

my $p = P.new(x => 0, y => 0);
my $q = P.new(x => 3, y => 4);
is $p.dist($q), 5, 'power and sqrt on attribute reads';
is $p.m(17, 5), 5, 'modulus and integer division';
is $p.neg(4), -4, 'negation';
is $p.lt(1, 2), True, 'numeric comparison';
is $p.seq("a", "a"), True, 'string comparison';
is $q.cat("z"), '3/z', 'concatenation with an explicit return';
is $p.early(2), 8, 'explicit return of a power';

my $sum = 0;
$sum += $p.dist($q) for ^500;
is $sum, 2500, 'a hot loop over the slot frame';

class S { has $.v; method Str { "S" ~ $!v } }
is $p.cat(S.new(v => 9)), '0/S9', 'concatenation coerces an object through its Str';

multi sub infix:<**>(S $a, Int $b) { "pow:" ~ $a.v ~ ":" ~ $b }
class Q { method sq($a) { $a ** 2 } }
is Q.new.sq(S.new(v => 3)), 'pow:3:2', 'power calls a user operator candidate';
is Q.new.sq(5), 25, 'and still powers a plain Int';

{
    my $r = try { $p.neg("abc") };
    isa-ok $!, X::Str::Numeric, 'an exception thrown in the body propagates';
    is $p.neg(1), -1, 'the frame is usable again after an exception';
}

sub through-a-sub { $q.cat("s") ~ $q.early(1) }
is through-a-sub(), '3/s1', 'called from a sub frame';

is-deeply (1..3).map({ $p.early($_) }).List, (1, 8, 27), 'called from a closure';

{
    my @r = await (^4).map: -> $t {
        start { my $s = 0; $s += $p.dist($q) for ^200; $s }
    };
    is-deeply @r, [1000e0 xx 4], 'concurrent calls';
}
