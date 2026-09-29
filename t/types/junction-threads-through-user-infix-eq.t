use Test;

# From the Date::WorkdayCalendar distribution: it exports
# `multi infix:<eq>(WorkdayCalendar:D, WorkdayCalendar:D)`, and the test suite
# then writes `'$!calendar' eq any(@attributes)`.

plan 6;

class C { has $.a }
multi infix:<eq>(C:D $x, C:D $y) { $x.a == $y.a }

ok 'a' eq 'a', 'plain Str eq still uses the core candidate';
is-deeply ('a' eq any(<a b>)).raku, 'any(Bool::True, Bool::False)', 'Junction on the right threads';
my $j = any('a', 'b');
is-deeply ($j eq 'b').raku, 'any(Bool::False, Bool::True)', 'Junction in a variable on the left threads';
ok ('a' eq any(<x a>)).so, 'the collapsed Junction is truthy';
nok ('z' eq any(<x a>)).so, 'and falsy when no eigenstate matches';
ok C.new(:a(1)) eq C.new(:a(1)), 'the user candidate still takes objects';
