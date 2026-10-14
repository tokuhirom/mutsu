use Test;
plan 12;

# Candidates whose signature is only nominally typed positionals are matched on
# the argument types alone (#12942); the verdicts must be the ordinary ones.
class Grab {
    has %!hash;
    proto method pick(|) {*}
    multi method pick(Int)  { %!hash<int>++ }
    multi method pick(Str)  { %!hash<str>++ }
    multi method pick(Rat)  { %!hash<rat>++ }
    multi method pick(Num $n)  { %!hash<num>++ }
    multi method pick(Int $a, Str $b) { %!hash<int-str>++ }
    multi method pick($any, $other, $third) { %!hash<three>++ }
    method counts { %!hash.sort.map(*.kv.join('=')).join(',') }
}

my $g = Grab.new;
$g.pick(42);
$g.pick('s');
$g.pick(1.5);
$g.pick(2e0);
$g.pick(1, 'x');
$g.pick(1, 2, 3);
is $g.counts, 'int=1,int-str=1,num=1,rat=1,str=1,three=1', 'each call reaches its own candidate';

throws-like { $g.pick(1, 2) }, X::Multi::NoMatch, 'no candidate takes (Int, Int)';
throws-like { $g.pick() }, X::Multi::NoMatch, 'no candidate takes no arguments';

class Wide {
    multi method m(Int $x)  { 'int' }
    multi method m(Cool $x) { 'cool' }
    multi method m(Any $x)  { 'any' }
    multi method m($x)      { 'untyped' }
}
is Wide.new.m(1), 'int', 'narrowest nominal candidate wins';
is Wide.new.m('a'), 'cool', 'a Str is a Cool';
is Wide.new.m(Any.new), 'any', 'an Any object takes the Any candidate';

# Int, Num, Str and Rat are unrelated classes: none accepts another.
class Disjoint {
    multi method d(Num $x) { 'num' }
    multi method d(Str $x) { 'str' }
}
is Disjoint.new.d(1e0), 'num', 'Num';
is Disjoint.new.d('1'), 'str', 'Str';
throws-like { Disjoint.new.d(1) }, X::Multi::NoMatch, 'an Int is neither a Num nor a Str';
throws-like { Disjoint.new.d(1.5) }, X::Multi::NoMatch, 'a Rat is neither a Num nor a Str';

# A subset named after a core class is still honoured by the general checker.
class Subbed {
    multi method s(Int $x) { 'int' }
    multi method s(Str $x) { 'str' }
}
is Subbed.new.s(5), 'int', 'Int';
is Subbed.new.s('5'), 'str', 'Str';
