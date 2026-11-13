use Test;

# A `Cool` method (`.comb`, `.uc`, ...) on an instance of a class that is
# itself `Cool` resolves to the builtin even when the class (or the grammar
# whose `Match` it is) defines `FALLBACK`; only a missing name reaches it.

plan 5;

grammar G {
    token TOP { (abc) }
    method FALLBACK($name, |c) { die "FALLBACK $name" }
}
my $m = G.parse('abc');
is $m[0].comb.join(','), 'a,b,c', 'Match of a FALLBACK grammar: .comb is the builtin';
is $m.Str.comb.elems, 3, '.Str.comb on the whole match';

class C is Cool {
    method FALLBACK($n, |c) { "fb:$n" }
    method Str { "xy" }
}
is C.new.comb.join(','), 'x,y', 'Cool class with FALLBACK: .comb reads .Str';
is C.new.nosuch, 'fb:nosuch', 'an unknown name still reaches FALLBACK';

class D {
    method FALLBACK($n, |c) { "fb:$n" }
}
is D.new.uc, 'fb:uc', 'a non-Cool class gets Cool-only names through FALLBACK';
