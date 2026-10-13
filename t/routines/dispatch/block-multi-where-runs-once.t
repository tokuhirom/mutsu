use Test;

# #13183: a multi call resolves once, so a candidate's `where` (or a subset's
# predicate) runs once per call however the call reaches the family: a
# block-declared multi called from a closure dispatches by name through its
# code value, and a multi returned out of its block dispatches through the
# captured candidates.

plan 8;

{
    my $n = 0;
    sub c() { $n++; 0 }
    multi k(Int $x where * > c()) { 'pos' }
    multi k(Int $x) { 'int' }
    my @r = (^5).map({ k($_) });
    is @r.join(','), 'int,pos,pos,pos,pos', 'block multi from a closure picks the right candidates';
    is $n, 5, 'block multi from a closure runs the winner where once per call';
}

{
    my $n = 0;
    subset P of Int where { $n++; $_ > 0 };
    multi t(P $x) { 'pos' }
    multi t(Int $x) { 'int' }
    is (^5).map({ t($_) }).join(','), 'int,pos,pos,pos,pos', 'block subset multi from a closure';
    is $n, 5, 'block subset multi from a closure runs the predicate once per call';
}

my $m = 0;
sub d() { $m++; 0 }
my &kk = do { multi k(Int $x where * > d()) { 'pos' }; multi k(Int $x) { 'int' }; &k };
is (^5).map({ kk($_) }).join(','), 'int,pos,pos,pos,pos', 'escaped multi picks the right candidates';
is $m, 5, 'escaped multi runs where once per call, a losing candidate included';

# The deferral chain still reaches the next candidate that accepts the call.
my &nn = do {
    multi f(Int $x where * > 0) { 'pos>' ~ callsame }
    multi f(Int $x) { 'int' }
    multi f(Str $x) { 'str' }
    &f
};
is nn(3), 'pos>int', 'callsame from an escaped where winner reaches the next candidate';
is nn(0), 'int', 'an escaped multi whose where fails takes the next candidate';
