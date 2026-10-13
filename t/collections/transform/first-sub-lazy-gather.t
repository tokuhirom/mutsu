use v6;
use Test;

# #13025: the sub form `first(&matcher, gather {...})` pulls the gather one
# element per test, as the method form does, so an infinite gather answers and
# the body runs only as far as the first match. Adverbs reach the same path.
# Every expectation is Rakudo v2026.09's output.

plan 11;

sub inf() { gather { my $i = 0; loop { take $i++ } } }

is first({ $_ > 1 }, gather { loop { take 1; take 2 } }), 2,
    'the issue repro answers';
is first(* > 5, inf()), 6, 'a WhateverCode matcher over an infinite gather';
is first(* > 5, (1..*).map(* * 2)), 6, 'an infinite .map pipe';
is first(* > 3, inf(), :k), 4, ':k over an infinite gather';
is first(* > 1, gather { take 1; take 2; take 3 }, :end), 3, ':end over a finite gather';
is first(* > 1, gather { take 1; take 2; take 3 }, :p), (1 => 2), ':p over a finite gather';
is first(* > 10, gather { take 1; take 2 }), Nil, 'no match in a finite gather';

{
    my $n = 0;
    is first(* >= 3, gather { loop { $n++; take $n } }), 3, 'the match';
    is $n, 3, 'the body ran only as far as the first match';
}

is first(* >= 20000, inf()), 20000, 'a long scan answers';
is first(* > 1, 1, 2, 3), 2, 'a flat argument list is unchanged';
