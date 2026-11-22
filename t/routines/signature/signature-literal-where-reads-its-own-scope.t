use Test;

# A `where` clause of a signature literal reads the names of the scope the
# literal is written in, whoever smartmatches against it (#18570).

plan 9;

{
    my &good = { True };
    my $sig = :($x where &good);
    sub check($c) { my &good = { False }; $c ~~ $sig }
    ok check(\(42)), 'a caller\'s same-named &good does not answer';
}

{
    my $s = do { my &good2 = { True }; :($x where &good2) };
    ok \(42) ~~ $s, 'a name declared only in the literal\'s own scope is found';
}

sub g(&c) { c() }

g {
    my $wanted;
    my &pb = { $wanted = $^got; True };
    my sub pm($v) { $wanted = $v; True }
    g {
        $wanted = Nil;
        ok \(Nil, 42, 50, :73y) ~~ :($, $x where &pb, *@, *% (:$y where &pb)),
            'a nested block matches through an outer block\'s &pb';
        ok $wanted ~~ 42 | 73, 'the clause wrote the outer variable';
        $wanted = Nil;
        ok \(45) ~~ :($x where &pm), 'an outer lexical sub as the clause';
        is $wanted, 45, 'the lexical sub wrote the outer variable';
        nok \(46) ~~ :($x where { $_ < 10 }), 'a failing clause is a non-match';
    }
}

{
    my $lo = 10;
    my $sig = :($x where * > $lo);
    sub probe($c) { my $lo = 100; $c ~~ $sig }
    ok probe(\(50)), 'a WhateverCode clause reads its own scope\'s $lo';
}

{
    my $fallback = 7;
    my $sig = :($x = $fallback);
    ok \() ~~ $sig, 'a computed default does not stop a match';
}
