use v6;
use Test;

# #13072 / ADR-11678: `polymod`, `flat` and `cache` read a gather lazily, as
# Rakudo's do, so an infinite gather works; a finite one gives the same answers
# as before. `cache`, `polymod` and a subscript leave the Seq readable, while
# `flat` consumes it. Every expectation is Rakudo v2026.09's output.

plan 22;

sub inf() { gather { my $i = 0; loop { take $i++ } } }
sub g() { gather { take 1; take (2, 3); take (gather { take 4; take 5 }) } }

# What re-reading `s` gives: 'reread', or the exception's type.
sub reread(\s) {
    my $r = try { s.List; 'reread' };
    $r // $!.^name
}

# --- The issue's lines ----------------------------------------------------------

is 600.polymod(gather { loop { take 10 } }).head(3), (0, 0, 6), 'polymod, infinite divisors';
is flat(inf).head(3), (0, 1, 2), 'flat(), infinite gather';
is cache(inf).head(3), (0, 1, 2), 'cache(), infinite gather';
is inf.flat.head(3), (0, 1, 2), '.flat, infinite gather';

# --- A finite gather ------------------------------------------------------------

is 600.polymod(gather { take 10; take 10 }), (0, 0, 6), 'polymod, finite divisors';
is 6.polymod(gather { take 10; take 10 }), (6, 0, 0), '... keeps the trailing zeros';
is flat(g()).raku, '(1, 2, 3, 4, 5).Seq', 'flat() descends into nested lists and gathers';
is flat(g()).elems, 5, 'flat() elems';
is g().flat.raku, '(1, 2, 3, 4, 5).Seq', '.flat descends too';
is flat(gather { take [1, 2]; take 3 }), (1, 2, 3), 'flat() descends into a taken Array';
is flat(gather { take $(1, 2); take 3 }).raku, '($(1, 2), 3).Seq', 'flat() keeps an itemized List';
is cache(g()).raku, '(1, (2, 3), (4, 5).Seq)', 'cache() is a List of the elements';
isa-ok cache(g()), List, 'cache() is a List';

# --- Which reads consume --------------------------------------------------------

{ my \s = gather { take 10; take 10 }; my $x = 600.polymod(s); is reread(s), 'reread', 'polymod leaves the gather readable'; }
{ my \s = g(); my $x = cache(s); is reread(s), 'reread', 'cache() leaves the gather readable'; }
{ my \s = g(); my $x = flat(s); is reread(s), 'X::Seq::Consumed', 'flat() consumes'; }
{ my \s = g(); my $x = s.flat; is reread(s), 'X::Seq::Consumed', '.flat consumes'; }
{ my \s = g(); s.cache; is s.flat.elems, 5, '.flat on a cached gather'; is reread(s), 'reread', '... leaves it readable'; }

# --- A cached gather's prefix ---------------------------------------------------

{
    my \s = inf();
    is s.cache.head(3), (0, 1, 2), '.cache.head on an infinite gather';
    is s[5], 5, '... and a later subscript';
}

{
    my $n = 0;
    my $c = cache(gather { loop { $n++; take $n } });
    $c.head(2);
    is $n, 2, 'cache() runs the body only as far as it is read';
}
