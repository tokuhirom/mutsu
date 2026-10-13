use v6;
use Test;

# ADR-11678 §8.5 / #13026: the operand of a reduction or of the zip and cross
# metaoperators is iterated, as Rakudo's operators iterate it. An uncached Seq
# (a gather, a `.map`) is consumed, a consumed one raises at once, a cached one
# stays readable, and an infinite gather still streams under `Z` and `[\op]`.
# `[max]` / `[min]` call the operator's list candidate, which caches the Seq
# instead. Every expectation is Rakudo v2026.09's output.

plan 25;

sub g() { gather { take 1; take 2; take 3 } }
sub m() { (1..3).map(* + 1) }
sub inf() { gather { my $i = 0; loop { take $i++ } } }

# What re-reading `s` gives: 'reread', or the exception's type.
sub reread(\s) {
    my $r = try { s.List; 'reread' };
    $r // $!.^name
}

# --- Reductions consume ---------------------------------------------------------

{ my \s = g(); my $x = [+] s; is reread(s), 'X::Seq::Consumed', '[+] consumes a gather'; }
{ my \s = g(); my $x = [*] s; is reread(s), 'X::Seq::Consumed', '[*] consumes a gather'; }
{ my \s = g(); my $x = [~] s; is reread(s), 'X::Seq::Consumed', '[~] consumes a gather'; }
{ my \s = g(); my $x = [,] s; is reread(s), 'X::Seq::Consumed', '[,] consumes a gather'; }
{ my \s = m(); my $x = [+] s; is reread(s), 'X::Seq::Consumed', '[+] consumes a .map Seq'; }
{ my \s = m(); my $x = [\+] s; is reread(s), 'X::Seq::Consumed', '[\+] consumes a .map Seq'; }
{
    my $s = m();
    is ([+] $s), 9, '[+] over an itemized Seq sums its elements';
    is (try { $s.List; 'reread' }) // $!.^name, 'X::Seq::Consumed',
        '... and consumes it';
}

# --- [max] and [min] cache ------------------------------------------------------

{ my \s = g(); my $x = [max] s; is reread(s), 'reread', '[max] leaves a gather readable'; }
{ my \s = m(); my $x = [min] s; is reread(s), 'reread', '[min] leaves a .map Seq readable'; }
{ my \s = m(); my $x = [\max] s; is reread(s), 'X::Seq::Consumed', '[\max] consumes'; }

# --- A consumed Seq raises ------------------------------------------------------

{ my \s = g(); s.List; throws-like { [+] s }, X::Seq::Consumed, '[+] over a consumed gather'; }
{ my \s = g(); s.List; throws-like { [max] s }, X::Seq::Consumed, '[max] over a consumed gather'; }
{ my \s = g(); s.List; throws-like { (s Z 1).List }, X::Seq::Consumed, 'Z over a consumed gather'; }
{ my \s = g(); s.List; throws-like { (s X 1).List }, X::Seq::Consumed, 'X over a consumed gather'; }

# --- A cached Seq is re-readable ------------------------------------------------

{
    my \s = g();
    s[1];
    is ([+] s), 6, '[+] over a subscripted gather';
    is ([+] s), 6, '... twice';
}

# --- Zip and cross consume, and stream an infinite gather -----------------------

{ my \s = g(); my $x = (s Z 1..3).List; is reread(s), 'X::Seq::Consumed', 'Z consumes a gather'; }
{ my \s = m(); my $x = (s Z 1).List; is reread(s), 'X::Seq::Consumed', 'Z consumes a .map Seq'; }
{ my \s = m(); my $x = (s X 1).List; is reread(s), 'X::Seq::Consumed', 'X consumes a .map Seq'; }
is (inf() Z 1..3).List, ((0, 1), (1, 2), (2, 3)), 'Z streams an infinite gather';
is ([\+] inf())[^5], (0, 1, 3, 6, 10), '[\+] streams an infinite gather';

# --- An itemized Seq is one zip or cross element --------------------------------

{
    my $s = g();
    my @rows = $s Z 1..3;
    is @rows.elems, 1, 'Z reads an itemized gather as one element';
    isa-ok @rows[0][0], Seq, '... the Seq itself';
    is reread($s), 'reread', '... and leaves it readable';
}
