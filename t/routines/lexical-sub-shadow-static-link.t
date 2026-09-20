# ADR-0110 §3.3 links a call site to a routine at COMPILE time. A bare name
# can mean different routines at different points in the same compunit — a
# `my sub` is lexical to its block, and a later declaration shadows an earlier
# one — so the link has to follow the name's lexical fate, not merely the last
# routine that happened to carry a chunk.
#
# Both directions were wrong before the link was scoped: the first answered 4
# instead of 300.
use Test;
use nqp;

plan 4;

{
    my sub shadowed(int $a) { nqp::add_i($a, 1) }
    my int $x = 1;
    is shadowed($x), 2, 'the lexical sub answers inside its own block';
}
sub shadowed($a) { $a * 100 }
my $y = 3;
is shadowed($y), 300,
    'a later same-named routine shadows the block-lexical one the call site saw';

sub outer-first($a) { $a ~ '!' }
{
    my sub outer-first(int $a) { nqp::add_i($a, 1) }
    my int $z = 7;
    is outer-first($z), 8, 'the inner lexical sub wins inside the block';
}
is outer-first('hi'), 'hi!',
    'and stops being the call target once its block has exited';
