use v6;
use Test;

plan 6;

# A failed `.parse` measures how far it got with a side-effect-free probe
# (ADR-0009). A grammar METHOD reached as a subrule is user code, so the probe
# must not call it: the real match already ran it once at each position, and
# raku never runs it a second time (#11608).

my @seen;
grammar W {
    rule TOP { 'a' 'b' 'c' }
    method ws() { @seen.push: self.pos; callsame }
}

@seen = ();
nok W.parse('a b x'), 'the parse fails on the third word';
is-deeply @seen, [1, 3], 'the overridden ws ran once per position, not twice';

@seen = ();
ok W.parse('a b c'), 'the parse succeeds on the full input';
is-deeply @seen, [1, 3, 5], 'a successful parse calls ws once per position';

# A plain method subrule whose side effect is a counter.
my $calls = 0;
grammar M {
    token TOP { 'x' <counted> 'y' }
    method counted() { $calls++; self }
}

$calls = 0;
nok M.parse('xz'), 'the parse fails after the method subrule';
is $calls, 1, 'the method subrule ran exactly once';
