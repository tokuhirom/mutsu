use v6.e.PREVIEW;
use Test;

plan 42;

# ---------------------------------------------------------------------------
# Range.int-bounds($from is rw, $to is rw --> Bool)
#
# The two-argument candidate binds the integer bounds into its rw arguments and
# answers whether the Range has any. The lower endpoint is never rounded
# outward: a fractional min simply has no integer bounds.
# ---------------------------------------------------------------------------

if (3..5).int-bounds(my $min, my $max) {
    is "$min, $max", "3, 5", 'int-bounds binds the bounds of 3..5';
} else {
    flunk 'int-bounds binds the bounds of 3..5';
}

sub bounds-of($r) {
    my ($from, $to);
    my $ok = $r.int-bounds($from, $to);
    $ok ?? ($from, $to) !! Nil;
}

is-deeply bounds-of(3..5),        (3, 5),   'int-bounds 3..5';
is-deeply bounds-of(3..^5),       (3, 4),   'int-bounds excludes the max';
is-deeply bounds-of(3^..5),       (4, 5),   'int-bounds excludes the min';
is-deeply bounds-of(3^..^5),      (4, 4),   'int-bounds excludes both ends';
is-deeply bounds-of(-3..^0),      (-3, -1), 'int-bounds over negatives';
is-deeply bounds-of(1..^1),       (1, 0),   'int-bounds of an empty int range';
is-deeply bounds-of(1.0..5.0),    (1, 5),   'integral Rat endpoints have int bounds';
is-deeply bounds-of(1..5.5),      (1, 5),   'a fractional max floors';
is-deeply bounds-of(1..^5.5),     (1, 5),   'an excluded fractional max still floors';
is-deeply bounds-of(1..^5.0),     (1, 4),   'an excluded integral max drops one';
is-deeply bounds-of(-5..-1.5),    (-5, -2), 'a fractional max floors downward';
is-deeply bounds-of(1e0..5e0),    (1, 5),   'integral Num endpoints have int bounds';

nok (1.1..5.2).int-bounds(my $a1, my $b1),  'a fractional min has no int bounds';
nok (1.5..^5).int-bounds(my $a2, my $b2),   'a fractional min is not rounded up';
nok (1..Inf).int-bounds(my $a3, my $b3),    'an infinite max has no int bounds';
nok (-Inf..5).int-bounds(my $a4, my $b4),   'an infinite min has no int bounds';
nok (1..*).int-bounds(my $a5, my $b5),      'a Whatever max has no int bounds';
nok ('a'..'z').int-bounds(my $a6, my $b6),  'a Str range has no int bounds';

# The zero-argument candidate returns the pair, and fails when there is none.
is-deeply (3..5).int-bounds,   (3, 5), 'zero-arg int-bounds returns the pair';
is-deeply (3..^5).int-bounds,  (3, 4), 'zero-arg int-bounds honours exclusivity';
is-deeply (0..5.5).int-bounds, (0, 5), 'zero-arg int-bounds floors a fractional max';
dies-ok { (1.1..5.2).int-bounds }, 'zero-arg int-bounds dies on a fractional min';
dies-ok { ('a'..'z').int-bounds }, 'zero-arg int-bounds dies on a Str range';
dies-ok { (1..Inf).int-bounds },   'zero-arg int-bounds dies on an infinite range';
is-deeply int64.Range.int-bounds, (-9223372036854775808, 9223372036854775807),
    'the genuine full-i64 range still has concrete bounds';

# ---------------------------------------------------------------------------
# Range.minmax
#
# An excluded end folds into the returned bound only when the Range is is-int.
# Otherwise the excluded bound cannot be named and minmax is an error.
# ---------------------------------------------------------------------------

is-deeply (1..5).minmax,    (1, 5), 'minmax of a plain int range';
is-deeply (1..^5).minmax,   (1, 4), 'minmax folds an excluded int max';
is-deeply (1^..5).minmax,   (2, 5), 'minmax folds an excluded int min';
is-deeply (1^..^5).minmax,  (2, 4), 'minmax folds both excluded int ends';
is-deeply ('a'..'z').minmax, ('a', 'z'), 'minmax of an inclusive Str range';

dies-ok { (1.1..^5.2).minmax }, 'minmax dies on an excluded Rat end';
dies-ok { (1.0..^5.0).minmax }, 'minmax dies on an excluded integral Rat end';
dies-ok { ('a'..^'z').minmax }, 'minmax dies on an excluded Str end';
dies-ok { (1..^Inf).minmax },   'minmax dies on an excluded infinite end';

# .min/.max keep the raw endpoints even when excluded.
is (1..^5).min, 1, '.min is the raw lower endpoint';
is (1..^5).max, 5, '.max is the raw upper endpoint';

# ---------------------------------------------------------------------------
# The 6.e `rotor` subroutine: rotor(**@cycle, \thing, Bool() :$partial)
# ---------------------------------------------------------------------------

is rotor(3, 'a'..'h').join('|'), 'a b c|d e f', 'rotor(3, list)';
is rotor(3, 'a'..'h', :partial).join('|'), 'a b c|d e f|g h', 'rotor with :partial';
is rotor(2 => 1, 'a'..'h').join('|'), 'a b|d e|g h', 'rotor with a gap Pair';
is rotor(3 => -1, 'a'..'h').join('|'), 'a b c|c d e|e f g', 'rotor with an overlap Pair';
is rotor(2, 3, 'a'..'h').join('|'), 'a b|c d e|f g', 'rotor cycles a multi-element spec';
