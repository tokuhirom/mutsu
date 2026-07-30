use Test;

# Raku's list-infix operators (the sequence operators `...`/`…`, and the `Z`/`X`
# meta-ops) are LOOSER than the comma separating a call's arguments, so in
# `.= new: 10, 20 ... 100` the whole comma level `10, 20` is the sequence's seed
# and the call receives ONE argument.
#
# The `.method: args` colon form used to parse each comma-separated argument on
# its own, which split the seed: `10, (20 ... 100)` built 10, 20, 21, 22, … .
# The four `.=` sites (declaration, assignment statement, `has` default,
# `constant`) each had their own copy of the argument loop and all four were
# missing the lift; they share one implementation now.

plan 12;

# --- Declaration form ---
my Array $a .= new: 10, 20 ... 50;
is-deeply $a.List, (10, 20, 30, 40, 50), 'my Type $x .= new: seed, seed ... limit';

my Array $desc .= new: 50, 40 ... 10;
is-deeply $desc.List, (50, 40, 30, 20, 10), 'a descending sequence';

my Array $geo .= new: 1, 2, 4 ... 32;
is-deeply $geo.List, (1, 2, 4, 8, 16, 32), 'a three-element geometric seed';

# --- Assignment-statement form ---
my $b = Array;
$b .= new: 10, 20 ... 50;
is-deeply $b.List, (10, 20, 30, 40, 50), '$x .= new: seed, seed ... limit';

# --- Ordinary colon-arg lists keep their per-argument meaning ---
my Array $plain .= new: 1, 2, 3;
is-deeply $plain.List, (1, 2, 3), 'a comma list without a list infix is untouched';

my Array $one .= new: 7;
is-deeply $one.List, (7,), 'a single argument is untouched';

# --- `has` attribute default ---
class WithSeq {
    has Array $.seq .= new: 5, 10 ... 25;
    has Array $.plain .= new: 1, 2;
}
is-deeply WithSeq.new.seq.List, (5, 10, 15, 20, 25), 'has $.x .= new: … in a class body';
is-deeply WithSeq.new.plain.List, (1, 2), 'and an ordinary list beside it';

# --- The Z/X meta-ops obey the same precedence ---
my Array $zipped .= new: 1, 2 Z 10, 20;
is-deeply $zipped.List.map(*.List).List, ((1, 10), (2, 20)),
    'Z is looser than the argument comma too';

# --- Named arguments and adverbs still parse ---
class Named {
    has $.x;
    has $.y;
}
my Named $n .= new: x => 1, y => 2;
is $n.x ~ $n.y, '12', 'named arguments in a colon-arg list';

my Array $shaped .= new: :shape(3);
is $shaped.elems, 3, 'a colonpair argument in a colon-arg list';

# --- The postfix method-call form, which already had the lift, still agrees ---
is-deeply (Array.new: 10, 20 ... 50).List, (10, 20, 30, 40, 50),
    'Type.new: … agrees with .= new: …';
