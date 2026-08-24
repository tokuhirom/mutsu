use Test;

plan 3;

# A small scalar constant count is known while compiling `xx`, so it can run
# its re-evaluated left side directly in the enclosing frame.
constant REPEATS = 10;
my $calls = 0;
my @values = $calls++ xx REPEATS;

is @values.elems, REPEATS, 'constant-count xx produces every repetition';
is $calls, REPEATS, 'constant-count xx re-evaluates its left side';

# A sequence generator is compiled independently from its surrounding unit, so
# its compiler cannot fold the unit's constant repeat count.  The resulting xx
# thunk must remain a bare block and inherit the generator's current topic.
constant COPIES = 2;
sub keep-topic(Str $value) { $value }
my @generated;
@generated.push($_) for "AA", {
    (keep-topic($_) xx COPIES)[0] eq "AA" ?? "AB" !! "AB"
} ... "AB";
is-deeply @generated, ["AA", "AB"],
    'xx thunk inside a deferred sequence inherits the current topic';
