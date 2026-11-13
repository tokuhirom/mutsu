use Test;

# A `{*}`-only proto with a bare `(|)` signature lets every argument list
# through to its candidates; a proto with a real signature still gates.

plan 7;

my proto sub ins(|) {*}
my multi sub ins(@a, $pos, \value) { "array:$pos" }
my multi sub ins(IterationBuffer:D $b, $pos, \value) { "buffer:$pos" }
my multi sub ins(Str $s) { "str:$s" }

my @a = 1, 2, 3;
is (^150).map({ ins(@a, $_ % 3, $_) }).unique.List, <array:0 array:1 array:2>,
    'positional array candidate, repeatedly';
is ins(IterationBuffer.new, 1, 2), 'buffer:1', 'the IterationBuffer candidate';
is ins("x"), 'str:x', 'a different arity';
my ($p, $q) = 1, 2;
throws-like { ins($p, $q) }, X::Multi::NoMatch, 'no candidate: the bare capture lets it reach the candidates';

proto sub gated(Int $x, |) {*}
multi sub gated(Int $x) { "one" }
multi sub gated(Int $x, $y) { "two" }
is gated(1), 'one', 'a proto with a real signature dispatches';
is gated(1, 2), 'two', 'with a further argument';
my $no = "no";
dies-ok { gated($no) }, 'the proto signature still gates';
