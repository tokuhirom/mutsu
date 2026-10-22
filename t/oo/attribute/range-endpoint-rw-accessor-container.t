use Test;

# A Range endpoint read through an `is rw` AT-POS that hands back the element's
# container (`callsame`) is the value the container holds: `1 .. $w[1]` is
# `1..2` and iterates both elements. The endpoint used to stay a container, so
# the range listed as a single `1..2` element and `for` ran once (PDF's
# `for 1 .. $W[$k]` in `unpack-raku`). Expected values are the `raku`
# (v2026.09) output.

plan 4;

class T is Array { method AT-POS($p) is rw { callsame } }
my $w = T.new(1, 2, 1);

my $n = 0;
for 1 .. $w[1] { $n++ }
is $n, 2, 'for over 1 .. $w[1] runs twice';
my $r = 1 .. $w[1];
is $r.raku, '1..2', 'the range prints as 1..2';
is $r.list.raku, '(1, 2)', '... and lists both elements';
is (1 .. $w[2]).elems, 1, 'another element';
