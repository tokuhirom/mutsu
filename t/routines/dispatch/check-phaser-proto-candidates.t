use Test;

# A unit with a CHECK phaser installs its subs before the CHECK runs. An
# explicit `proto` goes with its `multi` candidates, in source order: when only
# the candidates were installed early, the proto was registered after them,
# started an empty candidate list, and every call died with "Routine does not
# have any candidates".

plan 4;

proto sub g($) {*}
multi sub g(Int $x) { "int" }
multi sub g(Str $x) { "str" }

proto sub p(|) {*}
multi sub p() { "none" }

my $seen;
CHECK { $seen = &g.candidates.elems }

is g(1), 'int', 'a proto call after a CHECK reaches its candidate';
is g('x'), 'str', 'and its other candidate';
is p(), 'none', 'a second proto keeps its candidate too';
is $seen, 2, 'the CHECK sees every candidate';
