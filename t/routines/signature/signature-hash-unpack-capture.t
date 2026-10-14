use Test;

# From CSS::Module's t/css1-properties.t: `-> % ( :$module!, |c)` over a Hash.
# A Hash's Capture is purely named, so `|c` is a Capture of the named
# arguments no named sub-parameter took, and no positional is left over.

plan 6;

for ({ :m(1), :z(2), :w(3) },) -> % ( :$m!, |c) {
    is c.raku, '\(:w(3), :z(2))', 'for pointy: |c holds the untaken named args';
    is c.list.elems, 0, 'for pointy: |c has no positional part';
}

sub f(% ( :$m!, |c)) { c.raku }
is f({ :m(1), :z(2) }), '\(:z(2))', 'sub: |c over a Hash is a Capture of the rest';

for ((1, 2, 3),) -> ($a, |c) {
    is c.raku, '\(2, 3)', 'positional unpack: |c holds the remaining elements';
}

for ((1, 2, :x(5)),) -> ($a, |c) {
    is c.raku, '\(2, :x(5))', 'list unpack: |c keeps the named part too';
}

for ([1, 2, 3],) -> [$a, |c] {
    is c.raku, '\(2, 3)', 'array unpack in a for header accepts |c';
}
