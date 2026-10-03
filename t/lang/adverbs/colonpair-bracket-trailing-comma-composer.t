use Test;

# `:name[ {...}, ]`: a trailing comma keeps a lone hash as one element instead
# of flattening it into its pairs -- in a hash composer and in a call's named
# argument too, not only as a bare colonpair. From PDF::Grammar's
# `my $fdf-small-ast = { :body[ { :objects[...], :trailer{...} }, ] }`.

plan 6;

is-deeply (:b[{a => 1},]).value, [{a => 1},], 'bare colonpair keeps the hash';
is-deeply {:b[{a => 1},]}<b>, [{a => 1},], 'inside a hash composer';
is-deeply {:b[{a => 1}]}<b>, [a => 1], 'without the comma the hash flattens';

sub f(:$b) { $b }
is-deeply f(:b[{a => 1},]), [{a => 1},], 'named argument keeps the hash';
is-deeply f(:b[{a => 1}]), [a => 1], 'named argument without the comma flattens';
is-deeply f(:b[1, 2,]), [1, 2], 'a trailing comma after several items changes nothing';
