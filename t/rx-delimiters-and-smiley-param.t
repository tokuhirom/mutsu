use v6;
use Test;

# --- rx// with arbitrary (non-word) delimiters -------------------------------
# `rx` accepts any non-word character as its delimiter, matching m// and s///.
# Regression pin for dist Net::IP::Parse (`rx|^(\d+)...$|`).

ok "1.2.3.4" ~~ rx|^(\d+).(\d+).(\d+).(\d+)$|, 'rx| | pipe delimiter matches';
ok "axc" ~~ rx!a.c!, 'rx! ! bang delimiter matches';
ok "axc" ~~ rx,a.c,, 'rx, , comma delimiter matches';
nok "xyz" ~~ rx|^\d+$|, 'rx| | pipe delimiter negative';

# a pipe-delimited rx literal bound to a variable then smartmatched
my $m = rx|^(\d+)$|;
ok "42" ~~ $m, 'pipe-delimited rx bound to a variable matches';

# bracket delimiters still nest, slash still works
ok "ab" ~~ rx/a/, 'rx/ / slash delimiter still works';
ok "ab" ~~ rx{a}, 'rx{ } bracket delimiter still works';

# --- definedness smiley before parametrization ------------------------------
# `Array:D[Int]` (smiley then type params) is accepted in a signature, the same
# as `Array[Int]:D`. Regression pin for Net::IP::Parse BUILD signature.

sub f1(Array:D[Int] $x) { $x.elems }
is f1([1, 2, 3]), 3, 'Array:D[Int] param (smiley before parametrization)';

sub f2(Array[Int]:D $x) { $x.elems }
is f2([1, 2, 3]), 3, 'Array[Int]:D param (smiley after parametrization) still works';

sub f3(Hash:D[Int] $x) { $x.elems }
is f3({ a => 1, b => 2 }), 2, 'Hash:D[Int] param';

# combined with a where-clause junction, as in the dist
sub f4(Array:D[Int] :$octets where $octets.elems == 4 | 16) { $octets.elems }
is f4(octets => [1, 2, 3, 4]), 4, 'Array:D[Int] named param with where-junction';

done-testing;
