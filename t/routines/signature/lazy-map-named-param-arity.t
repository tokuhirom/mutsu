use Test;

# From Math::Sequences (A000031: `𝕀.map: &necklaces` with
# `sub necklaces ($n, :ary($k) = 2)`): a lazy `map` over an infinite source
# feeds one element per positional parameter; named parameters take none.

plan 5;

sub with-named ($n, :$k = 2) { $n + $k }

is (0..*).map(&with-named)[^3].List, (2, 3, 4), 'named sub, optional named param';
is (0..*).map(-> $n, :$k = 2 { $n + $k })[^3].List, (2, 3, 4), 'pointy block with a named param';
is (0..*).map(-> $a, $b, :$s = 10 { $a + $b + $s })[^3].List, (11, 15, 19),
    'two positionals plus a named param chunk the source in pairs';
is (0..*).map(-> $n, *%h { $n })[^3].List, (0, 1, 2), 'named slurpy takes no element';
is (1..*).map(sub ($n, :ary($k) = 2) { $n * $k }).head(3).List, (2, 4, 6), 'aliased named param';
